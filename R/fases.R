fases_download <- function(path = "data-raw/html/fases", id) {
  load("data/contratos.rda")

  url <- contratos |>
    dplyr::filter(id_processo == id) |>
    dplyr::pull(link_processo)

  fs::dir_create(path)

  file <- glue::glue("{path}/{id}.html")

  r <- httr::GET(url, httr::write_disk(file, overwrite = TRUE))

  file
}

fases_parse <- function(file) {

  id_processo <- stringr::str_extract(file, "[0-9]+")

  html <- file |>
    xml2::read_html() |>
    xml2::xml_find_all("//table[@id!='yw0']")

  arquivos <- tibble::tibble(
    id_arq = html |>
      xml2::xml_find_all("./tr[3]//dl") |>
      xml2::xml_text(),
    nome_arq = html |>
      xml2::xml_find_all("./tr[3]//dl/dt") |>
      xml2::xml_text(),
    link_arq = html |>
      xml2::xml_find_all("./tr[3]//dd/a") |>
      xml2::xml_attr("href")
  ) |>
    dplyr::mutate(
      id_arq = dplyr::row_number(),
      nome_arq = stringr::str_squish(nome_arq),
      link_arq = paste0("https://aplic.metrosp.com.br", link_arq)
    )

  fases <- tibble::tibble(
    id_processo = id_processo,
    id_fase = length(html):1L,
    fase = html |>
      xml2::xml_find_all("./tr[1]/td") |>
      xml2::xml_text(),
    descricao = html |>
      xml2::xml_find_all("./tr[2]/td") |>
      xml2::xml_text(),
    id_arq =  html |>
      xml2::xml_find_all("./tr[3]/td") |>
      xml2::xml_text()
  ) |>
    dplyr::mutate(
      id_arq = stringr::str_split(id_arq, "KB")
    ) |>
    tidyr::unnest(id_arq) |>
    dplyr::filter(id_arq != "") |>
    dplyr::mutate(id_arq = dplyr::row_number()) |>
    dplyr::left_join(arquivos) |>
    dplyr::select(!id_arq) |>
    tidyr::nest(.by = c(id_processo, id_fase, fase, descricao), .key = "arquivos")

  fases
}


