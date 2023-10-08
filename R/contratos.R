contrato_download <- function(path = "data-raw/html/contratos", cd_processo = NULL, ds_objeto) {

  url <- "https://aplic.metrosp.com.br/as0001/frontendTransparencia/index.php/processo/index"

  if(is.null(cd_processo)) {
    cd_processo = ""
  }
  ds_objeto <- ds_objeto |>
    stringr::str_replace(" ", "+")

  endpoint <- glue::glue("?Processo%5Bcd_processo%5D={cd_processo}&Processo%5Bds_objeto%5D={ds_objeto}&yt0=Buscar")

  fs::dir_create(path)

  file <- glue::glue("{path}/contratos.html")

  r <- httr::GET(paste0(url, endpoint), httr::write_disk(file, overwrite = TRUE))

  file
}

contrato_parse <- function(file = "data-raw/html/contratos/contratos.html") {
  html <- xml2::read_html(file) |>
    xml2::xml_find_first("//tbody") |>
    xml2::xml_find_all("./tr")

  contratos <- tibble::tibble(
    id_processo = html |>
      xml2::xml_find_all("./td[1]") |>
      xml2::xml_text(),
    nome_contrato = html |>
      xml2::xml_find_all("./td[2]") |>
      xml2::xml_text(),
    link_processo = html |>
      xml2::xml_find_all("./td[3]/a") |>
      xml2::xml_attr("href")
  ) |>
    dplyr::mutate(
      link_processo = paste0("https://aplic.metrosp.com.br", link_processo)
    )

  contratos
}

