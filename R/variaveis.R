extract_pdf <- function(i, path) {
  id <- i$id_processo
  url <- i$link_arq
  fs::dir_create(path)
  file <- glue::glue("{path}/{id}.pdf")

  httr::GET(url, httr::write_disk(file, TRUE))
}

#' Extrai o valor total ou mensal de um contrato
#'
#' Busca o valor da cláusula "Para:" de um aditivo, quando o contrato foi
#' reajustado, ou o valor original da cláusula 7.1, quando não há aditivo.
#'
#' @param texto vetor de texto dos PDFs, um elemento por contrato
#' @param termo termo de busca, "total" ou "mensal"
#' @return vetor de valores no formato "0.000,00", com NA quando não encontrado
extrair_valor <- function(texto, termo) {
  rgx_clausula <- c(
    total  = "valor total deste Contrato de Concessão é de R\\$ [\\d.,]+",
    mensal = "valor da remuneração mensal é de R\\$ [\\d.,]+"
  )

  texto |>
    purrr::map_chr(function(txt) {
      valor_aditivo <- txt |>
        stringr::str_extract_all(stringr::regex("(?<=Para:[\n ]).+", TRUE)) |>
        unlist() |>
        purrr::keep(~ stringr::str_detect(.x, "\\d{1,3}\\.\\d{3},\\d{2}")) |>
        purrr::keep(~ stringr::str_detect(.x, termo)) |>
        purrr::pluck(1, .default = NA_character_)

      valor_original <- txt |>
        stringr::str_extract(rgx_clausula[[termo]])

      dplyr::coalesce(valor_aditivo, valor_original)
    }) |>
    stringr::str_extract("\\b(?:\\d{1,3}(?:\\.\\d{3})*,\\d{2}|\\d{1,3},\\d{2})\\b")
}
