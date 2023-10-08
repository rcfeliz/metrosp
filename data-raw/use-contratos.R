# download ----------------------------------------------------------------

path <- "data-raw/html/contratos"

file <- contrato_download(path = path, ds_objeto = "naming rights")

# parse -------------------------------------------------------------------

contratos <- contrato_parse(file) |>
  dplyr::transmute(
    id_processo = stringr::str_squish(id_processo),
    nome_contrato = stringr::str_replace(nome_contrato, "\u0093", "\""),
    nome_contrato = stringr::str_replace(nome_contrato, "\u0094", "\""),
    estacao = nome_contrato |>
      stringr::str_to_lower() |>
      stringr::str_extract("(?<=estação ).+(?= da)") |>
      stringr::str_to_sentence(),
    link_processo
  )

fs::dir_delete(path)

usethis::use_data(contratos, overwrite = TRUE)
