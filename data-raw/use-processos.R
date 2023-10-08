# download -----------------------------------------------------------------

load("data/contratos.rda")

path = "data-raw/html/processos"

contratos |>
  dplyr::pull(id_processo) |>
  purrr::map(processos_download, path = path)

# parse -------------------------------------------------------------------

processos <- fs::dir_ls("data-raw/html/processos/") |>
  purrr::map_dfr(processos_parse)

fs::dir_delete(path)

usethis::use_data(processos, overwrite = TRUE)
