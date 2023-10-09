# download -----------------------------------------------------------------

load("data/contratos.rda")

path = "data-raw/html/fases"

contratos |>
  dplyr::pull(id_processo) |>
  purrr::map(fases_download, path = path)

# parse -------------------------------------------------------------------

fases <- fs::dir_ls("data-raw/html/fases/") |>
  purrr::map_dfr(fases_parse)

fs::dir_delete(path)

usethis::use_data(fases, overwrite = TRUE)
