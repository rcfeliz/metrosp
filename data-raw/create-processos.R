# processos_bruto ---------------------------------------------------------

load("data/fases.rda")
load("data/contratos.rda")

processos_bruto <- fases |>
  tidyr::nest(.by = id_processo, .key = "fases") |>
  dplyr::left_join(contratos) |>
  dplyr::select(id_processo, estacao, fases)


# extrair a data de início ------------------------------------------------

path_avl <- "data-raw/pdf/avl"

processos_bruto |>
  tidyr::unnest(fases) |>
  dplyr::filter(id_fase == 1) |>
  tidyr::unnest(arquivos) |>
  dplyr::mutate(
    nome_arq = nome_arq |>
      stringr::str_remove_all("_[0-9]+") |>
      stringr::str_to_lower() |>
      abjutils::rm_accent(),
    nome_arq = dplyr::case_when(
      stringr::str_detect(nome_arq, "avl") ~ "avl",
      stringr::str_detect(nome_arq, "aviso de licitacao") ~ "avl",
      TRUE ~ nome_arq
    )
  ) |>
  dplyr::filter(nome_arq == "avl") |>
  dplyr::select(id_processo, link_arq) |>
  dplyr::group_split(id_processo) |>
  purrr::map(extract_pdf, path = path_avl)

aux_dt_inicio <- tibble::tibble(
  id_processo = fs::dir_ls(path_avl) |>
    stringr::str_extract("[0-9]+"),
  dt_inicio = fs::dir_ls(path_avl) |>
    purrr::map_dfr(pdftools::pdf_text) |>
    stringr::str_extract("\\d{2}/\\d{2}/\\d{4}") |>
    lubridate::dmy()
)

# extrair se deu certo ou não ---------------------------------------------
aux_status <- processos_bruto |>
  dplyr::left_join(aux_dt_inicio) |>
  dplyr::group_by(estacao) |>
  dplyr::arrange(estacao, dt_inicio) |>
  dplyr::mutate(
    status = dplyr::case_when(
      dplyr::lead(estacao) == estacao ~ "fracasso"
    )
  ) |>
  dplyr::ungroup() |>
  tidyr::unnest(fases) |>
  dplyr::group_by(id_processo) |>
  dplyr::mutate(
    status = dplyr::case_when(
      any(stringr::str_detect(fase, "HOMOLOG")) ~ "sucesso",
      is.na(status) ~ "em andamento",
      TRUE ~ status
    )
  ) |>
  dplyr::ungroup() |>
  tidyr::nest(.by = c(id_processo, estacao, dt_inicio, status), .key = "fases") |>
  dplyr::mutate(status = ifelse(is.na(estacao), NA_character_, status)) |>
  dplyr::arrange(id_processo)

# extrair valores (R$) ---------------------------------------------------------

path_valor <- "data-raw/pdf/valor"

# download
aux_status |>
  dplyr::filter(status == "sucesso") |>
  tidyr::unnest(fases) |>
  dplyr::filter(stringr::str_detect(fase, "HOMOLOG")) |>
  dplyr::group_by(id_processo) |>
  dplyr::filter(id_fase == max(id_fase, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  tidyr::unnest(arquivos) |>
  dplyr::select(id_processo, link_arq) |>
  dplyr::group_split(id_processo) |>
  purrr::map(extract_pdf, path = path_valor)

# parse
rgx_reais <- "\\b(?:\\d{1,3}(?:\\.\\d{3})*,\\d{2}|\\d{1,3},\\d{2})\\b"

aux_valores <- tibble::tibble(
  id_processo = fs::dir_ls(path_valor) |>
    stringr::str_extract("[0-9]+"),
  valor_total = fs::dir_ls(path_valor) |>
    purrr::map(pdftools::pdf_text) |>
    purrr::map(~stringr::str_extract_all(.x, stringr::regex("(?<=Para:[\n ]).+", TRUE))) |>
    purrr::map(~unlist(.x)) |>
    purrr::map(~purrr::keep(.x, stringr::str_detect(.x, "\\d{1,3}\\.\\d{3},\\d{2}"))) |>
    purrr::map(~purrr::keep(.x, stringr::str_detect(.x, "total"))) |>
    purrr::map(~stringr::str_extract(.x, rgx_reais)) |>
    unlist(),
  valor_mensal = fs::dir_ls(path_valor) |>
    purrr::map(pdftools::pdf_text) |>
    purrr::map(~stringr::str_extract_all(.x, stringr::regex("(?<=Para:[\n ]).+", TRUE))) |>
    purrr::map(~unlist(.x)) |>
    purrr::map(~purrr::keep(.x, stringr::str_detect(.x, "\\d{1,3}\\.\\d{3},\\d{2}"))) |>
    purrr::map(~purrr::keep(.x, stringr::str_detect(.x, "mensal"))) |>
    purrr::map(~stringr::str_extract(.x, rgx_reais)) |>
    unlist()
)


# extrair tempo -----------------------------------------------------------

path_tempo <- "data-raw/pdf/tempo"

aux_status |>
  dplyr::filter(status == "sucesso") |>
  tidyr::unnest(fases) |>
  dplyr::filter(stringr::str_detect(fase, "HOMOLOG")) |>
  dplyr::group_by(id_processo) |>
  dplyr::filter(id_fase == min(id_fase, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  tidyr::unnest(arquivos) |>
  dplyr::select(id_processo, link_arq) |>
  dplyr::group_split(id_processo) |>
  purrr::map(extract_pdf, path = path_tempo)

aux_tempo <- tibble::tibble(
  id_processo = fs::dir_ls(path_valor) |>
    stringr::str_extract("[0-9]+"),
  tempo_contrato = fs::dir_ls(path_tempo) |>
    purrr::map(pdftools::pdf_text) |>
    purrr::map(~purrr::keep(.x, stringr::str_detect(.x, "VIGÊNCIA/PRAZOS"))) |>
    purrr::discard(~ length(.x) == 0) |>
    paste0() |>
    stringr::str_extract("(?<=O prazo de vigência do presente Contrato de Concessão é de )[0-9]+") |>
    paste0(" anos")
)
# processos ---------------------------------------------------------------

processos <- list(
  processos_bruto,
  aux_dt_inicio,
  dplyr::select(aux_status, id_processo, status),
  aux_valores,
  aux_tempo
) |>
  purrr::reduce(dplyr::left_join, by = "id_processo") |>
  dplyr::select(id_processo, estacao, dt_inicio, status, valor_total, valor_mensal, tempo_contrato, fases) |>
  dplyr::filter(!is.na(estacao))

processos |>
  dplyr::select(dplyr::contains("valor")) |>
  dplyr::filter(!is.na(valor_total)) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::contains("valor"),
      .fns = ~readr::parse_number(.x, locale = readr::locale(grouping_mark = ".", decimal_mark = ","))
    )
  )

usethis::use_data(processos, overwrite = TRUE)
