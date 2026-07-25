# preparacao ----

load("data/processos.rda")

# tabela ----

tabela_processos <- processos |>
  dplyr::mutate(
    status = forcats::fct_relevel(
      status,
      "sucesso",
      "em andamento",
      "fracasso"
    ),
    dt_inicio = format(dt_inicio, "%d/%m/%Y"),
    valor_total = stringr::str_c("R$ ", valor_total),
    valor_mensal = stringr::str_c("R$ ", valor_mensal)
  ) |>
  dplyr::arrange(status) |>
  dplyr::select(-fases) |>
  flextable::flextable() |>
  flextable::set_header_labels(
    id_processo = "Processo",
    estacao = "Estação",
    dt_inicio = "Data de Início",
    status = "Status",
    valor_total = "Valor Total",
    valor_mensal = "Valor Mensal",
    tempo_contrato = "Tempo de Contrato"
  ) |>
  flextable::bold(part = "header")

tabela_processos

# exportar ----

png("data-raw/png/tabela_processos.png", width = 1600, height = 900, res = 150)
plot(tabela_processos)
dev.off()
