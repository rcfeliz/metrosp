extract_pdf <- function(i, path) {
  id <- i$id_processo
  url <- i$link_arq
  fs::dir_create(path)
  file <- glue::glue("{path}/{id}.pdf")

  httr::GET(url, httr::write_disk(file, TRUE))
}
