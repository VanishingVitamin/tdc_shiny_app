tdc_data <- readr::read_csv("inst/misc_data/tdc_data.csv")

citations <-
  tdc_data |>
  dplyr::distinct(unique_id) |>
  dplyr::mutate(
    metadata = purrr::map(
      unique_id,
      ~ {
        ret <-
          paste0("http://dx.doi.org/", URLdecode(.x)) |>
          urltools::url_encode() |>
          httr2::request() |>
          # httr2::req_url_path_append(URLencode(.x)) |>
          httr2::req_headers(accept = "application/json") |>
          httr2::req_error(is_error = \(resp) FALSE) |>
          httr2::req_perform()

        if (httr2::resp_is_error(ret)) {
          return(NA)
        } else {
          ret <- httr2::resp_body_json(ret)
          return(list(
            title = ret$title,
            DOI = ret$DOI,
            date_time = ret$created$`date-time`,
            publisher = ret$publisher,
            prefix = ret$prefix,
            volume = ret$volume,
            page = ret$page,
            authors = ret$author,
            language = ret$language
          ))
        }
      }
    )
  )

format_metadata <- function(article_meta) {
  if (!is.null(article_meta$authors)) {
    if (length(article_meta$authors) > 2) {
      author_string <- paste0(article_meta$authors[[1]]$family, " et al")
    } else if (length(article_meta$authors) == 2) {
      author_string <- paste0(
        article_meta$authors[[1]]$family,
        " and ",
        article_meta$authors[[2]]$family
      )
    } else {
      author_string <- article_meta$authors[[1]]$family
    }

    author_string <- paste0(author_string)
  } else {
    author_string <- ""
  }

  year_string <- paste0("(", lubridate::year(article_meta$date_time), ")")

  title_string <- paste0("<strong>", article_meta$title, "</strong>") |>
    stringr::str_replace(
      "Oncorhynchus tshawytscha",
      "<em>Oncorhynchus tshawytscha</em>"
    )

  link_string <- paste0("https://doi.org/", article_meta$DOI)
  link_string <- paste0(
    "<a href='",
    link_string,
    "' target='_blank'>",
    link_string,
    "</a>"
  )

  paste0(
    author_string,
    ". ",
    year_string,
    ". ",
    title_string,
    ". ",
    link_string,
    "."
  )
}
# undebug(format_metadata)
citations <- citations |>
  dplyr::mutate(
    formatted_metadata = purrr::map2_chr(
      metadata,
      unique_id,
      ~ ifelse(length(.x) == 1, .y, format_metadata(.x))
    )
  )

saveRDS(citations, file = "inst/misc_data/citations.rds")
usethis::use_data(citations, overwrite = TRUE)
