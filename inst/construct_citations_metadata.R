citations <-
  tdc_data |>
  distinct(DOI) |>
  mutate(DOI_num = case_when(stringr::str_detect(DOI, "org") ~ stringr::str_remove_all(DOI, "^.*org/"),
                             startsWith(DOI, "DOI: ") ~ stringr::str_remove(DOI, "DOI: "),
                             .default = DOI) |>
           URLdecode())   |>
  mutate(metadata = purrr::map(DOI_num, ~ {

    ret <-
      httr2::request("http://dx.doi.org") |>
      httr2::req_url_path_append(.x) |>
      httr2::req_headers(accept = "application/json") |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()

    if(httr2::resp_is_error(ret)){
      return(NA)
    }
    else{
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

  }))

format_metadata <- function(article_meta){

  if(!is.null(article_meta$authors)){
    author_string <- article_meta$authors |>
      purrr::map_chr(function(author){
        paste0(author$given," ",author$family)
      }) |>
      paste0(collapse = ", ")

    author_string <- paste0(author_string)
  } else{
    author_string <- ""
  }

  year_string <- paste0("(", lubridate::year(article_meta$date_time),")")

  title_string <- paste0("<strong>",article_meta$title,"</strong>")

  link_string <- paste0("https://doi.org/", article_meta$DOI)
  link_string <- paste0("<a href='",link_string,"' target='_blank'>",link_string,"</a>")

  paste0(author_string,". ",
         year_string,". ",
         title_string,". ",
         link_string,".")

}

citations <- mutate(citations,
                    formatted_metadata = purrr::map2_chr(metadata,DOI,
                                                         ~ ifelse(stringr::str_detect(.y, "doi"),
                                                                  format_metadata(.x),
                                                                  .y)))


saveRDS(citations, file = "vanishing_vitamin/data/citations.rds")
