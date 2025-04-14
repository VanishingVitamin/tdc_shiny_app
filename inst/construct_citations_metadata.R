tdc_data <- readr::read_csv("inst/misc_data/tdc_data.csv")

citations <-
  tdc_data |>
  dplyr::distinct(DOI = tolower(DOI)) |>
  dplyr::mutate(DOI_num = dplyr::case_when(stringr::str_detect(DOI, "org") ~ stringr::str_remove_all(DOI, "^.*org/"),
                             startsWith(DOI, "DOI: ") ~ stringr::str_remove(DOI, "DOI: "),
                             .default = DOI) |>
           URLdecode())   |>
  dplyr::mutate(metadata = purrr::map(DOI_num, ~ {

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

    if(length(article_meta$authors) > 2){
      author_string <- paste0(article_meta$authors[[1]]$family, " et al")
    } else if(length(article_meta$authors) == 2){
      author_string <- paste0(article_meta$authors[[1]]$family," and ",article_meta$authors[[2]]$family)
    } else{
      author_string <- article_meta$authors[[1]]$family
    }

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

citations <- citations |>
  dplyr::mutate(formatted_metadata = purrr::map2_chr(metadata,DOI,
                                                     ~ ifelse(stringr::str_detect(.y, "doi"),
                                                              format_metadata(.x),
                                                              .y)),
                data_collection_region = dplyr::case_when(
                  DOI %in% c(
                    "http://dx.doi.org/10.23849/npafcb6/21.31",
                    "https://doi.org/10.1002/aah.10024",
                    "https://github.com/milesedaniels/thiamine-dependent-fry-mortality"
                  ) ~ "PACIFIC",
                  DOI %in% c(
                    "https://doi.org/10.1016/j.jglr.2019.05.010",
                    "https://doi.org/10.1016/s0380-1330(08)71603-4",
                    "https://doi.org/10.1111/j.1749-7345.2000.tb00348.x",
                    "https://doi.org/10.1111/mec.15334",
                    "https://doi.org/10.1577/1548-8659(1996)125<0167:notdcr>2.3.co;2",
                    "https://doi.org/10.1577/h03-072.1",
                    "https://doi.org/10.3394/0380-1330(2006)32[293:esaooe]2.0.co;2",
                    "https://doi.org/10.3394/0380-1330(2007)33[93:etsolo]2.0.co;2",
                    "https://doi.org/10.1577/1548-8659(2000)129<0607:eotoro>2.0.co;2"
                  ) ~ "GREAT LAKES",
                  DOI %in% c(
                    "https://doi.org/10.1080/10236244.2021.1941942"
                  ) ~ "BALTIC",
                  .default = "UNCATEGORIZED"
                ))


saveRDS(citations, file = "inst/misc_data/citations.rds")
