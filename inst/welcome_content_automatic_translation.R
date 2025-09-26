library(polyglotr)

google_translate_utf8 <-
  function(
    text,
    target_language = "en",
    source_language = "auto",
    encoding = "UTF-8"
  ) {
    . <- NULL
    if (!google_is_valid_language_code(target_language)) {
      stop("Invalid target language code.")
    }
    if (!google_is_valid_language_code(source_language)) {
      stop("Invalid source language code.")
    }
    is_vector <- is.vector(text) && length(text) > 1
    if (is_vector) {
      translations <- purrr::map_chr(text, function(t) {
        replaced <- polyglotr:::replace_urls_with_placeholders(t)
        encoded <- urltools::url_encode(replaced$text)
        link <- paste0(
          "https://translate.google.com/m?tl=",
          target_language,
          "&sl=",
          source_language,
          "&q=",
          encoded
        )

        translated <-
          rvest::read_html(link, encoding = encoding) %>%
          rvest::html_nodes("div.result-container") %>%
          rvest::html_text() %>%
          urltools::url_decode() %>%
          gsub("\n", "", .)
        polyglotr:::restore_urls_from_placeholders(translated, replaced$urls)
      })
      translations
    } else {
      replaced <- polyglotr:::replace_urls_with_placeholders(text)
      encoded <- urltools::url_encode(replaced$text)
      link <- paste0(
        "https://translate.google.com/m?tl=",
        target_language,
        "&sl=",
        source_language,
        "&q=",
        encoded
      )
      translated <- rvest::read_html(link, encoding = encoding) %>%
        rvest::html_nodes("div.result-container") %>%
        rvest::html_text() %>%
        urltools::url_decode() %>%
        gsub("\n", "", .)
      polyglotr:::restore_urls_from_placeholders(translated, replaced$urls)
    }
  }

purrr::walk(c("es", "sv", "fi"), function(lang) {
  print(lang)

  purrr::walk(
    list.files(
      "www/welcome_page_content_en/",
      pattern = "*.qmd$",
      recursive = TRUE,
      full.names = FALSE
    ),
    ~ {
      section_name <- stringr::str_remove(.x, "/.*$")

      print(section_name)

      dir_name <- paste0("www/welcome_page_content_", lang, "/", section_name)

      dir.create(dir_name, showWarnings = FALSE, recursive = TRUE)

      if (section_name %in% c("references", "resources")) {
        file.copy(
          from = paste0("www/welcome_page_content_en/", .x),
          to = paste0(dir_name, '/', section_name, ".qmd"),
          overwrite = TRUE
        )

        return(NULL)
      }

      paste0("www/welcome_page_content_en/", .x) |>
        readLines(encoding = "UTF-8") |>
        paste0(collapse = "\n") |>
        # split on empty lines (separating paragraphs)
        stringr::str_split_1(pattern = "\n\n") |>
        purrr::map_chr(function(paragraph) {
          # pass YAML header without translation
          if (stringr::str_detect(paragraph, "^---")) {
            return(paragraph)
          } else if (stringr::str_detect(paragraph, "^!")) {
            # for figures, only translate the figure caption
            stringr::str_replace(
              paragraph,
              # extract fig caption between [] characters
              pattern = stringr::str_extract(paragraph, "(?<=\\[)(.*?)(?=\\])"),
              replacement = google_translate_utf8(
                stringr::str_extract(paragraph, "(?<=\\[)(.*?)(?=\\])"),
                target_language = lang,
                source_language = "en",
                encoding = "UTF-8"
              )
            )
          } else {
            google_translate_utf8(
              paragraph,
              target_language = lang,
              source_language = "en",
              encoding = "UTF-8"
            )
          }
        }) |>
        paste0(collapse = "\n\n") |>
        writeLines(con = paste0(dir_name, '/', section_name, ".qmd"))
    }
  )
})

purrr::walk(
  list.files("www/", pattern = "qmd", recursive = TRUE, full.names = TRUE),
  quarto::quarto_render
)
