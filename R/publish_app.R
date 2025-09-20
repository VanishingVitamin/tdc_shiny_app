#' Publish VanishingVitamin app to Posit Connect
#'
#' @export
publish_app <- function() {
  r_scripts <- list.files("R/", full.names = TRUE)

  www_content <- list.files("www/", full.names = TRUE, recursive = TRUE)

  data_content <- list.files("data/", full.names = TRUE)

  rsconnect::deployApp(
    appDir = getwd(),
    appFiles = c(
      "app.R",
      "DESCRIPTION",
      "NAMESPACE",
      "USGS_ID_black.png",
      r_scripts,
      www_content,
      data_content
    )
  )
}
