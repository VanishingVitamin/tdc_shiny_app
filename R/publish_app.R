#' Publish VanishingVitamin app to Posit Connect
#'
#' @export
publish_app <- function() {
  r_scripts <- list.files("R/", full.names = TRUE)

  www_content <- list.files("www/", full.names = TRUE, recursive = TRUE)

  data_content <- list.files("data/", full.names = TRUE)

  rsconnect::deployApp(
    appId = "38a66d86-5329-46bd-96e8-644e1eac078b",
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
