#'Defines the Server function for the Vanishing Vitamin Shiny app
#'
#'This is an internal function that is used within the exported launch_app()
#'function.
#'
#'@param tdc_data data set containing Thiamine by Survivability data. Should be
#'  the data set exported by the vanishingVitamin package,
#'  vanishingVitamin::tdc_data
#'@param citations data set containing citations for data in the tdc_data data
#'  set. Should be the data set exported by the vanishingVitamin package,
#'  vanishingVitamin::citations
#'@param dose_response_params data set containing estimated parameters for the dose-response model.
#'  Should be the data set exported by the vanishingVitamin
#'  package, vanishingVitamin::dose_response_params
#'
#' @return a function object containing app server logic
#' @keywords internal
#' @noRd

app_server <- function(tdc_data, citations, dose_response_params, translator) {
  function(input, output, session) {
    shiny::addResourcePath(
      "www",
      system.file("www", package = "vanishingVitamin")
    )

    # Toggle sidebar icon to + or - based on whether it's collapsed
    shiny::observeEvent(input$filter_sidebar, {
      if (input$filter_sidebar) {
        shinyjs::removeCssClass(
          id = "header_toggle",
          class = "far fa-square-plus"
        )
        shinyjs::addCssClass(
          id = "header_toggle",
          class = "far fa-square-minus"
        )
      } else {
        shinyjs::removeCssClass(
          id = "header_toggle",
          class = "far fa-square-minus"
        )
        shinyjs::addCssClass(id = "header_toggle", class = "far fa-square-plus")
      }
    })

    tab_automatically_opened <- shiny::reactiveVal(value = FALSE)

    # The first time the user clicks on the Data or Visualize tab, if the filter
    # sidebar isn't open, then open it.
    shiny::observe({
      if (
        !shiny::isolate(input$filter_sidebar) &
        input$navmenu %in% c("data", "visualize") &
        !tab_automatically_opened()
      ) {
        bs4Dash::updateSidebar(id = "filter_sidebar")
        tab_automatically_opened(TRUE)
      }
    })

    filtered_data <- shiny::reactiveValues(
      tdc_data = tdc_data,
      citations = citations
    )

    shiny::observe({
      if (is.null(input$tdc_table_filter_location)) {
        selected_location <- ""
      } else {
        selected_location <- input$tdc_table_filter_location
      }

      if (is.null(input$tdc_table_filter_species)) {
        selected_species <- ""
      } else {
        selected_species <- input$tdc_table_filter_species
      }

      if (is.null(input$tdc_table_filter_run)) {
        selected_run <- ""
      } else {
        selected_run <- input$tdc_table_filter_run
      }

      if (is.null(input$tdc_table_filter_tissue)) {
        selected_tissue <- ""
      } else {
        selected_tissue <- input$tdc_table_filter_tissue
      }

      # if no value of a filter is selected, treat as if all values of that
      # filter have been selected
      filtered_data$tdc_data <-
        tdc_data |>
        dplyr::filter(
          (all(selected_location == "") |
             Location_label %in% selected_location),
          (all(selected_species == "") | Species_label %in% selected_species),
          (all(selected_run == "") | Run_label %in% selected_run),
          (all(selected_tissue == "") | Tissue_label %in% selected_tissue),
          (published | input$tdc_table_filter_unpublished)
        ) |>
        dplyr::arrange(unique_id)

      filtered_data$citations <-
        citations |>
        dplyr::filter(unique_id %in% filtered_data$tdc_data$unique_id) |>
        dplyr::arrange(unique_id)
    })

    translation_server(input, output, session, translator)

    help_message_server(input = input,
                        output = output,
                        session = session)

    data_tab_server(input = input,
                    output = output,
                    session = session,
                    filtered_data = filtered_data)

    visualize_tab_server(input = input,
                         output = output,
                         session = session,
                         filtered_data = filtered_data,
                         dose_response_params = dose_response_params)
  }
}

#' Non-exported helper function for computing dose response
#'
#' @param Thiamine_conc thiamine concentration value (nmol/g)
#' @param ec50_mu EC50 mean value
#' @param slope_p Slope parameter in dose response model
#' @param upper_p Upper limit parameter in dose response model
#' @param lower_p Lower limit parameter in dose response model
#'
#' @keywords internal
#' @noRd
dose_response <-
  function(Thiamine_conc, ec50_mu, slope_p, upper_p, lower_p = 0) {
    upper_p + (lower_p - upper_p) / (1 + (Thiamine_conc / ec50_mu)**slope_p)
  }
