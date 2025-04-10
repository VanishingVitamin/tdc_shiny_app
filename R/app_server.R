#' Defines the Server function for the Vanishing Vitamin Shiny app
#'
#' This is an internal function that is used within the exported launch_app()
#' function.
#'
#' @value a function object containing app server logic

app_server <- function(){
  function(input, output, session) {

    shiny::observeEvent(input$sidebarId,
                        {

                          if(input$sidebarId){
                            shinyjs::removeCssClass(id = "header_toggle",
                                                    class = "far fa-square-caret-right")
                            shinyjs::addCssClass(id = "header_toggle",
                                                 class = "far fa-square-caret-left")
                          } else{
                            shinyjs::removeCssClass(id = "header_toggle",
                                                    class = "far fa-square-caret-left")
                            shinyjs::addCssClass(id = "header_toggle",
                                                 class = "far fa-square-caret-right")
                          }

                        })

    filtered_data <- shiny::reactiveValues(
      tdc_data = tdc_data,
      citations = citations
    )

    shiny::observe({

      if(is.null(input$tdc_table_filter_location)){
        selected_location <- ""
      } else{
        selected_location <- input$tdc_table_filter_location
      }

      if(is.null(input$tdc_table_filter_species)){
        selected_species <- ""
      } else{
        selected_species <- input$tdc_table_filter_species
      }

      if(is.null(input$tdc_table_filter_run)){
        selected_run <- ""
      } else{
        selected_run <- input$tdc_table_filter_run
      }

      if(is.null(input$tdc_table_filter_tissue)){
        selected_tissue <- ""
      } else{
        selected_tissue <- input$tdc_table_filter_tissue
      }

      filtered_data$tdc_data <-
        tdc_data |>
        dplyr::filter((all(selected_location == "") | Location_label %in% selected_location),
                      (all(selected_species == "") | Species_label %in% selected_species),
                      (all(selected_run == "") | Run_label %in% selected_run),
                      (all(selected_tissue == "") | Tissue_label %in% selected_tissue))

      filtered_data$citations <-
        citations |>
        dplyr::filter(DOI %in% filtered_data$tdc_data$DOI)

    })

    # Create a table summarizes data sets by their associated reference (assuming
    # the reference exists)
    output$tdc_data_table <-
      reactable::renderReactable({

        reactable::reactable(data.frame("x" = filtered_data$citations$formatted_metadata),
                             columns = list(
                               x = reactable::colDef(html = TRUE,
                                                     name = "")
                             ),
                             sortable = FALSE,
                             # selection = "single",
                             showSortable = FALSE,
                             defaultPageSize = 30,
                             details = function(index){

                               htmltools::div(#style = "padding: 1rem",
                                 reactable::reactable(tdc_data |>
                                                        dplyr::filter(DOI == filtered_data$citations[index,]$DOI) |>
                                                        dplyr::select(-c(dplyr::ends_with("label"),
                                                                         Title, DOI,
                                                                         location_type)),
                                                      outlined = TRUE)
                               )

                             })

      })

    # Re-render the table depending on how the user zooms into the map
    shiny::observe({

      shiny::req(input$tdc_data_map_bounds)

      map_zoom_dois <-
        filtered_data$tdc_data |>
        dplyr::filter(
          dplyr::between(Latitude_DD, input$tdc_data_map_bounds$south, input$tdc_data_map_bounds$north),
          dplyr::between(Longitude_DD, input$tdc_data_map_bounds$west, input$tdc_data_map_bounds$east)
        ) |>
        dplyr::pull(DOI)

      filtered_citations_zoomed <-
        filtered_data$citations |>
        dplyr::filter(DOI %in% map_zoom_dois)

      # if the user has selected a marker, highlight in the table.
      if(!is.null(input$tdc_data_map_marker_click$id)){

        selected_marker_doi <-
          filtered_data$tdc_data  |>
          shiny::isolate() |>
          dplyr::distinct(DOI, Location, Latitude_DD, Longitude_DD, marker_label)  |>
          dplyr::filter(!is.na(Latitude_DD)) |>
          dplyr::slice(input$tdc_data_map_marker_click$id) |>
          dplyr::pull(DOI)

        selected_citation_index <- as.integer(which(filtered_citations_zoomed$DOI == selected_marker_doi))

      } else{
        selected_citation_index <- -1L
      }

      if(length(selected_citation_index) == 0) selected_citation_index <- -1L

      # reactable::updateReactable(
      #   outputId = "tdc_data_table",
      #   data = data.frame("x" = filtered_data$citations |>
      #                       filter(DOI %in% map_zoom_dois) |>
      #                       pull(formatted_metadata)),
      #   session = session
      # )

      output$tdc_data_table <-
        reactable::renderReactable({

          reactable::reactable(data.frame("x" =  filtered_citations_zoomed$formatted_metadata),
                               columns = list(
                                 x = reactable::colDef(html = TRUE,
                                                       name = "")
                               ),
                               sortable = FALSE,
                               # selection = "single",
                               showSortable = FALSE,
                               defaultPageSize = 30,
                               rowStyle = function(index){

                                 if(index == selected_citation_index){
                                   return(list(background = "#D3D3D3"))
                                 }

                               },
                               details = function(index){

                                 htmltools::div(#style = "padding: 1rem",
                                   reactable::reactable(tdc_data |>
                                                          dplyr::filter(DOI == filtered_citations_zoomed[index,]$DOI) |>
                                                          dplyr::select(-c(dplyr::ends_with("label"),
                                                                           Title, DOI,
                                                                           location_type)),
                                                        outlined = TRUE)
                                 )

                               })

        })

    })


    # Create an interactive map with markers indicating where data were collected.
    # Clicking on a marker shows information about that data collection.
    output$tdc_data_map <- leaflet::renderLeaflet({

      lat_bounds <- range(filtered_data$tdc_data$Latitude_DD, na.rm = TRUE)
      long_bounds <- range(filtered_data$tdc_data$Longitude_DD, na.rm = TRUE)

      plt_data <-
        filtered_data$tdc_data |>
        dplyr::distinct(DOI, Latitude_DD, Longitude_DD, .keep_all = TRUE) |>
        dplyr::filter(!is.na(Latitude_DD))

      tdc_map <-
        leaflet::leaflet(data = plt_data) |>
        leaflet::addTiles() |>
        # addAwesomeMarkers(
        leaflet::addMarkers(
          layerId = 1:nrow(plt_data),
          lng = ~Longitude_DD,
          lat = ~Latitude_DD,
          # lng = ~jitter(Longitude_DD, factor = 0.001),
          # lat = ~jitter(Latitude_DD, factor = 0.001),
          # icon = awesomeIcons(icon = "map-pin", markerColor = "blue"),
          # label = ~purrr::map(marker_label, HTML),
          popup = ~purrr::map(marker_label, HTML)
          ,clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = TRUE,
                                                          spiderfyOnMaxZoom = TRUE,
                                                          maxClusterRadius = 0)
        )

      return(tdc_map)

    })

    # Update map markers based on selected filters
    shiny::observe({

      plt_data <-
        filtered_data$tdc_data |>
        dplyr::distinct(DOI, Latitude_DD, Longitude_DD, .keep_all = TRUE) |>
        dplyr::filter(!is.na(Latitude_DD))

      # selected <- getReactableState("tdc_data_table", "selected")
      #
      # if(is.null(selected)){
      #   icons <-
      #     awesomeIcons(icon = "map-pin",
      #                  markerColor = "blue")
      # } else{
      #   icons <-
      #     awesomeIcons(icon = "map-pin",
      #                  markerColor = c("blue", "red")[(plt_data$DOI == {filtered_data$citations |> slice(selected) |> pull(DOI)}) + 1])
      # }

      leaflet::leafletProxy("tdc_data_map",
                            session = session,
                            data = plt_data) |>
        leaflet::clearMarkers() |>
        # addAwesomeMarkers(
        leaflet::addMarkers(
          layerId = 1:nrow(plt_data),
          lng = ~Longitude_DD,
          lat = ~Latitude_DD,
          # lng = ~jitter(Longitude_DD, factor = 0.001),
          # lat = ~jitter(Latitude_DD, factor = 0.001),
          # icon = icons,
          # label = ~purrr::map(marker_label, HTML),
          popup = ~purrr::map(marker_label, HTML)
          ,clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = TRUE,
                                                          spiderfyOnMaxZoom = TRUE,
                                                          maxClusterRadius = 0)
        )

    })

    ### Visualize tab code

    output$ec50_curve <- plotly::renderPlotly({

      plt <-
        tdc_data |>
        dplyr::filter(Thiamine_conc < 30) |>
        dplyr::mutate(plot_label = paste0("Thiamin Conc: ", round(Thiamine_conc,2),"\n",
                                          "% Survived: ", round(Percent_survive,2))) |>
        ggplot2::ggplot(ggplot2::aes(x = Thiamine_conc,
                                     y = Percent_survive)) +
        ggplot2::geom_point() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Thiamin Concentration (nmol/g)",
                      y = "% Survived")

      plotly::ggplotly(plt)

    })

  }
}
