library(shiny)

citations <- readRDS("data/citations.rds")
tdc_data <- read_csv("data/tdc_data.csv") |>
  mutate(marker_label = purrr::pmap_chr(list(Location_label, Species_label, Run_label, Tissue_label, DOI, location_type),
                                        ~
                                          paste0("<strong>Location:</strong> ",..1,
                                                 ifelse(..6 == "approximated", " (Approximate)", ""),"</br>",
                                                 "<strong>Species:</strong> ",..2,"</br>",
                                                 "<strong>Run:</strong> ",..3,"</br>",
                                                 "<strong>Tissue:</strong> ",..4,"</br>",
                                                 "<strong>DOI:</strong> <a href='",..5,"' target='_blank'>",..5,"</a>")
  ))

function(input, output, session) {

  observeEvent(input$sidebarId,
               {

                 # browser()

                 if(input$sidebarId){
                   removeCssClass(id = "header_toggle",
                                  class = "far fa-square-caret-right")
                   addCssClass(id = "header_toggle",
                               class = "far fa-square-caret-left")
                 } else{
                   removeCssClass(id = "header_toggle",
                                  class = "far fa-square-caret-left")
                   addCssClass(id = "header_toggle",
                               class = "far fa-square-caret-right")
                 }


               })

  filtered_data <- reactiveValues(
    tdc_data = tdc_data,
    citations = citations
  )

  observe({

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
      filter((all(selected_location == "") | Location_label %in% selected_location),
             (all(selected_species == "") | Species_label %in% selected_species),
             (all(selected_run == "") | Run_label %in% selected_run),
             (all(selected_tissue == "") | Tissue_label %in% selected_tissue))

    filtered_data$citations <-
      citations |>
      filter(DOI %in% filtered_data$tdc_data$DOI)

  })

  # Create a table summarizes data sets by their associated reference (assuming it exists)
  output$tdc_data_table <-
    reactable::renderReactable({

      reactable::reactable(data.frame("x" = filtered_data$citations$formatted_metadata),
                           columns = list(
                             x = colDef(html = TRUE,
                                        name = "")
                           ),
                           sortable = FALSE,
                           # selection = "single",
                           showSortable = FALSE,
                           defaultPageSize = 30,
                           details = function(index){

                             htmltools::div(#style = "padding: 1rem",
                                            reactable(tdc_data |>
                                                        filter(DOI == filtered_data$citations[index,]$DOI) |>
                                                        select(-c(ends_with("label"),
                                                                  Title, DOI,
                                                                  location_type)),
                                                      outlined = TRUE)
                             )

                           })

    })

  # Re-render the table depending on how the user zooms into the map
  observe({

    req(input$tdc_data_map_bounds)

    # browser()

    map_zoom_dois <-
      filtered_data$tdc_data |>
      filter(
        between(Latitude_DD, input$tdc_data_map_bounds$south, input$tdc_data_map_bounds$north),
        between(Longitude_DD, input$tdc_data_map_bounds$west, input$tdc_data_map_bounds$east)
      ) |>
      pull(DOI)

    filtered_citations_zoomed <-
      filtered_data$citations |>
      filter(DOI %in% map_zoom_dois)

    # if the user has selected a marker, highlight in the table.
    if(!is.null(input$tdc_data_map_marker_click$id)){

      selected_marker_doi <-
        filtered_data$tdc_data  |>
        isolate() |>
        distinct(DOI, Location, Latitude_DD, Longitude_DD, marker_label)  |>
        filter(!is.na(Latitude_DD)) |>
        slice(input$tdc_data_map_marker_click$id) |>
        pull(DOI)

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
                               x = colDef(html = TRUE,
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
                                 reactable(tdc_data |>
                                             filter(DOI == filtered_citations_zoomed[index,]$DOI) |>
                                             select(-c(ends_with("label"),
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
      # isolate() |>
      distinct(DOI, Latitude_DD, Longitude_DD, .keep_all = TRUE) |>
      filter(!is.na(Latitude_DD))

    tdc_map <-
      leaflet(data = plt_data) |>
      addTiles() |>
      # addAwesomeMarkers(
      addMarkers(
        layerId = 1:nrow(plt_data),
        lng = ~Longitude_DD,
        lat = ~Latitude_DD,
        # lng = ~jitter(Longitude_DD, factor = 0.001),
        # lat = ~jitter(Latitude_DD, factor = 0.001),
        # icon = awesomeIcons(icon = "map-pin", markerColor = "blue"),
        # label = ~purrr::map(marker_label, HTML),
        popup = ~purrr::map(marker_label, HTML)
        ,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE,
                                               spiderfyOnMaxZoom = TRUE,
                                               maxClusterRadius = 0)
        )

    return(tdc_map)

  })

  # Update map markers based on selected filters
  observe({

    plt_data <-
      filtered_data$tdc_data |>
      distinct(DOI, Latitude_DD, Longitude_DD, .keep_all = TRUE) |>
      filter(!is.na(Latitude_DD))

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

    leafletProxy("tdc_data_map",
                 session = session,
                 data = plt_data) %>%
      clearMarkers() |>
      # addAwesomeMarkers(
      addMarkers(
        layerId = 1:nrow(plt_data),
        lng = ~Longitude_DD,
        lat = ~Latitude_DD,
        # lng = ~jitter(Longitude_DD, factor = 0.001),
        # lat = ~jitter(Latitude_DD, factor = 0.001),
        # icon = icons,
        # label = ~purrr::map(marker_label, HTML),
        popup = ~purrr::map(marker_label, HTML)
        ,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE,
                                               spiderfyOnMaxZoom = TRUE,
                                               maxClusterRadius = 0)
        )

  })

  ### Visualize tab code

  output$ec50_curve <- renderPlotly({

    plt <-
      tdc_data |>
      filter(Thiamine_conc < 30) |>
      mutate(plot_label = paste0("Thiamin Conc: ", round(Thiamine_conc,2),"\n",
                                 "% Survived: ", round(Percent_survive,2))) |>
      ggplot(aes(x = Thiamine_conc,
                 y = Percent_survive)) +
      geom_point() +
      theme_minimal() +
      labs(x = "Thiamin Concentration (nmol/g)",
           y = "% Survived")

    plotly::ggplotly(plt)

  })

}
