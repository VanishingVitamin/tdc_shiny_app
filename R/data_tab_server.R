data_tab_server <- function(input, output, session, filtered_data){
  # Create a table summarizes data sets by their associated reference
  # (assuming the reference exists)
  output$tdc_data_table <-
    reactable::renderReactable({
      reactable::reactable(
        data.frame("metadata" = filtered_data$citations$formatted_metadata) |>
          dplyr::mutate(
            download_button = paste0(
              "<a id='data_download_",
              1:dplyr::n(),
              "' class='shiny-download-link disabled' href='' target='_blank' download aria-disabled='true' tabindex='-1'>Download data</a>"
            ),
            download_button = dplyr::case_when(
              startsWith(metadata, "Unpublished") ~
                "<p>Not available for download</p>"
            )
          ),
        columns = list(
          metadata = reactable::colDef(
            html = TRUE,
            name = "",
            details = function(index) {
              detail_data <- tdc_data |>
                dplyr::distinct(unique_id, table_details) |>
                dplyr::filter(
                  unique_id == filtered_data$citations[index, ]$unique_id
                )

              paste0(
                "<div style = 'padding: 1rem'>",
                detail_data$table_details,
                "</div>"
              )
            }
          ),
          download_button = reactable::colDef(html = TRUE, show = FALSE)
        ),
        sortable = FALSE,
        # selection = "single",
        showSortable = FALSE,
        defaultPageSize = 30
      )
    })

  # show a pop-up message that lets user download data
  shiny::observe({
    shiny::showModal(
      ui = shiny::modalDialog(
        easyClose = TRUE,
        size = "xl",
        title = "Download Data",
        fade = TRUE,
        # drop-down menu element (excludes unpublished data)
        shinyWidgets::virtualSelectInput(
          inputId = "data_download_select",
          choices = stats::setNames(
            filtered_data$citations$formatted_metadata[
              !startsWith(
                filtered_data$citations$formatted_metadata,
                "Unpublished"
              )
            ],
            filtered_data$citations$formatted_metadata[
              !startsWith(filtered_data$citations$unique_id, "Unpublished")
            ]
          ),
          html = TRUE,
          width = "100%",
          label = "Select publication (only published data available)",
          disableSelectAll = FALSE,
          multiple = TRUE,
          allowNewOption = FALSE,
          hideClearButton = FALSE
        ),
        # download button element
        shiny::div(
          style = "position: inline;",
          shiny::downloadButton(
            outputId = "download_data_csv",
            label = "Download to csv"
          )
        )
      ),
      session = session
    )
  }) |>
    shiny::bindEvent(input$dataset_download_popup)

  # logic to save user-selected data as a csv
  output$download_data_csv <-
    shiny::downloadHandler(
      filename = function() {
        paste0("data_download_", Sys.Date(), ".csv")
      },
      content = function(file) {
        selected_ids <-
          citations |>
          dplyr::filter(formatted_metadata %in% input$data_download_select) |>
          dplyr::pull(unique_id)

        tdc_data |>
          dplyr::filter(unique_id %in% selected_ids) |>
          readr::write_csv(file = file)
      }
    )

  # Re-render the table depending on how the user zooms into the map
  shiny::observe({
    shiny::req(input$tdc_data_map_bounds)

    map_zoom_ids <-
      filtered_data$tdc_data |>
      dplyr::filter(
        dplyr::between(
          Latitude_DD,
          input$tdc_data_map_bounds$south,
          input$tdc_data_map_bounds$north
        ),
        dplyr::between(
          Longitude_DD,
          input$tdc_data_map_bounds$west,
          input$tdc_data_map_bounds$east
        )
      ) |>
      dplyr::pull(unique_id) |>
      unique()

    filtered_citations_zoomed <-
      filtered_data$citations |>
      dplyr::filter(unique_id %in% map_zoom_ids) |>
      unique()

    # if the user has selected a marker, highlight in the table.

    selected_marker_id <- ""
    if (!is.null(input$tdc_data_map_marker_click$id)) {
      selected_marker_id <-
        filtered_data$tdc_data |>
        shiny::isolate() |>
        dplyr::distinct(
          unique_id,
          Location,
          Latitude_DD,
          Longitude_DD,
          marker_label
        ) |>
        dplyr::filter(!is.na(Latitude_DD)) |>
        dplyr::mutate(
          dist_to_click = purrr::map2_dbl(
            Latitude_DD,
            Longitude_DD,
            ~ sqrt(
              (.x - input$tdc_data_map_marker_click$lat)^2 +
                (.y - input$tdc_data_map_marker_click$lng)^2
            )
          )
        ) |>
        dplyr::filter(dist_to_click == min(dist_to_click)) |>
        # dplyr::slice(1) |>
        dplyr::pull(unique_id) |>
        unique()

      # selected_citation_index <- as.integer(which(filtered_citations_zoomed$unique_id == selected_marker_id))
      selected_citation_index <- 1:length(selected_marker_id)
    } else {
      selected_citation_index <- -1L
    }

    if (length(selected_citation_index) == 0) {
      selected_citation_index <- -1L
    }

    # pull the table details for the rearranged rows
    table_details_zoomed <- tdc_data |>
      dplyr::distinct(unique_id, table_details) |>
      dplyr::filter(unique_id %in% map_zoom_ids) |>
      dplyr::arrange(forcats::fct_relevel(
        factor(unique_id),
        selected_marker_id
      )) |>
      dplyr::pull(table_details)

    output$tdc_data_table <-
      reactable::renderReactable({
        reactable::reactable(
          data.frame(
            "x" = filtered_citations_zoomed |>
              dplyr::arrange(forcats::fct_relevel(
                factor(unique_id),
                selected_marker_id
              )) |>
              dplyr::pull(formatted_metadata)
          ),
          columns = list(
            x = reactable::colDef(
              html = TRUE,
              name = "",
              details = function(index) {
                paste0(
                  "<div style = 'padding: 1rem'>",
                  table_details_zoomed[index],
                  "</div>"
                )
              }
            )
          ),
          sortable = FALSE,
          # selection = "single",
          showSortable = FALSE,
          defaultPageSize = 30,
          rowStyle = function(index) {
            if (index %in% selected_citation_index) {
              return(list(background = "#EFEFEF"))
            }
          }
        )
      })
  })

  # Create an interactive map with markers indicating where data were collected.
  # Clicking on a marker shows information about that data collection.
  output$tdc_data_map <- leaflet::renderLeaflet({
    lat_bounds <- range(filtered_data$tdc_data$Latitude_DD, na.rm = TRUE)
    long_bounds <- range(filtered_data$tdc_data$Longitude_DD, na.rm = TRUE)

    plt_data <-
      filtered_data$tdc_data |>
      dplyr::distinct(
        unique_id,
        Latitude_DD,
        Longitude_DD,
        .keep_all = TRUE
      ) |>
      dplyr::filter(!is.na(Latitude_DD))

    tdc_map <-
      leaflet::leaflet(data = plt_data) |>
      leaflet::addTiles() |>
      leaflet::addMarkers(
        layerId = seq_len(length.out = nrow(plt_data)),
        lng = ~Longitude_DD,
        lat = ~Latitude_DD,
        popup = ~ purrr::map(marker_label, HTML),
        clusterOptions = leaflet::markerClusterOptions(
          removeOutsideVisibleBounds = TRUE,
          spiderfyOnMaxZoom = TRUE
        )
      )

    # if the user hasn't applied any filters, set the view manually to fit Pacific to Baltic in the view window
    if (nrow(filtered_data$tdc_data) == nrow(tdc_data)) {
      tdc_map <- tdc_map |>
        leaflet::setView(
          lng = mean(long_bounds),
          lat = mean(lat_bounds),
          zoom = 2
        )
    }

    return(tdc_map)
  })

  # Update map markers based on selected filters
  shiny::observe({
    plt_data <-
      filtered_data$tdc_data |>
      dplyr::distinct(
        unique_id,
        Latitude_DD,
        Longitude_DD,
        .keep_all = TRUE
      ) |>
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
    #                  markerColor = c("blue", "black")[(plt_data$unique_id == {filtered_data$citations |> slice(selected) |> pull(unique_id)}) + 1])
    # }

    # leafletProxy lets you update the currently rendered leaflet map (rather than re-rendering a *new* map)
    leaflet::leafletProxy(
      "tdc_data_map",
      session = session,
      data = plt_data
    ) |>
      leaflet::clearMarkers() |>
      leaflet::addMarkers(
        layerId = seq_len(length.out = nrow(plt_data)),
        lng = ~Longitude_DD,
        lat = ~Latitude_DD,
        popup = ~ purrr::map(marker_label, HTML),
        clusterOptions = leaflet::markerClusterOptions(
          removeOutsideVisibleBounds = TRUE,
          spiderfyOnMaxZoom = TRUE
        )
      )
  })
}
