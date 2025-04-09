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
lc50_curve <- read_csv("data/lc50_curve_tdc_data.csv") |>
  mutate(plot_label = paste0("Thiamin Conc.: ",Thiamin_conc, " nmol/g\n",
                             "Estimated % Survival: ",round(survival_median,2),"%",
                             " (",round(survival_ci_97.5, 2),"%, ",round(survival_ci_2.5, 2), "%)"))

dose_response <-
  function(Thiamin_conc, ec50_mu, slope_p, upper_p, lower_p = 0){
    upper_p + (lower_p - upper_p)/(1 + (Thiamin_conc/ec50_mu)**slope_p)
  }

function(input, output, session) {

  observeEvent(input$filter_sidebar,
               {

                 if(input$filter_sidebar){
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

  observe({

    if(!isolate(input$filter_sidebar) & input$navmenu %in% c("data", "visualize")){
      bs4Dash::updateSidebar(id = "filter_sidebar")
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

    #

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
      distinct(DOI, Latitude_DD, Longitude_DD, .keep_all = TRUE) |>
      filter(!is.na(Latitude_DD))

    # fish_icon <- makeIcon(
    #   iconUrl = 'https://www.svgrepo.com/show/461462/fish.svg',
    #   iconWidth = 40, iconHeight = 30,
    #   iconAnchorX = 0, iconAnchorY = 0
    # )
    fish_icons <- iconList(
      "CHINOOK" = makeIcon("./www/fish-outline-black.svg", iconWidth = 40, iconHeight = 30),
      "LAKE TROUT" = makeIcon("./www/fish-outline-blue.svg", iconWidth = 40, iconHeight = 30),
      "COHO" = makeIcon("./www/fish-outline-orange.svg", iconWidth = 40, iconHeight = 30),
      "STEELHEAD TROUT" = makeIcon("./www/fish-outline-purple.svg", iconWidth = 40, iconHeight = 30),
      "ATLANTIC SALMON" = makeIcon("./www/fish-outline-red.svg", iconWidth = 40, iconHeight = 30)
    )

    html_legend <- "<img src='fish-outline-black.svg' style='width: 40px; height: 30px'>CHINOOK<br/>
    <img src='fish-outline-blue.svg' style='width: 40px; height: 30px'>LAKE TROUT<br/>
    <img src='fish-outline-orange.svg' style='width: 40px; height: 30px'>COHO<br/>
    <img src='fish-outline-purple.svg' style='width: 40px; height: 30px'>STEELHEAD TROUT<br/>
    <img src='fish-outline-red.svg' style='width: 40px; height: 30px'>ATLANTIC SALMON"

    tdc_map <-
      leaflet(data = plt_data) |>
      addTiles() |>
      addMarkers(
        layerId = 1:nrow(plt_data),
        lng = ~Longitude_DD,
        lat = ~Latitude_DD,
        popup = ~purrr::map(marker_label, HTML),
        # icon = fish_icon,
        icon = ~fish_icons[Species_label],
        ,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE,
                                               spiderfyOnMaxZoom = TRUE,
                                               maxClusterRadius = 0)
      ) |>
      addControl(html = html_legend, position = "bottomright")

    return(tdc_map)

  })

  # Update map markers based on selected filters
  observe({

    plt_data <-
      filtered_data$tdc_data |>
      distinct(DOI, Latitude_DD, Longitude_DD, .keep_all = TRUE) |>
      filter(!is.na(Latitude_DD))

    fish_icons <- iconList(
      "CHINOOK" = makeIcon("./www/fish-outline-black.svg", iconWidth = 40, iconHeight = 30),
      "LAKE TROUT" = makeIcon("./www/fish-outline-blue.svg", iconWidth = 40, iconHeight = 30),
      "COHO" = makeIcon("./www/fish-outline-orange.svg", iconWidth = 40, iconHeight = 30),
      "STEELHEAD TROUT" = makeIcon("./www/fish-outline-purple.svg", iconWidth = 40, iconHeight = 30),
      "ATLANTIC SALMON" = makeIcon("./www/fish-outline-red.svg", iconWidth = 40, iconHeight = 30)
    )

    leafletProxy("tdc_data_map",
                 session = session,
                 data = plt_data) %>%
      clearMarkers() |>
      addMarkers(
        layerId = 1:nrow(plt_data),
        lng = ~Longitude_DD,
        lat = ~Latitude_DD,
        popup = ~purrr::map(marker_label, HTML)
        , icon = ~fish_icons[Species_label]
        ,clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = TRUE,
                                               spiderfyOnMaxZoom = TRUE,
                                               maxClusterRadius = 0)
      )

  })

  ### Visualize tab code

  # Render interactive scatterplot
  output$ec50_curve <-
    renderPlotly({

      # Prepare the TDC data for plotting. This includes creating a label for each
      # point that will appear when hovering over the point.
      plt_data <-
        filtered_data$tdc_data |>
        group_by(Thiamin_conc) |>
        arrange(desc(Percent_survive)) |>
        group_by(Thiamin_conc) |>
        mutate(ind = row_number(),
               plot_label = paste0("Thiamin Conc: ", Thiamin_conc," nmol/g\n",
                                   "Observed % Survived: ", round(Percent_survive,2),"%")) |>
        ungroup()

      # The first "layer" of the plot will be a model-based approximate 95%
      # confidence band
      plt <- plot_ly() |>
        add_ribbons(
          data = lc50_curve,
          x = ~Thiamin_conc,
          ymin = ~survival_ci_2.5,
          ymax = ~survival_ci_97.5,
          fillcolor = "black",
          opacity = .15,
          line = list(color = "black", opacity = .1),
          showlegend = FALSE
        ) |>
        # The second layer of the plot is a fitted median survival curve.
        add_lines(data = lc50_curve,
                  x = ~Thiamin_conc, y = ~survival_median,
                  text = ~plot_label, hoverinfo = "text",
                  hoverlabel = list(bgcolor = "#e5e5e5"),
                  color = I("black"), opacity = .5,
                  showlegend = FALSE)

      # Next add the points ("markers"). We want the user to hover over a
      # particular Thiamin Concentration "x" value and see (1) all observed % Survived
      # "y" values for each x.

      # Note that there are some observations that share a common Thiamin
      # Concentration value but have different Survival % values. These would
      # appear as vertically stacked points in the plot. A quirk of plotly in this
      # situation is that it will only show one of the hoverable labels for each
      # Thiamin Conc value *unless* they are plotted as separate "layers". The for
      # loop creates multiple layers of points to ensure that all point labels
      # will show when hovering.
      for(trace_ind in unique(plt_data$ind)){

        plt <- plt |>
          add_markers(data = {plt_data |> filter(ind == trace_ind)},
                      x = ~Thiamin_conc, y = ~Percent_survive,
                      text = ~plot_label, hoverinfo = "text",
                      color = I("black"),
                      sizes = 2, opacity = .8,
                      showlegend = FALSE)

      }

      # The last layer in the plotly object should contain the user-uploaded
      # data. We'd like to keep track of which layer index this corresponds
      # to (starting from 0)
      filtered_data$max_base_layer <- length(plt$x$attrs) - 1

      #

      user_dat <- isolate(filtered_data$user_data)

      if(!is.null(user_dat)){
        if(nrow(user_dat) > 0){

          if(nrow(user_dat) == 1){
            user_dat_plt <- user_dat |>
              slice(1,1)
          } else{
            user_dat_plt <- user_dat
          }

          user_dat_plt <-
            user_dat_plt |>
            mutate(plt_label_observed = paste0("Thiamin Conc.: ",Thiamin_conc," nmol/g<br>",
                                               "Observed % Survived: ",Percent_survive,"%"),
                   plt_label_estimated = paste0("Thiamin Conc.: ",Thiamin_conc," nmol/g<br>",
                                                "Estimated % Survived: ",round(Estimated_survive,2),"%"))

          plt <- plt |>
            # add_markers(data = user_dat_plt,
            #             x = ~Thiamin_conc,
            #             y = ~Estimated_survive,
            #             text = ~plt_label_estimated,
            #             hoverinfo = "text",
            #             hoverlabel = list(bgcolor = "red"),
            #             marker = list(color = "red", size = 10, symbol = "x"),
            #             name = "Estimated user data") |>
            add_markers(data = user_dat_plt,
                        x = ~Thiamin_conc,
                        y = ~Percent_survive,
                        text = ~plt_label_observed,
                        hoverinfo = "text",
                        hoverlabel = list(bgcolor = "red"),
                        marker = list(color = "red", size = 8),
                        name = "Observed user data")

        }
      }

      # Finally, change the layout and styling of the output plot.
      plt |>
        plotly::layout(
          xaxis = list(title = "Thiamin Concentration (nmol/g)",
                       showspikes = TRUE,
                       spikesnap = "hovered data",
                       spikemode = "toaxis+across",
                       spikedash = "dot",
                       spikethickness = 1,
                       range = c(-1, max(plt_data$Thiamin_conc, na.rm = TRUE) + 1),
                       tickmode = "dynamic"),
          yaxis = list(title = "% Survived",
                       range = c(-3,103)),
          hovermode = "x",
          hoverdistance = 1,
          showlegend = TRUE
        ) |>
        plotly::style(
          hoverinfo = "skip", traces = 1
        )

    })

  # This code controls how data are uploaded to the app. The use can upload a
  # csv or xlsx file to the app. If they choose this option, they must indicate
  # which columns in the data set contain the Thiamin Concentration and
  # (optionally) the % Survived data.
  observe({

    ext <- tools::file_ext(input$visualize_add_data_file$datapath)

    if(ext == "csv"){
      filtered_data$user_uploaded_data <- readr::read_csv(input$visualize_add_data_file$datapath)
    } else if(ext == "xlsx"){
      filtered_data$user_uploaded_data <- readxl::read_xlsx(input$visualize_add_data_file$datapath)
    } else{
      showNotification(ui = "Unable to read the file. Make sure it is either .csv or .xlsx",
                       type = "error")
      req(FALSE)
    }

    shinyjs::show(id = "visualize_add_data_file_panel")

    updateSelectInput(inputId = "visualize_add_data_file_thiamin_col",
                      choices = c("", names(filtered_data$user_uploaded_data)))
    updateSelectInput(inputId = "visualize_add_data_file_survive_col",
                      choices = c("", names(filtered_data$user_uploaded_data)))

  }) |>
    bindEvent(input$visualize_add_data_file,
              ignoreInit = TRUE)

  # Once the user indicates which columns in the uploaded data set correspond to
  # Thiamin Concentration and (optionally) % Survived,
  observe({

    plot_data <-
      filtered_data$user_uploaded_data |>
      select(any_of(c(input$visualize_add_data_file_thiamin_col,
                      input$visualize_add_data_file_survive_col))) |>
      purrr::set_names(c("Thiamin_conc", "Percent_survive")) |>
      mutate(Thiamin_conc = as.numeric(Thiamin_conc),
             Percent_survive = as.numeric(Percent_survive))

    if(any(is.na(plot_data$Thiamin_conc))){
      showNotification(ui = "Something is wrong with the Thiamin Concentration column. It should only contain numeric values. Check your data and try again.",
                       type = "error")
      req(FALSE)
    }

    filtered_data$user_data <-
      bind_rows(filtered_data$user_data,
                plot_data) |>
      mutate(index = row_number(),
             Estimated_survive = dose_response(Thiamin_conc = Thiamin_conc,
                                               ec50_mu = unique(lc50_curve$ech50_50),
                                               slope_p = unique(lc50_curve$slope_50),
                                               upper_p = 1,lower_p = 0)*100) |>
      select(index, Thiamin_conc, Percent_survive, Estimated_survive)

  }) |>
    bindEvent(input$visualize_add_data_upload, ignoreInit = TRUE)

  # Another option is for the user to copy + paste data into a box. We'll make
  # some simple assumptions including that the first row contains column names.
  observe({

    req(input$visualize_add_data_clipboard)

    filtered_data$user_clipboard_data <-
      read.csv(text = input$visualize_add_data_clipboard,
                            header = TRUE)

    #

    shinyjs::show(id = "visualize_add_data_clipboard_panel")

    updateSelectInput(inputId = "visualize_add_data_clipboard_thiamin_col",
                      choices = c("",names(filtered_data$user_clipboard_data)))
    updateSelectInput(inputId = "visualize_add_data_clipboard_survive_col",
                      choices = c("",names(filtered_data$user_clipboard_data)))

  })

  observe({



    plot_data <-
      filtered_data$user_clipboard_data |>
      select(any_of(c(input$visualize_add_data_clipboard_thiamin_col,
                      input$visualize_add_data_clipboard_survive_col))) |>
      purrr::set_names(c("Thiamin_conc", "Percent_survive")) |>
      mutate(Thiamin_conc = as.numeric(Thiamin_conc),
             Percent_survive = as.numeric(Percent_survive))

    if(any(is.na(plot_data$Thiamin_conc))){
      showNotification(ui = "Something is wrong with the Thiamin Concentration column. It should only contain numeric values. Check your data and try again.",
                       type = "error")
      req(FALSE)
    }

    filtered_data$user_data <-
      bind_rows(filtered_data$user_data,
                plot_data) |>
      mutate(index = row_number(),
             Estimated_survive = dose_response(Thiamin_conc = Thiamin_conc,
                                               ec50_mu = unique(lc50_curve$ech50_50),
                                               slope_p = unique(lc50_curve$slope_50),
                                               upper_p = 1,lower_p = 0)*100) |>
      select(index, Thiamin_conc, Percent_survive, Estimated_survive)

  }) |>
    bindEvent(input$visualize_add_data_clipboard_button)

  # Another option is for the user to manually enter Thiamin Concentration and
  # (optionally) % Survived data. We'll add a few checks here to ensure the
  # information added satisfies some basic assumptions.
  observe({

    if(is.na(input$visualize_add_data_manual_thiamin) | is.null(input$visualize_add_data_manual_thiamin)){
      shiny::showNotification(ui = "Thiamin concentration must be a number greater than 0.", type = "error")
      DT::reloadData(proxy = DT::dataTableProxy(outputId = "visualize_add_data_manual"))
      req(FALSE)
    } else if(input$visualize_add_data_manual_thiamin <= 0){
      shiny::showNotification(ui = "Thiamin concentration must be a number greater than 0.", type = "error")
      DT::reloadData(proxy = DT::dataTableProxy(outputId = "visualize_add_data_manual"))
      req(FALSE)
    }

    filtered_data$user_data <-
      bind_rows(filtered_data$user_data,
                data.frame(Thiamin_conc = input$visualize_add_data_manual_thiamin,
                           Percent_survive = input$visualize_add_data_manual_survival)) |>
      mutate(index = row_number(),
             Estimated_survive = dose_response(Thiamin_conc = Thiamin_conc,
                                               ec50_mu = unique(lc50_curve$ech50_50),
                                               slope_p = unique(lc50_curve$slope_50),
                                               upper_p = 1,lower_p = 0)*100) |>
      select(index, Thiamin_conc, Percent_survive, Estimated_survive)

  }) |>
    bindEvent(input$visualize_add_data_new_row, ignoreInit = TRUE)

  # This code updates the Plotly scatterplot to show the user's new data as red
  # points
  observe({

    req(filtered_data$user_data)

    #

    plotly::plotlyProxy("ec50_curve", session, deferUntilFlush = FALSE) |>
      # Delete all layers above the top-most base layer
      plotlyProxyInvoke(method = "deleteTraces", list(as.integer(filtered_data$max_base_layer + 1))) |>
      # Replace deleted layer(s) with new layer
      plotlyProxyInvoke("addTraces",
                        # NOTE: addTraces needs at least two points, for some
                        # reason, so we'll repeat single points twice
                        list(x = rep(filtered_data$user_data$Thiamin_conc,
                                     length.out = max(2,length(filtered_data$user_data$Thiamin_conc))),
                             y = rep(filtered_data$user_data$Percent_survive,
                                     length.out = max(2,length(filtered_data$user_data$Percent_survive))),
                             text = paste0("Thiamin Conc.: ",filtered_data$user_data$Thiamin_conc," nmol/g<br>",
                                           "Observed % Survived: ",filtered_data$user_data$Percent_survive,"%"),
                             hoverinfo = "text",
                             hoverlabel = list(bgcolor = "red"),
                             marker = list(color = "red", size = 8),
                             name = "Observed user data",
                             type = "scatter", mode = "markers"))
  })

  output$visualize_add_data <-
    renderReactable({

      req(filtered_data$user_data)
      req(nrow(filtered_data$user_data) > 0)
      req(filtered_data$user_data$Estimated_survive)

      filtered_data$user_data |>
        mutate(Estimated_survive = round(Estimated_survive,2)) |>
        rename(`Observation Number` = index,
               `Thiamin Concentration (nmol/g)` = Thiamin_conc,
               `Observed % Survived` = Percent_survive,
               `Estimated % Survived` = Estimated_survive) |>
        select(-`Observation Number`) |>
        distinct() |>
        reactable()

    })

  # NOTE: the commented-out code below is not used in the current version of the
  # app. It's kept here for posterity.

  # observe({
  #
  #   req(input$visualize_add_data_manual_cell_edit)
  #
  #   info <- isolate(input$visualize_add_data_manual_cell_edit)
  #   i <- info$row
  #   j <- info$col + 1
  #   v <- as.numeric(info$value)
  #
  #   if(j == 1){
  #     if(is.na(v)){
  #       shiny::showNotification(ui = "Thiamin concentration must be a number greater than 0.", type = "error")
  #       DT::reloadData(proxy = DT::dataTableProxy(outputId = "visualize_add_data_manual"))
  #       req(FALSE)
  #     } else if(v <= 0){
  #       shiny::showNotification(ui = "Thiamin concentration must be a number greater than 0.", type = "error")
  #       DT::reloadData(proxy = DT::dataTableProxy(outputId = "visualize_add_data_manual"))
  #       req(FALSE)
  #     }
  #   } else if(j == 2){
  #     if(is.na(v)){
  #       shiny::showNotification(ui = "% Survived must be a number between 0 and 100.", type = "error")
  #       DT::reloadData(proxy = DT::dataTableProxy(outputId = "visualize_add_data_manual"))
  #       req(FALSE)
  #     } else if(v < 0 | v > 100){
  #       shiny::showNotification(ui = "% Survived must be a number between 0 and 100.", type = "error")
  #       DT::reloadData(proxy = DT::dataTableProxy(outputId = "visualize_add_data_manual"))
  #       req(FALSE)
  #     }
  #   }
  #
  #   filtered_data$user_data[i, j] <- as.character(v)
  #
  # })
  #
  # observeEvent(input$visualize_add_data_new_row,{
  #
  #   filtered_data$user_data <- bind_rows(isolate(filtered_data$user_data),
  #                                        tibble::tibble("Thiamin Conc. (nmol/g)" = "Double-click to edit",
  #                                                       "% Survived" = "(Optional) Double-click to edit"))
  #
  # })

}
