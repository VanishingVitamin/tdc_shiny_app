#'Defines the Server function for the Vanishing Vitamin Shiny app
#'
#'This is an internal function that is used within the exported launch_app()
#'function.
#'
#'@param tdc_data data set containing Thiamin by Survivability data. Should be
#'  the data set exported by the vanishingVitamin package,
#'  vanishingVitamin::tdc_data
#'@param citations data set containing citations for data in the tdc_data data
#'  set. Should be the data set exported by the vanishingVitamin package,
#'  vanishingVitamin::citations
#'@param lc50_curve data set containing model-estimated survival % by thiamin
#'  concentration.Should be the data set exported by the vanishingVitamin
#'  package, vanishingVitamin::lc50_curve
#'
#'@return a function object containing app server logic
#' @keywords internal
#' @noRd

app_server <- function(tdc_data, citations, lc50_curve){
  function(input, output, session) {

    #addResourcePath("extdata", system.file("extdata", package = "vanishingVitamin"))

    output$splash_page_background <- shiny::renderUI({

      # shiny::tags$iframe(src = "extdata/splash_page_background.html",
      #                    width = "100%",
      #                    height = 1000,
      #                    seamless = "seamless")

      shiny::includeHTML("inst/extdata/splash_page_background.html")

    })

    output$splash_page_what_causes_tdc <- shiny::renderUI({

      # shiny::tags$iframe(src = "extdata/splash_page_background.html",
      #                    width = "100%",
      #                    height = 1000,
      #                    seamless = "seamless")

      shiny::includeHTML("inst/extdata/splash_page_what_causes_tdc.html")

    })

    output$splash_page_vitamers <- shiny::renderUI({

      # shiny::tags$iframe(src = "extdata/splash_page_background.html",
      #                    width = "100%",
      #                    height = 1000,
      #                    seamless = "seamless")

      shiny::includeHTML("inst/extdata/splash_page_vitamers.html")

    })

    output$splash_page_glossary <- shiny::renderUI({

      # shiny::tags$iframe(src = "extdata/splash_page_background.html",
      #                    width = "100%",
      #                    height = 1000,
      #                    seamless = "seamless")

      shiny::includeHTML("inst/extdata/splash_page_glossary.html")

    })

    shiny::observeEvent(input$filter_sidebar,
                        {

                          if(input$filter_sidebar){
                            shinyjs::removeCssClass(id = "header_toggle",
                                                    class = "far fa-square-plus")
                            shinyjs::addCssClass(id = "header_toggle",
                                                 class = "far fa-square-minus")
                          } else{
                            shinyjs::removeCssClass(id = "header_toggle",
                                                    class = "far fa-square-minus")
                            shinyjs::addCssClass(id = "header_toggle",
                                                 class = "far fa-square-plus")
                          }

                        })

    tab_automatically_opened <- reactiveVal(value = FALSE)

    shiny::observe({

      if(!shiny::isolate(input$filter_sidebar) & input$navmenu %in% c("data", "visualize") & !tab_automatically_opened()){
        bs4Dash::updateSidebar(id = "filter_sidebar")
        tab_automatically_opened(TRUE)
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

      selected_marker_doi <- ""
      if(!is.null(input$tdc_data_map_marker_click$id)){

        selected_marker_doi <-
          filtered_data$tdc_data  |>
          shiny::isolate() |>
          dplyr::distinct(DOI, Location, Latitude_DD, Longitude_DD, marker_label)  |>
          dplyr::filter(!is.na(Latitude_DD)) |>
          dplyr::mutate(dist_to_click = purrr::map2_dbl(Latitude_DD, Longitude_DD,
                                                        ~ sqrt((.x - input$tdc_data_map_marker_click$lat)^2 + (.y - input$tdc_data_map_marker_click$lng)^2))) |>
          dplyr::filter(dist_to_click == min(dist_to_click)) |>
          dplyr::slice(1) |>
          dplyr::pull(DOI)

        # selected_citation_index <- as.integer(which(filtered_citations_zoomed$DOI == selected_marker_doi))
        selected_citation_index <- 1

      } else{
        selected_citation_index <- -1L
      }

      if(length(selected_citation_index) == 0) selected_citation_index <- -1L

      output$tdc_data_table <-
        reactable::renderReactable({

          reactable::reactable(data.frame("x" =
                                            filtered_citations_zoomed |>
                                            dplyr::arrange(forcats::fct_relevel(factor(DOI), selected_marker_doi)) |>
                                            dplyr::pull(formatted_metadata)
          ),
          columns = list(
            x = reactable::colDef(html = TRUE,
                                  name = "")
          ),
          sortable = FALSE,
          # selection = "single",
          showSortable = FALSE,
          defaultPageSize = 30,
          rowStyle = function(index){

            if(index == selected_citation_index & selected_marker_doi %in% filtered_citations_zoomed$DOI){
              return(list(background = "#EFEFEF"))
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
          layerId = seq_len(length.out = nrow(plt_data)),
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
          layerId = seq_len(length.out = nrow(plt_data)),
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

    # Render interactive scatterplot
    output$ec50_curve <-
      plotly::renderPlotly({

        # Prepare the TDC data for plotting. This includes creating a label for
        # each point that will appear when hovering over the point.
        plt_data <-
          filtered_data$tdc_data |>
          dplyr::group_by(Thiamin_conc) |>
          dplyr::arrange(dplyr::desc(Percent_survive)) |>
          dplyr::group_by(Thiamin_conc) |>
          dplyr::mutate(ind = dplyr::row_number(),
                        plot_label = paste0("Thiamin Conc: ", Thiamin_conc," nmol/g\n",
                                            "Observed % Survived: ", round(Percent_survive,2),"%")) |>
          dplyr::ungroup()

        # The first "layer" of the plot will be a model-based approximate 95%
        # confidence band
        plt <- plotly::plot_ly() |>
          plotly::add_ribbons(
            data = lc50_curve,
            x = ~Thiamin_conc,
            ymin = ~survival_ci_2.5,
            ymax = ~survival_ci_97.5,
            name = "survival_ci",
            fillcolor = "black",
            opacity = .15,
            line = list(color = "black", opacity = .1),
            showlegend = FALSE
          ) |>
          # The second layer of the plot is a fitted median survival curve.
          plotly::add_lines(data = lc50_curve,
                            x = ~Thiamin_conc,
                            y = ~survival_median,
                            text = ~plot_label,
                            name = "survival_med",
                            hoverinfo = "text",
                            hoverlabel = list(bgcolor = "#e5e5e5"),
                            color = I("black"), opacity = .5,
                            showlegend = FALSE)

        # Next add the points ("markers"). We want the user to hover over a
        # particular Thiamin Concentration "x" value and see (1) all observed %
        # Survived "y" values for each x.

        # Note that there are some observations that share a common Thiamin
        # Concentration value but have different Survival % values. These would
        # appear as vertically stacked points in the plot. A quirk of plotly in this
        # situation is that it will only show one of the hoverable labels for each
        # Thiamin Conc value *unless* they are plotted as separate "layers". The for
        # loop creates multiple layers of points to ensure that all point labels
        # will show when hovering.
        for(trace_ind in unique(plt_data$ind)){

          plt <- plt |>
            plotly::add_markers(data = {plt_data |> dplyr::filter(ind == trace_ind)},
                                x = ~Thiamin_conc,
                                y = ~Percent_survive,
                                name = paste0("obs_survive_layer",trace_ind),
                                text = ~plot_label,
                                hoverinfo = "text",
                                color = I("black"),
                                sizes = 2, opacity = .8,
                                showlegend = FALSE)

        }

        # The last layer in the plotly object should contain the user-uploaded
        # data. We'd like to keep track of which layer index this corresponds
        # to (starting from 0)
        filtered_data$max_base_layer <- length(plt$x$attrs) - 1

        #

        user_dat <- shiny::isolate(filtered_data$user_data)

        if(!is.null(user_dat)){
          if(nrow(user_dat) > 0){

            if(nrow(user_dat) == 1){
              user_dat_plt <- user_dat |>
                dplyr::slice(1,1)
            } else{
              user_dat_plt <- user_dat
            }

            user_dat_plt <-
              user_dat_plt |>
              dplyr::mutate(plt_label_observed = paste0("Thiamin Conc.: ",Thiamin_conc," nmol/g<br>",
                                                        "Observed % Survived: ",Percent_survive,"%"),
                            plt_label_estimated = paste0("Thiamin Conc.: ",Thiamin_conc," nmol/g<br>",
                                                         "Estimated % Survived: ",round(Estimated_survive,2),"%"))

            plt <- plt |>
              plotly::add_markers(data = user_dat_plt,
                                  x = ~Thiamin_conc,
                                  y = ~Percent_survive,
                                  text = ~plt_label_observed,
                                  hoverinfo = "text",
                                  hoverlabel = list(bgcolor = "red"),
                                  marker = list(color = "red", size = 8),
                                  name = "Observed user data") |>
              plotly::add_markers(data = user_dat_plt,
                                  x = ~Thiamin_conc,
                                  y = ~Estimated_survive,
                                  text = ~plt_label_estimated,
                                  hoverinfo = "skip",
                                  # hoverlabel = list(bgcolor = "red"),
                                  marker = list(color = "red", size = 8, symbol = "circle-open-dot"),
                                  name = "Estimated user data")

          }
        }

        # Finally, change the layout and styling of the output plot.

        # This is a bit of custom JavaScript code for controlling how points are
        # removed from the plot. See <https://stackoverflow.com/a/53831080>
        remove_trace_js <- "function(el, x, data){
         var id = el.getAttribute('id');
         Shiny.addCustomMessageHandler('remove-trace', function(tracename) {
         function getTraceIndices(trace, traceindex) {
           if (trace.name === tracename) {
             Plotly.deleteTraces(id, traceindex);
           }
         }
         x.data.forEach(getTraceIndices);
         });
       }"

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
          ) |>
          ## The following commented-out code would add the ability to switch
          ## between a linear vs. log scale x-axis. This looks a bit awkward
          ## since there are a non-trivial number of thiamin concentrations
          ## between 0 and 1. I'll leave this here in case we think of another
          ## solution.
          # plotly::layout(
          #   updatemenus = list(
          #     list(y = .5,x = .5,
          #          buttons = list(
          #            list(method = "relayout",
          #                 label = "Linear x-axis",
          #                 args = list(list(xaxis = list(type =  "linear")))
          #            ),
          #            list(method = "relayout",
          #                 label = "Log x-axis",
          #                 args = list(list(xaxis = list(type =  "log")))
          #            )
          #          ))
          #   )
          #   ) |>
          htmlwidgets::onRender(remove_trace_js)

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
        shiny::showNotification(ui = "Unable to read the file. Make sure it is either .csv or .xlsx",
                                type = "error")
        shiny::req(FALSE)
      }

      shinyjs::show(id = "visualize_add_data_file_panel")

      shiny::updateSelectInput(inputId = "visualize_add_data_file_thiamin_col",
                               choices = c("", names(filtered_data$user_uploaded_data)))
      shiny::updateSelectInput(inputId = "visualize_add_data_file_survive_col",
                               choices = c("", names(filtered_data$user_uploaded_data)))

    }) |>
      shiny::bindEvent(input$visualize_add_data_file,
                       ignoreInit = TRUE)

    # Once the user indicates which columns in the uploaded data set correspond to
    # Thiamin Concentration and (optionally) % Survived,
    observe({

      plot_data <-
        filtered_data$user_uploaded_data |>
        dplyr::select(dplyr::any_of(c(input$visualize_add_data_file_thiamin_col,
                                      input$visualize_add_data_file_survive_col))) |>
        purrr::set_names(c("Thiamin_conc", "Percent_survive")) |>
        dplyr::mutate(Thiamin_conc = as.numeric(Thiamin_conc),
                      Percent_survive = as.numeric(Percent_survive))

      if(any(is.na(plot_data$Thiamin_conc))){
        shiny::showNotification(ui = "Something is wrong with the Thiamin Concentration column. It should only contain numeric values. Check your data and try again.",
                                type = "error")
        shiny::req(FALSE)
      }

      filtered_data$user_data <-
        dplyr::bind_rows(filtered_data$user_data,
                         plot_data) |>
        dplyr::mutate(index = dplyr::row_number(),
                      Estimated_survive = dose_response(Thiamin_conc = Thiamin_conc,
                                                        ec50_mu = unique(lc50_curve$ec50_50),
                                                        slope_p = unique(lc50_curve$slope_50),
                                                        upper_p = 1,lower_p = 0)*100) |>
        dplyr::select(index, Thiamin_conc, Percent_survive, Estimated_survive)

    }) |>
      shiny::bindEvent(input$visualize_add_data_upload, ignoreInit = TRUE)

    # Another option is for the user to copy + paste data into a box. We'll make
    # some simple assumptions including that the first row contains column names.
    shiny::observe({

      shiny::req(input$visualize_add_data_clipboard)

      browser()

      filtered_data$user_clipboard_data <-
        utils::read.csv(text = input$visualize_add_data_clipboard,
                        sep = "",
                        header = TRUE)

      shinyjs::show(id = "visualize_add_data_clipboard_panel")

      shiny::updateSelectInput(inputId = "visualize_add_data_clipboard_thiamin_col",
                               choices = c("",names(filtered_data$user_clipboard_data)))
      shiny::updateSelectInput(inputId = "visualize_add_data_clipboard_survive_col",
                               choices = c("",names(filtered_data$user_clipboard_data)))

    })

    shiny::observe({



      plot_data <-
        filtered_data$user_clipboard_data |>
        dplyr::select(dplyr::any_of(c(input$visualize_add_data_clipboard_thiamin_col,
                                      input$visualize_add_data_clipboard_survive_col))) |>
        purrr::set_names(c("Thiamin_conc", "Percent_survive")) |>
        dplyr::mutate(Thiamin_conc = as.numeric(Thiamin_conc),
                      Percent_survive = as.numeric(Percent_survive))

      if(any(is.na(plot_data$Thiamin_conc))){
        shiny::showNotification(ui = "Something is wrong with the Thiamin Concentration column. It should only contain numeric values. Check your data and try again.",
                                type = "error")
        shiny::req(FALSE)
      }

      filtered_data$user_data <-
        dplyr::bind_rows(filtered_data$user_data,
                         plot_data) |>
        dplyr::mutate(index = dplyr::row_number(),
                      Estimated_survive = dose_response(Thiamin_conc = Thiamin_conc,
                                                        ec50_mu = unique(lc50_curve$ec50_50),
                                                        slope_p = unique(lc50_curve$slope_50),
                                                        upper_p = 1,lower_p = 0)*100) |>
        dplyr::select(index, Thiamin_conc, Percent_survive, Estimated_survive)

    }) |>
      shiny::bindEvent(input$visualize_add_data_clipboard_button)

    # Another option is for the user to manually enter Thiamin Concentration and
    # (optionally) % Survived data. We'll add a few checks here to ensure the
    # information added satisfies some basic assumptions.
    shiny::observe({

      if(is.na(input$visualize_add_data_manual_thiamin) | is.null(input$visualize_add_data_manual_thiamin)){
        shiny::showNotification(ui = "Thiamin concentration must be a number greater than 0.", type = "error")
        shiny::req(FALSE)
      } else if(input$visualize_add_data_manual_thiamin <= 0){
        shiny::showNotification(ui = "Thiamin concentration must be a number greater than 0.", type = "error")
        shiny::req(FALSE)
      }

      filtered_data$user_data <-
        dplyr::bind_rows(filtered_data$user_data,
                         data.frame(Thiamin_conc = input$visualize_add_data_manual_thiamin,
                                    Percent_survive = input$visualize_add_data_manual_survival)) |>
        dplyr::mutate(index = dplyr::row_number(),
                      Estimated_survive = dose_response(Thiamin_conc = Thiamin_conc,
                                                        ec50_mu = unique(lc50_curve$ec50_50),
                                                        slope_p = unique(lc50_curve$slope_50),
                                                        upper_p = 1,lower_p = 0)*100) |>
        dplyr::select(index, Thiamin_conc, Percent_survive, Estimated_survive)

    }) |>
      shiny::bindEvent(input$visualize_add_data_new_row, ignoreInit = TRUE)

    # This code updates the Plotly scatterplot to show the user's new data as red
    # points
    shiny::observe({

      shiny::req(filtered_data$user_data)

      # See the renderPlotly call above for the definition of this custom
      # JavaScript method:
      session$sendCustomMessage("remove-trace", "Observed user data")
      session$sendCustomMessage("remove-trace", "Estimated user data")

      plotly::plotlyProxy("ec50_curve", session, deferUntilFlush = FALSE) |>
        # Delete all layers above the top-most base layer
        # plotly::plotlyProxyInvoke(method = "deleteTraces", list(as.integer(filtered_data$max_base_layer + 1))) |>
        # Replace deleted layer(s) with new layer
        plotly::plotlyProxyInvoke("addTraces",
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
                                       type = "scatter", mode = "markers")) |>
        plotly::plotlyProxyInvoke("addTraces",
                                  # NOTE: addTraces needs at least two points, for some
                                  # reason, so we'll repeat single points twice
                                  list(x = rep(filtered_data$user_data$Thiamin_conc,
                                               length.out = max(2,length(filtered_data$user_data$Thiamin_conc))),
                                       y = rep(filtered_data$user_data$Estimated_survive,
                                               length.out = max(2,length(filtered_data$user_data$Estimated_survive))),
                                       # text = paste0("Thiamin Conc.: ",filtered_data$user_data$Thiamin_conc," nmol/g<br>",
                                       #               "Estimated % Survived: ",filtered_data$user_data$Estimated_survive,"%"),
                                       # hoverinfo = "text",
                                       # hoverlabel = list(bgcolor = "red"),
                                       hoverinfo = "none",
                                       marker = list(color = "red", size = 8, symbol = "circle-open-dot"),
                                       name = "Estimated user data",
                                       type = "scatter", mode = "markers"))
    })



    output$visualize_add_data <-
      reactable::renderReactable({

        shiny::req(filtered_data$user_data)
        shiny::req(nrow(filtered_data$user_data) > 0)
        shiny::req(filtered_data$user_data$Estimated_survive)

        filtered_data$user_data |>
          dplyr::mutate(Estimated_survive = round(Estimated_survive,2)) |>
          dplyr::rename(`Observation Number` = index,
                        `Thiamin Concentration (nmol/g)` = Thiamin_conc,
                        `Observed % Survived` = Percent_survive,
                        `Estimated % Survived` = Estimated_survive) |>
          dplyr::select(-`Observation Number`) |>
          dplyr::distinct() |>
          reactable::reactable(bordered = TRUE,
                               highlight = TRUE)

      })

    # Download handler for data template
    output$visualize_add_data_template <- shiny::downloadHandler(
      filename = function(){
        "vanishingVitamin_data_upload_template.csv"
      },
      content = function(file){
        file.copy("www/template.csv", file)
      }
    )

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

}

#' Non-exported helper function for computing dose response
#'
#' @param Thiamin_conc thiamin concentration value (nmol/g)
#' @param ec50_mu EC50 mean value
#' @param slope_p Slope parameter in dose response model
#' @param upper_p Upper limit parameter in dose response model
#' @param lower_p Lower limit parameter in dose response model
#'
#' @keywords internal
#' @noRd
dose_response <-
  function(Thiamin_conc, ec50_mu, slope_p, upper_p, lower_p = 0){
    upper_p + (lower_p - upper_p)/(1 + (Thiamin_conc/ec50_mu)**slope_p)
  }

#' @noRd
#' @keywords internal
setupWidgets <- function() {
  addResourcePath('extdata', system.file('extdata', package='vanishingVitamin'))
}
