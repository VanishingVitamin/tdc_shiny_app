#' Launch Vanishing Vitamin app
#'
#' @param options list; last directly to options argument of shiny::shinyApp()
#'
#' @seealso [shiny::shinyApp()]

vanishingVitamin <- function(options = list()){

  color_theme <-
    fresh::create_theme(
      fresh::bs4dash_vars(
        navbar_light_color = NULL,
        navbar_light_active_color = NULL,
        navbar_light_hover_color = NULL
      ),
      fresh::bs4dash_yiq(
        contrasted_threshold = 10,
        text_dark = "#000000",
        text_light = "#FFFFFF"
      ),
      bs4dash_layout(
        main_bg = NULL
      ),
      fresh::bs4dash_sidebar_light(
        bg = NULL,
        color = NULL,
        active_color = NULL,
        hover_color = NULL,
        submenu_bg = NULL,
        submenu_color = NULL,
        submenu_hover_color = NULL
      ),
      fresh::bs4dash_status(
        primary = "#B8C9FF",
        danger = NULL,
        light = NULL
      ),
      fresh::bs4dash_color(
      )
    )

  ui <-
    bs4Dash::dashboardPage(
      scrollToTop = TRUE,
      freshTheme = color_theme,
      help = NULL,
      dark = NULL,
      # controlbar = dashboardControlbar(disable = TRUE,width = 0, overlay = TRUE),
      header = bs4Dash::dashboardHeader(title = shiny::h5("Vanishing Vitamin", style = "padding-left:10px;"),
                                        sidebarIcon = shiny::icon("caret-square-left",
                                                                  style = "font-size:24px;",
                                                                  id = "header_toggle"),
                                        bs4Dash::navbarMenu(
                                          id = "navmenu",
                                          shinyjs::useShinyjs(),
                                          bs4Dash::navbarTab(tabName = "welcome", text = tags$span(icon("fish-fins"), "Welcome!")),
                                          bs4Dash::navbarTab(tabName = "data", text = tags$span(icon("table"), "Data")),
                                          bs4Dash::navbarTab(tabName = "visualize", text = tags$span(icon("chart-line"), "Visualize"))
                                        )),
      sidebar = bs4Dash::dashboardSidebar(disable = FALSE,
                                          elevation = 2,
                                          collapsed = FALSE,
                                          minified = FALSE,
                                          expandOnHover = TRUE,
                                          fixed = TRUE,
                                          # text = "Dashboard filters",
                                          # icon = shiny::icon("filter"),
                                          shiny::h6("Filter by:"),
                                          shiny::selectizeInput(inputId = "tdc_table_filter_location",
                                                                label = "Collection Location",
                                                                multiple = TRUE,
                                                                choices = c("",
                                                                            rev(unique(tdc_data$Location_label)))
                                          ),
                                          shiny::selectInput(inputId = "tdc_table_filter_species",
                                                             label = "Species",
                                                             multiple = TRUE,
                                                             choices = c("",
                                                                         rev(unique(tdc_data$Species_label)))
                                          ),
                                          shiny::selectInput(inputId = "tdc_table_filter_run",
                                                             label = "Run",
                                                             multiple = TRUE,
                                                             choices = c("",
                                                                         rev(unique(tdc_data$Run_label)))
                                          ),
                                          shiny::selectInput(inputId = "tdc_table_filter_tissue",
                                                             label = "Tissue",
                                                             multiple = TRUE,
                                                             choices = c("",
                                                                         rev(unique(tdc_data$Tissue_label)))
                                          )
                                          # ,sliderInput(inputId = "tdc_table_filter_date_range",
                                          #             label = "Date Collected",
                                          #             choices = c("",
                                          #                         unique(tdc_data$Title_label))
                                          #             )
      ),
      # dashboardSidebar(collapsed = TRUE,
      #   sidebarMenu(
      #     menuItem(text = "Welcome!", tabName = "welcome", icon = icon("fish-fins")),
      #     menuItem(text = "Data", tabName = "data", icon = icon("table")),
      #     menuItem(text = "Visualize", tabName = "visualize", icon = icon("chart-line"))
      #   )
      # ),
      body = shiny::dashboardBody(
        tags$style(type = "text/css",
                   "#tdc_data_map {height: calc(100vh - 57px) !important;
                    width: calc(80vh) !important;
                    overflow-x: hidden;
                    overflow-y: hidden;}"),
        shiny::tabItems(
          shiny::tabItem(tabName = "welcome",
                         shiny::h3("Welcome to Vanishing Vitamin!"),
                         shiny::h5("This tab helps users get started.
                 It summarizes the app's primary functionality and directs users to where they can find more information."),
                         shiny::h5("Note to future self: link to ",
                                   shiny::a(href = "https://sites.google.com/ucdavis.edu/salmonintheclassroomresources/home",
                                            target = '_blank',
                                            "https://sites.google.com/ucdavis.edu/salmonintheclassroomresources/home"),
                                   " for resources, data, and other documentation."),
                         shiny::br(),
                         shiny::h5(shiny::strong("Click on one of the buttons at the top to get started!"))),
          shiny::tabItem(tabName = "data",
                         shiny::fluidRow(column(width = 6,
                                                bs4Dash::box(width = 12,title = "Datasets",
                                                             reactable::reactableOutput(outputId = "tdc_data_table"),
                                                             collapsible = FALSE,closable = FALSE,maximizable = TRUE,
                                                             headerBorder = FALSE,solidHeader = FALSE,
                                                             style = 'height: calc(100vh - 80px); overflow-y:scroll'
                                                )),
                                         shiny::column(width = 6,
                                                       leaflet::leafletOutput("tdc_data_map",width = "100%"))
                         )
          ),
          shiny::tabItem(tabName = "visualize",
                         shiny::h5("This tab will contain EC50 curves for survival % vs. thiamin concentration."),
                         shiny::sidebarLayout(
                           sidebarPanel = shiny::div(id = "tdc_table_filter_sidebar",
                                                            class = "col-sm-2",
                                                            shiny::sidebarPanel(width = 10,
                                                                                "This sidebar will let users filter the scatterplot.")),
                           mainPanel = shiny::mainPanel(width = 10,
                                                        plotly::plotlyOutput("ec50_curve",height = "800px"))
                         ))
        )
      )
    )

  server <- function(input, output, session) {

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
                                                        dplyr::select(-c(ends_with("label"),
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
                                                          dplyr::select(-c(ends_with("label"),
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
    observe({

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
                            data = plt_data) %>%
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
        ggplot2::ggplot(aes(x = Thiamine_conc,
                            y = Percent_survive)) +
        ggplot2::geom_point() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Thiamin Concentration (nmol/g)",
                      y = "% Survived")

      plotly::ggplotly(plt)

    })

  }

  shiny::shinyApp(ui = ui, server = server, options = options)

}
