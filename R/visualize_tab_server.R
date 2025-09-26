visualize_tab_server <- function(input, output, session, filtered_data, dose_response_params){

  ### Visualize tab code

  # we'll plot an EC50 curve for each species the user has selected.
  # if the user hasn't selected any species, we'll plot all 5 curves
  shiny::observe({
    #req(input$tdc_table_filter_species)

    thiamine_conc_seq <- sort(unique(c(
      tdc_data$Thiamine_conc,
      seq(from = 0, to = 30, by = 0.05),
      1:30
    )))

    if (is.null(input$tdc_table_filter_species)) {
      species <- c(
        "ATLANTIC SALMON",
        "CHINOOK SALMON",
        "COHO SALMON",
        "LAKE TROUT",
        "STEELHEAD TROUT"
      )
      region <- c(
        "overall",
        "overall",
        "greatLakes",
        "greatLakes",
        "greatLakes"
      )
    } else {
      species <- input$tdc_table_filter_species
      region <- dplyr::case_when(
        species == "ATLANTIC SALMON" ~ "overall",
        species == "CHINOOK SALMON" ~ "overall",
        species == "COHO SALMON" ~ "greatLakes",
        species == "LAKE TROUT" ~ "greatLakes",
        species == "STEELHEAD TROUT" ~ "greatLakes"
      )
    }

    # compute the dose response for each selected species, at each thiamine conc value
    filtered_data$dose_response_curve <-
      purrr::map2_dfr(species, region, function(spec, reg) {
        params <- dose_response_params |>
          dplyr::filter(
            toupper(species) == spec &
              region == reg &
              parameter != "ec50_sigma"
          )

        tibble::tibble(
          Thiamine_conc = thiamine_conc_seq,
          survival_median = do.call(
            dose_response,
            c(
              list(Thiamine_conc = thiamine_conc_seq),
              as.list(stats::setNames(params$median, params$parameter))
            )
          ),
          survival_ci_2.5 = do.call(
            dose_response,
            c(
              list(Thiamine_conc = thiamine_conc_seq),
              as.list(stats::setNames(params$CI_2.5, params$parameter))
            )
          ),
          survival_ci_97.5 = do.call(
            dose_response,
            c(
              list(Thiamine_conc = thiamine_conc_seq),
              as.list(stats::setNames(params$CI_97.5, params$parameter))
            )
          )
        ) |>
          dplyr::mutate(
            species = spec,
            plot_label = paste0(
              "Species: ",species,"</br>",
              "Thiamine Conc: ",
              Thiamine_conc,
              " nmol/g\n",
              "Estimated % Survived: ",
              round(survival_median*100, 2),
              "%"
            )
          )
      })
  })

  # Render interactive scatterplot
  output$ec50_curve <-
    plotly::renderPlotly({
      shiny::req(filtered_data$dose_response_curve)

      # Prepare the TDC data for plotting. This includes creating a label for
      # each point that will appear when hovering over the point.
      plt_data <-
        filtered_data$tdc_data |>
        dplyr::group_by(Thiamine_conc) |>
        dplyr::arrange(dplyr::desc(Percent_survive)) |>
        dplyr::group_by(Thiamine_conc) |>
        dplyr::mutate(
          ind = dplyr::row_number(),
          plot_label = paste0(
            "Species: ", Species_label,"</br>",
            "Thiamine Conc: ",
            Thiamine_conc,
            " nmol/g\n",
            "Observed % Survived: ",
            round(Percent_survive, 2),
            "%"
          )
        ) |>
        dplyr::ungroup()

      # The first "layer" of the plot will be a model-based approximate 95%
      # credible band
      plt <- plotly::plot_ly() |>
        plotly::add_ribbons(
          data = filtered_data$dose_response_curve,
          x = ~Thiamine_conc,
          ymin = ~ survival_ci_2.5 * 100,
          ymax = ~ survival_ci_97.5 * 100,
          name = "Survival 95% C.I.",
          hoverinfo = "skip",
          fillcolor = ~species,
          color = ~species,
          opacity = .3,
          line = list(color = ~species, opacity = .1),
          colors = c(
            "ATLANTIC SALMON" = "#648FFF",
            "CHINOOK SALMON" = "#DD2680",
            "COHO SALMON" = "#FFB001",
            "LAKE TROUT" = "#FE6100",
            "STEELHEAD TROUT" = "#775EF0"
          ),
          showlegend = FALSE
        ) |>
        # The second layer of the plot is a fitted median survival curve.
        plotly::add_lines(
          data = filtered_data$dose_response_curve,
          x = ~Thiamine_conc,
          y = ~ survival_median * 100,
          text = ~plot_label,
          name = ~species,
          hoverinfo = "text",
          hoverlabel = list(bgcolor = "#e5e5e5"),
          color = ~species,
          colors = c(
            "ATLANTIC SALMON" = "#648FFF",
            "CHINOOK SALMON" = "#DD2680",
            "COHO SALMON" = "#FFB001",
            "LAKE TROUT" = "#FE6100",
            "STEELHEAD TROUT" = "#775EF0"
          ),
          opacity = 0.7,
          showlegend = TRUE
        )

      # Next add the points ("markers"). We want the user to hover over a
      # particular Thiamine Concentration "x" value and see (1) all observed %
      # Survived "y" values for each x.

      # Note that there are some observations that share a common Thiamine
      # Concentration value but have different Survival % values. These would
      # appear as points that a share a common x-value in the plot. A quirk of plotly in this
      # situation is that it will only show *one* of the hoverable labels for each
      # Thiamine Conc value *unless* they are plotted as separate "layers". The for
      # loop creates multiple layers of points to ensure that all point labels
      # will show when hovering.
      for (trace_ind in unique(plt_data$ind)) {
        plt <- plt |>
          plotly::add_markers(
            data = {
              plt_data |> dplyr::filter(ind == trace_ind)
            },
            x = ~Thiamine_conc,
            y = ~Percent_survive,
            name = paste0("obs_survive_layer", trace_ind),
            text = ~plot_label,
            hoverinfo = "text",
            color = ~Species_label,
            colors = c(
              "ATLANTIC SALMON" = "#648FFF",
              "CHINOOK SALMON" = "#DD2680",
              "COHO SALMON" = "#FFB001",
              "LAKE TROUT" = "#FE6100",
              "STEELHEAD TROUT" = "#775EF0"
            ),
            sizes = 2,
            opacity = 1,
            showlegend = FALSE
          )
      }

      # The last layer in the plotly object should contain the user-uploaded
      # data. We'd like to keep track of which layer index this corresponds
      # to (starting from 0)
      filtered_data$max_base_layer <- length(plt$x$attrs) - 1

      if(!is.null(filtered_data$user_data)){
        user_dat <- filtered_data$user_data |>
          dplyr::filter(all(input$tdc_table_filter_species == "") |
                          species %in% input$tdc_table_filter_species)

        if (!is.null(user_dat)) {
          if (nrow(user_dat) > 0) {
            if (nrow(user_dat) == 1) {
              user_dat_plt <- user_dat |>
                dplyr::slice(1, 1)
            } else {
              user_dat_plt <- user_dat
            }

            user_dat_plt <-
              user_dat_plt |>
              dplyr::mutate(
                plt_label_observed = paste0(
                  "Species: ",species,"</br>",
                  "Thiamine Conc.: ",
                  Thiamine_conc,
                  " nmol/g<br>",
                  "Observed % Survived: ",
                  Percent_survive,
                  "%"
                ),
                plt_label_estimated = paste0(
                  "Species: ",species,"</br>",
                  "Thiamine Conc.: ",
                  Thiamine_conc,
                  " nmol/g<br>",
                  "Estimated % Survived: ",
                  round(Estimated_survive, 2),
                  "%"
                )
              )

            plt <- plt |>
              plotly::add_markers(
                data = user_dat_plt,
                x = ~Thiamine_conc,
                y = ~Percent_survive,
                text = ~plt_label_observed,
                hoverinfo = "text",
                hoverlabel = list(bgcolor = "black"),
                marker = list(color = "black", size = 8),
                name = "Observed user data"
              ) |>
              plotly::add_markers(
                data = user_dat_plt,
                x = ~Thiamine_conc,
                y = ~Estimated_survive,
                text = ~plt_label_estimated,
                hoverinfo = "skip",
                # hoverlabel = list(bgcolor = "black"),
                marker = list(
                  color = "black",
                  size = 8,
                  symbol = "circle-open-dot"
                ),
                name = "Estimated user data"
              )
          }
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
          xaxis = list(
            title = "Thiamine Concentration (nmol/g)",
            showspikes = TRUE,
            spikesnap = "hovered data",
            spikemode = "toaxis+across",
            spikedash = "dot",
            spikethickness = 1,
            range = c(
              -1,
              30
              # max(plt_data$Thiamine_conc, na.rm = TRUE) + 1
            ),
            tickmode = "dynamic"
          ),
          yaxis = list(title = "% Survived", range = c(-3, 103)),
          # hovermode = "x",
          hovermode = "closest",
          hoverdistance = 10,
          showlegend = TRUE
        ) |>
        # plotly::style(
        #   hoverinfo = "skip",
        #   traces = c(0,1,2)
        # ) |>
        ## The following commented-out code would add the ability to switch
        ## between a linear vs. log scale x-axis. This looks a bit awkward
        ## since there are a non-trivial number of thiamine concentrations
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
        htmlwidgets::onRender(remove_trace_js) |>
        plotly::config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("lasso2d", "autoscale2d", "select2d")
        )
    })

  ## NOTE 2025-09-24: I'm commenting the following bit of code out as I'm not
  ## sure we'll need anything other than the manual entry option (the
  ## default). Also, I didn't want to take the time to add the logic for
  ## checking the Species of the user-uploaded data.

  ## This code controls how data are uploaded to the app. The use can upload a
  ## csv or xlsx file to the app. If they choose this option, they must indicate
  ## which columns in the data set contain the Thiamine Concentration and
  ## (optionally) the % Survived data.

  # shiny::observe({
  #   ext <- tools::file_ext(input$visualize_add_data_file$datapath)
  #
  #   if (ext == "csv") {
  #     filtered_data$user_uploaded_data <- readr::read_csv(
  #       input$visualize_add_data_file$datapath
  #     )
  #   } else if (ext == "xlsx") {
  #     filtered_data$user_uploaded_data <- readxl::read_xlsx(
  #       input$visualize_add_data_file$datapath
  #     )
  #   } else {
  #     shiny::showNotification(
  #       ui = "Unable to read the file. Make sure it is either .csv or .xlsx",
  #       type = "error"
  #     )
  #     shiny::req(FALSE)
  #   }
  #
  #   shinyjs::show(id = "visualize_add_data_file_panel")
  #
  #   shiny::updateSelectInput(
  #     inputId = "visualize_add_data_file_thiamine_col",
  #     choices = c("", names(filtered_data$user_uploaded_data))
  #   )
  #   shiny::updateSelectInput(
  #     inputId = "visualize_add_data_file_survive_col",
  #     choices = c("", names(filtered_data$user_uploaded_data))
  #   )
  # }) |>
  #   shiny::bindEvent(input$visualize_add_data_file, ignoreInit = TRUE)
  #
  # # Once the user indicates which columns in the uploaded data set correspond to
  # # Thiamine Concentration and (optionally) % Survived,
  # shiny::observe({
  #   req(input$visualize_add_data_file_thiamine_col)
  #
  #   plot_data <-
  #     filtered_data$user_uploaded_data |>
  #     dplyr::select(dplyr::any_of(c(
  #       input$visualize_add_data_file_thiamine_col,
  #       input$visualize_add_data_file_survive_col
  #     )))
  #
  #   if (
  #     ncol(plot_data) == 1 | input$visualize_add_data_file_survive_col == ""
  #   ) {
  #     plot_data <- plot_data |>
  #       dplyr::mutate(Percent_survive = NA)
  #   }
  #
  #   # Check that Thiamine concentration column has no missing values
  #   if (any(is.na(plot_data[[input$visualize_add_data_file_thiamine_col]]))) {
  #     shiny::showNotification(
  #       ui = "Something is wrong with the Thiamine Concentration column. It should only contain numeric values. Check your data and try again.",
  #       type = "error"
  #     )
  #     shiny::req(FALSE)
  #   }
  #
  #   plot_data <- plot_data |>
  #     stats::setNames(c("Thiamine_conc", "Percent_survive")) |>
  #     dplyr::mutate(
  #       Thiamine_conc = as.numeric(Thiamine_conc),
  #       Percent_survive = as.numeric(Percent_survive)
  #     )
  #
  #   # check that Percent_surive column is either all NA or is a valid percent
  #   if (
  #     !all(
  #       is.na(plot_data$Percent_survive) |
  #         (plot_data$Percent_survive > 0 & plot_data$Percent_survive < 100)
  #     )
  #   ) {
  #     shiny::showNotification(
  #       ui = "Something is wrong with the Percent Survive column. It can contain either percent values (between 0% and 100%) or 'NA' missingvalues. Check your data and try again.",
  #       type = "error"
  #     )
  #     shiny::req(FALSE)
  #   }
  #
  #   filtered_data$user_data <-
  #     dplyr::bind_rows(filtered_data$user_data, plot_data) |>
  #     dplyr::mutate(
  #       index = dplyr::row_number(),
  #       Estimated_survive = dose_response(
  #         Thiamine_conc = Thiamine_conc,
  #         ec50_mu = unique(lc50_curve$ec50_50),
  #         slope_p = unique(lc50_curve$slope_50),
  #         upper_p = 1,
  #         lower_p = 0
  #       ) *
  #         100
  #     ) |>
  #     dplyr::select(index, species, Thiamine_conc, Percent_survive, Estimated_survive)
  # }) |>
  #   shiny::bindEvent(input$visualize_add_data_upload, ignoreInit = TRUE)

  ## Another option is for the user to copy + paste data into a box. We'll make
  ## some simple assumptions including that the first row contains column names.
  # shiny::observe({
  #   shiny::req(input$visualize_add_data_clipboard)
  #
  #   filtered_data$user_clipboard_data <-
  #     utils::read.csv(
  #       text = input$visualize_add_data_clipboard,
  #       sep = "",
  #       header = TRUE
  #     )
  #
  #   shinyjs::show(id = "visualize_add_data_clipboard_panel")
  #
  #   shiny::updateSelectInput(
  #     inputId = "visualize_add_data_clipboard_thiamine_col",
  #     choices = c("", names(filtered_data$user_clipboard_data))
  #   )
  #   shiny::updateSelectInput(
  #     inputId = "visualize_add_data_clipboard_survive_col",
  #     choices = c("", names(filtered_data$user_clipboard_data))
  #   )
  # })
  #
  # shiny::observe({
  #   req(input$visualize_add_data_clipboard_thiamine_col)
  #
  #   plot_data <-
  #     filtered_data$user_clipboard_data |>
  #     dplyr::select(dplyr::any_of(c(
  #       input$visualize_add_data_clipboard_thiamine_col,
  #       input$visualize_add_data_clipboard_survive_col
  #     )))
  #
  #   if (
  #     ncol(plot_data) == 1 |
  #       input$visualize_add_data_clipboard_survive_col == ""
  #   ) {
  #     plot_data <- plot_data |>
  #       dplyr::mutate(Percent_survive = NA)
  #   }
  #
  #   if (
  #     any(is.na(plot_data[[input$visualize_add_data_clipboard_thiamine_col]]))
  #   ) {
  #     shiny::showNotification(
  #       ui = "Something is wrong with the Thiamine Concentration column. It should only contain numeric values. Check your data and try again.",
  #       type = "error"
  #     )
  #     shiny::req(FALSE)
  #   }
  #
  #   plot_data <- plot_data |>
  #     stats::setNames(c("Thiamine_conc", "Percent_survive")) |>
  #     dplyr::mutate(
  #       Thiamine_conc = as.numeric(Thiamine_conc),
  #       Percent_survive = as.numeric(Percent_survive)
  #     )
  #
  #   if (
  #     !all((plot_data$Percent_survive > 0 & plot_data$Percent_survive < 100))
  #   ) {
  #     shiny::showNotification(
  #       ui = "Something is wrong with the Percent Survive column. It can contain either percent values (between 0% and 100%) or 'NA' missingvalues. Check your data and try again.",
  #       type = "error"
  #     )
  #     shiny::req(FALSE)
  #   }
  #
  #   filtered_data$user_data <-
  #     dplyr::bind_rows(filtered_data$user_data, plot_data) |>
  #     dplyr::mutate(
  #       index = dplyr::row_number(),
  #       Estimated_survive = dose_response(
  #         Thiamine_conc = Thiamine_conc,
  #         ec50_mu = unique(lc50_curve$ec50_50),
  #         slope_p = unique(lc50_curve$slope_50),
  #         upper_p = 1,
  #         lower_p = 0
  #       ) *
  #         100
  #     ) |>
  #     dplyr::select(index, species, Thiamine_conc, Percent_survive, Estimated_survive)
  # }) |>
  #   shiny::bindEvent(input$visualize_add_data_clipboard_button)

  # Another option is for the user to manually enter Thiamine Concentration
  # and (optionally) % Survived data. We'll add a few checks here to ensure
  # the information added satisfies some basic assumptions.
  shiny::observe({
    if (
      is.na(input$visualize_add_data_manual_thiamin) |
      is.null(input$visualize_add_data_manual_thiamin)
    ) {
      shiny::showNotification(
        ui = "Thiamine concentration must be a number greater than 0.",
        type = "error"
      )
      shiny::req(FALSE)
    } else if (input$visualize_add_data_manual_thiamin <= 0) {
      shiny::showNotification(
        ui = "Thiamine concentration must be a number greater than 0.",
        type = "error"
      )
      shiny::req(FALSE)
    }

    if (is.null(input$visualize_add_data_manual_species) | input$visualize_add_data_manual_species == "") {
      shiny::showNotification(
        ui = "You must select a species to obtain an estimate",
        type = "error"
      )
      shiny::req(FALSE)
    }

    selected_ec50_mu <- dose_response_params |>
      dplyr::filter(
        parameter == "ec50_mu" &
          species == input$visualize_add_data_manual_species &
          ((input$visualize_add_data_manual_species %in%
              c("ATLANTIC SALMON", "CHINOOK SALMON") &
              region == "overall") |
             ((input$visualize_add_data_manual_species %in%
                 c("COHO SALMON", "LAKE TROUT", "STEELHEAD TROUT") &
                 region == "greatLakes")))
      ) |>
      dplyr::pull(median)

    selected_slope_p <- dose_response_params |>
      dplyr::filter(
        parameter == "slope_p" &
          species == input$visualize_add_data_manual_species &
          ((input$visualize_add_data_manual_species %in%
              c("ATLANTIC SALMON", "CHINOOK SALMON") &
              region == "overall") |
             ((input$visualize_add_data_manual_species %in%
                 c("COHO SALMON", "LAKE TROUT", "STEELHEAD TROUT") &
                 region == "greatLakes")))
      ) |>
      dplyr::pull(median)

    selected_upper_p <- dose_response_params |>
      dplyr::filter(
        parameter == "upper_p" &
          species == input$visualize_add_data_manual_species &
          ((input$visualize_add_data_manual_species %in%
              c("ATLANTIC SALMON", "CHINOOK SALMON") &
              region == "overall") |
             ((input$visualize_add_data_manual_species %in%
                 c("COHO SALMON", "LAKE TROUT", "STEELHEAD TROUT") &
                 region == "greatLakes")))
      ) |>
      dplyr::pull(median)

    filtered_data$user_data <-
      dplyr::bind_rows(
        filtered_data$user_data,
        data.frame(
          species = input$visualize_add_data_manual_species,
          Thiamine_conc = input$visualize_add_data_manual_thiamin,
          Percent_survive = input$visualize_add_data_manual_survival
        )
      ) |>
      dplyr::mutate(
        index = dplyr::row_number(),
        Estimated_survive = 100 * dose_response(
          Thiamine_conc = Thiamine_conc,
          ec50_mu = selected_ec50_mu,
          slope_p = selected_slope_p,
          upper_p = selected_upper_p,
          lower_p = 0
        )
      ) |>
      dplyr::select(index, species, Thiamine_conc, Percent_survive, Estimated_survive)
  }) |>
    shiny::bindEvent(input$visualize_add_data_new_row, ignoreInit = TRUE)

  ## NOTE 2025-09-24: I've commented out the following plotlyProxy updating as
  ## I was running into logic issues with updating the plotly object when the
  ## user changes the filters in the sidebar. Instead, the plotly object is
  ## re-rendered each time new data are added or new filters are applied,
  ## which is slightly slower than using plotlyProxy (which just adds points
  ## to the currently rendered plotly), but simpler overall.

  ## This code updates the Plotly scatterplot to show the user's new data as red
  ## points
  # shiny::observe({
  #   shiny::req(filtered_data$user_data)
  #
  #   # See the renderPlotly call above for the definition of this custom
  #   # JavaScript method:
  #   session$sendCustomMessage("remove-trace", "Observed user data")
  #   session$sendCustomMessage("remove-trace", "Estimated user data")
  #
  #   # only show estimates for user-filtered
  #   if(all(is.null(input$tdc_table_filter_species) | input$tdc_table_filter_species == "")) {
  #     data_to_plot <- filtered_data$user_data
  #   } else{
  #     data_to_plot <- filtered_data$user_data |>
  #       dplyr::filter(species %in% input$tdc_table_filter_species)
  #   }
  #
  #   browser()
  #
  #   plotly::plotlyProxy("ec50_curve", session, deferUntilFlush = FALSE) |>
  #     # Delete all layers above the top-most base layer
  #     # plotly::plotlyProxyInvoke(method = "deleteTraces", list(as.integer(filtered_data$max_base_layer + 1))) |>
  #     # Replace deleted layer(s) with new layer
  #     plotly::plotlyProxyInvoke("addTraces",
  #                               # NOTE: addTraces needs at least two points, for some
  #                               # reason, so we'll repeat single points twice
  #                               list(x = rep(data_to_plot$Thiamine_conc,
  #                                            length.out = max(2,length(data_to_plot$Thiamine_conc))),
  #                                    y = rep(as.numeric(data_to_plot$Percent_survive),
  #                                            length.out = max(2,length(data_to_plot$Percent_survive))),
  #                                    text = paste0("Thiamine Conc.: ",data_to_plot$Thiamine_conc," nmol/g<br>",
  #                                                  "Observed % Survived: ",data_to_plot$Percent_survive,"%"),
  #                                    hoverinfo = "text",
  #                                    hoverlabel = list(bgcolor = "black"),
  #                                    marker = list(color = "black", size = 8),
  #                                    name = "Observed user data",
  #                                    type = "scatter", mode = "markers")) |>
  #     plotly::plotlyProxyInvoke("addTraces",
  #                               # NOTE: addTraces needs at least two points, for some
  #                               # reason, so we'll repeat single points twice
  #                               list(x = rep(data_to_plot$Thiamine_conc,
  #                                            length.out = max(2,length(data_to_plot$Thiamine_conc))),
  #                                    y = rep(as.numeric(data_to_plot$Estimated_survive),
  #                                            length.out = max(2,length(data_to_plot$Estimated_survive))),
  #                                    text = paste0("Species: ",data_to_plot$species,"</br>",
  #                                                  "Thiamin Conc.: ",data_to_plot$Thiamine_conc," nmol/g<br>",
  #                                                  "Estimated % Survived: ",round(data_to_plot$Estimated_survive, 2),"%"),
  #                                    hoverinfo = "text",
  #                                    hoverlabel = list(bgcolor = "#e5e5e5"),
  #                                    # hoverlabel = list(bgcolor = "red"),
  #                                    # hoverinfo = "none",
  #                                    marker = list(color = "black", size = 8, symbol = "circle-open-dot"),
  #                                    name = "Estimated user data",
  #                                    type = "scatter", mode = "markers"))
  # })

  output$visualize_add_data <-
    reactable::renderReactable({
      shiny::req(filtered_data$user_data)
      shiny::req(nrow(filtered_data$user_data) > 0)
      shiny::req(filtered_data$user_data$Estimated_survive)

      filtered_data$user_data |>
        dplyr::rename(
          `Observation Number` = index,
          Species = species,
          `Thiamine Concentration (nmol/g)` = Thiamine_conc,
          `Observed % Survived` = Percent_survive,
          `Estimated % Survived` = Estimated_survive
        ) |>
        dplyr::select(-`Observation Number`) |>
        dplyr::distinct() |>
        reactable::reactable(bordered = TRUE, highlight = TRUE, compact = TRUE,
                             columns = list(
                               "Estimated % Survived" = reactable::colDef(format = reactable::colFormat(digits = 2))
                             ))
    })

  # show Download button once the user has uploaded data
  shiny::observe({
    shiny::req(nrow(filtered_data$user_data) > 0)

    shinyjs::show("visualize_data_download")
  })

}
