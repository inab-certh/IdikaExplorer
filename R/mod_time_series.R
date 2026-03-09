#' time_series UI Function
#'
#' @description A shiny Module for time series visualization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_time_series_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h4("Time Series Analysis"),
        shiny::helpText("Visualize prescription trends over time by selected grouping variables")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("metric"),
          "Select Metric:",
          choices = c(
            "Prescriptions per 100K" = "prescriptions_per_100k",
            "Patients per 100K"      = "patients_per_100k",
            "Total Prescriptions"    = "total_prescriptions",
            "Unique Patients"        = "unique_patients"
          ),
          selected = "prescriptions_per_100k"
        )
      ),
      shiny::column(
        4,
        shiny::selectInput(
          ns("atc_filter"),
          "Filter by ATC Code / Group:",
          choices  = NULL,
          multiple = FALSE
        )
      ),
      shiny::column(
        4,
        shiny::checkboxInput(
          ns("show_points"),
          "Show Data Points",
          value = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("time_plot"), height = "600px")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::downloadButton(ns("download_plot"), "Download Plot (PNG)", class = "btn-primary"),
        shiny::downloadButton(ns("download_data"), "Download Data (CSV)",  class = "btn-secondary")
      )
    )
  )
}

#' time_series Server Functions
#'
#' @noRd 
mod_time_series_server <- function(id, data_reactive, grouping_vars_reactive,
                                   denominator_reactive, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── ATC filter dropdown ──────────────────────────────────────────────────
    # Reflects whatever values are in the (already-grouped) display data.
    shiny::observe({
      data <- data_reactive()
      shiny::req(nrow(data) > 0)

      atc_values <- sort(unique(data$atc_code))

      shiny::updateSelectInput(
        session, "atc_filter",
        choices  = c("All ATC Codes / Groups" = "all",
                     stats::setNames(atc_values, atc_values)),
        selected = "all"
      )
    })

    # ── Prepare time-series data ─────────────────────────────────────────────
    time_series_data <- shiny::reactive({
      data          <- data_reactive()
      grouping_vars <- grouping_vars_reactive()
      denominator_data <- denominator_reactive()

      shiny::req(nrow(data) > 0)
      shiny::req("prescription_insert_year" %in% names(data))

      # Optional ATC filter
      if (!is.null(input$atc_filter) && input$atc_filter != "all") {
        data <- data |> dplyr::filter(atc_code == input$atc_filter)
      }

      if (nrow(data) == 0) return(NULL)

      # Always group by year plus any selected variables
      group_vars <- unique(c("prescription_insert_year", grouping_vars))

      available_vars <- intersect(
        group_vars,
        c("prescription_insert_year", "age_group", "sex",
          "region", "icd10", "atc_code")
      )

      summary_data <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(available_vars))) |>
        dplyr::summarise(
          total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
          unique_patients     = sum(unique_patients,     na.rm = TRUE),
          .groups = "drop"
        )

      # Join population denominator for rate calculations
      demographic_vars <- intersect(available_vars,
                                    c("age_group", "sex", "region"))

      if (length(demographic_vars) > 0) {
        join_vars <- intersect(available_vars,
                               names(denominator_data$denominator))

        if (length(join_vars) > 0) {
          summary_data <- summary_data |>
            dplyr::left_join(denominator_data$denominator, by = join_vars)
        } else {
          summary_data$population <- sum(denominator_data$denominator$population)
        }

        summary_data <- summary_data |>
          dplyr::mutate(
            prescriptions_per_100k = (total_prescriptions / population) * 100000,
            patients_per_100k      = (unique_patients     / population) * 100000
          )
      } else {
        summary_data$prescriptions_per_100k <- NA_real_
        summary_data$patients_per_100k      <- NA_real_
      }

      summary_data
    })

    # ── Plot ─────────────────────────────────────────────────────────────────
    output$time_plot <- plotly::renderPlotly({
      data <- time_series_data()
      shiny::req(!is.null(data), nrow(data) > 0)

      metric        <- input$metric
      grouping_vars <- grouping_vars_reactive()

      line_group_vars <- setdiff(grouping_vars, "prescription_insert_year")

      if (length(line_group_vars) > 0) {
        data <- data |>
          dplyr::mutate(
            group_label = dplyr::case_when(
              length(line_group_vars) == 1 ~
                as.character(.data[[line_group_vars[1]]]),
              TRUE ~
                paste(!!!rlang::syms(line_group_vars), sep = " | ")
            )
          )
      } else {
        data$group_label <- "All Data"
      }

      metric_label <- dplyr::case_when(
        metric == "prescriptions_per_100k" ~ "Prescriptions per 100K",
        metric == "patients_per_100k"      ~ "Patients per 100K",
        metric == "total_prescriptions"    ~ "Total Prescriptions",
        metric == "unique_patients"        ~ "Unique Patients",
        TRUE                               ~ metric
      )

      if (!(metric %in% names(data))) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              title = "Cannot display this metric",
              annotations = list(
                text = paste(
                  "Rates (per 100K) require demographic grouping variables",
                  "(age, sex, or region)"
                ),
                showarrow = FALSE, xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, font = list(size = 16)
              )
            )
        )
      }

      if (all(is.na(data[[metric]]))) {
        return(
          plotly::plot_ly() |>
            plotly::layout(
              title = "Cannot calculate rates",
              annotations = list(
                text = paste(
                  "Please select at least one demographic grouping variable",
                  "(Age, Sex, or Region) to calculate rates"
                ),
                showarrow = FALSE, xref = "paper", yref = "paper",
                x = 0.5, y = 0.5, font = list(size = 16)
              )
            )
        )
      }

      atc_title_suffix <- if (!is.null(input$atc_filter) &&
                               input$atc_filter != "all") {
        paste0(" (ATC: ", input$atc_filter, ")")
      } else {
        ""
      }

      plotly::plot_ly(
        data          = data,
        x             = ~prescription_insert_year,
        y             = ~get(metric),
        color         = ~group_label,
        type          = "scatter",
        mode          = if (input$show_points) "lines+markers" else "lines",
        marker        = list(size = 8),
        line          = list(width = 2.5),
        hovertemplate = paste0(
          "<b>%{fullData.name}</b><br>",
          "Year: %{x}<br>",
          metric_label, ": %{y:,.1f}<br>",
          "<extra></extra>"
        )
      ) |>
        plotly::layout(
          title = list(
            text = paste0(metric_label, " Over Time", atc_title_suffix),
            font = list(size = 18)
          ),
          xaxis = list(
            title    = "Year",
            dtick    = 1,
            gridcolor = "#e0e0e0"
          ),
          yaxis = list(
            title    = metric_label,
            gridcolor = "#e0e0e0"
          ),
          hovermode = "closest",
          legend = list(
            title = list(
              text = if (length(line_group_vars) > 0)
                paste(line_group_vars, collapse = " | ")
              else ""
            ),
            orientation = "v", x = 1.02, y = 1
          ),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff"
        )
    })

    # ── Downloads ─────────────────────────────────────────────────────────────
    output$download_plot <- shiny::downloadHandler(
      filename = function() paste0("time_series_", input$metric, "_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- plotly::plotly_build(plotly::ggplotly(ggplot2::last_plot()))
        plotly::export(p, file = file)
      }
    )

    output$download_data <- shiny::downloadHandler(
      filename = function() paste0("time_series_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        readr::write_csv(time_series_data(), file)
      }
    )
  })
}
