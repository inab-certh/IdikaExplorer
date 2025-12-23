#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Database connection
  con <- get_duckdb_connection()

  session$onSessionEnded(function() {
    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  # Tab 1: Overall data with filters
  overall_data <- mod_data_filters_server("filters_overall", con = con)

  overall_display_data <- shiny::reactive({
    if (overall_data$filter_btn() > 0) {
      overall_data$filtered_data()
    } else {
      overall_data$initial_data()
    }
  })

  mod_raw_table_server("raw_table_1", data_reactive = overall_display_data)

  # Tab 2: Subgroup Analysis
  # Uses the SAME filtered data from Tab 1
  # User selects grouping variables based on what's available in that data
  
  selected_grouping_vars <- mod_grouping_selector_server(
    "grouping_selector", 
    filtered_data_reactive = overall_display_data
  )
  
  mod_summary_table_server(
    "summary_table", 
    data_reactive = overall_display_data,
    grouping_vars_reactive = selected_grouping_vars
  )
}
