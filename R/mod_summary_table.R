mod_summary_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(ns("summary_table"))
  )
}

mod_summary_table_server <- function(id, data_reactive, grouping_vars_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$summary_table <- DT::renderDataTable({
      data <- data_reactive()
      grouping_vars <- grouping_vars_reactive()
      
      shiny::req(nrow(data) > 0)
      
      message("Computing summary statistics...")
      message("Grouping by: ", paste(grouping_vars, collapse = ", "))
      
      # If no grouping variables selected, show overall totals
      if (length(grouping_vars) == 0) {
        summary_data <- data |>
          dplyr::summarise(
            total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
            unique_patients = sum(unique_patients, na.rm = TRUE)
          ) |>
          dplyr::mutate(group = "Overall")
        
        # Reorder columns to put 'group' first
        summary_data <- summary_data |>
          dplyr::select(group, dplyr::everything())
        
      } else {
        # Group by selected variables
        summary_data <- data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
          dplyr::summarise(
            total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
            unique_patients = sum(unique_patients, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::arrange(dplyr::desc(total_prescriptions))
      }
      
      message("Summary table has ", nrow(summary_data), " rows")
      
      DT::datatable(
        summary_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        filter = "top",
        rownames = FALSE
      ) |>
        DT::formatRound(columns = c("total_prescriptions", "unique_patients"), digits = 0)
    })
  })
}
