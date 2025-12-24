#' raw_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_raw_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(ns("table"))
  )
}
    
#' raw_table Server Functions
#'
#' @noRd 
mod_raw_table_server <- function(id, data_reactive){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$table <- DT::renderDataTable({
      data <- data_reactive()
      print(nrow(data))
      
      # Create nice column names mapping
      column_name_map <- c(
        "prescription_insert_year" = "Year",
        "age_group" = "Age",
        "sex" = "Sex",
        "region" = "Region",
        "icd10" = "ICD-10",
        "atc_code" = "ATC Code",
        "total_prescriptions" = "Total Prescriptions",
        "unique_patients" = "Unique Patients"
      )
      
      # Rename columns to nice names
      current_names <- names(data)
      nice_names <- sapply(current_names, function(name) {
        if (name %in% names(column_name_map)) {
          column_name_map[name]
        } else {
          name
        }
      }, USE.NAMES = FALSE)
      
      names(data) <- nice_names
      
      DT::datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        filter = "top",
        rownames = FALSE
      )
    })
  })
}
    
## To be copied in the UI
# mod_raw_table_ui("raw_table_1")
    
## To be copied in the server
# mod_raw_table_server("raw_table_1")
