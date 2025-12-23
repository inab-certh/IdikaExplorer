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
      DT::datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        filter = "top"
      )
    })
  })
}
    
## To be copied in the UI
# mod_raw_table_ui("raw_table_1")
    
## To be copied in the server
# mod_raw_table_server("raw_table_1")
