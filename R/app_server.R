#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  con <- get_duckdb_connection()

  session$onSessionEnded(function() {
    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  mod_extractdata_server("extractdata_1", con = con)
}
