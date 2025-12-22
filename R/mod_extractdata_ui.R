mod_extractdata_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      3,
      shiny::selectizeInput(
        ns("age_group"),
        "Age group:",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Select age groups...")
      )
    ),
    shiny::column(
      3,
      shiny::selectizeInput(
        ns("sex_id"),
        "Sex:",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Select sex...")
      )
    ),
    shiny::column(
      3,
      shiny::selectizeInput(
        ns("county"),
        "County:",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Select counties...")
      )
    ),
    shiny::column(
      3,
      shiny::selectizeInput(
        ns("atc_code"),
        "ATC Code:",
        choices = NULL,
        multiple = TRUE,
        options = list(placeholder = "Select ATC Codes...")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::actionButton(ns("filter_btn"), "Apply filters", class = "btn-primary"),
        shiny::actionButton(ns("reset_btn"), "Reset filters", class = "btn-primary"),
      )
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        12,
        DT::dataTableOutput(ns("test"))
      )
    )
  )
}
