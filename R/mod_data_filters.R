#' data_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("prescription_insert_year"),
          "Prescription year:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select prescription years...")
        )
      ),
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("age_group"),
          "Age group:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select age groups...")
        )
      ),
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("sex_id"),
          "Sex:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select sex...")
        )
      ),
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("county"),
          "County:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select counties...")
        )
      ),
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("atc_code"),
          "ATC Code:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select ATC Codes...")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::actionButton(ns("filter_btn"), "Apply filters", class = "btn-primary"),
        shiny::actionButton(ns("reset_btn"), "Reset filters", class = "btn-secondary")
      )
    ),
    shiny::br()
  )
}
    
#' data_filters Server Functions
#'
#' @noRd 
mod_data_filters_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get distinct values for each filter on module initialization
    shiny::observe({
      shiny::req(con)

      filter_columns <- c(
        "prescription_insert_year",
        "age_group", "sex_id", "county", "atc_code"
      )
      filter_values <- purrr::map(
        filter_columns,
        ~ {
          result <- DBI::dbGetQuery(
            con,
            glue::glue("SELECT DISTINCT {.x} FROM idika ORDER BY {.x}")
          ) |> dplyr::pull()
          result
        }
      )
      names(filter_values) <- filter_columns

      shiny::updateSelectizeInput(
        session, "prescription_insert_year",
        choices = filter_values$prescription_insert_year
      )
      shiny::updateSelectizeInput(
        session, "age_group",
        choices = filter_values$age_group
      )
      shiny::updateSelectizeInput(
        session, "sex_id",
        choices = filter_values$sex_id
      )
      shiny::updateSelectizeInput(
        session, "county",
        choices = filter_values$county
      )
      shiny::updateSelectizeInput(
        session, "atc_code",
        choices = filter_values$atc_code
      )
    })

    filtered_data <- shiny::eventReactive(
      input$filter_btn,
      {
        shiny::req(con)

        age_group_val <- if (length(input$age_group) == 0) "all" else input$age_group
        sex_id_val <- if (length(input$sex_id) == 0) "all" else input$sex_id
        county_val <- if (length(input$county) == 0) "all" else input$county
        atc_code_val <- if (length(input$atc_code) == 0) "all" else input$atc_code
        prescription_insert_year_val <- if (length(input$prescription_insert_year) == 0) "all" else input$prescription_insert_year

        sql_query <- generate_sql_query(
          prescription_insert_year = prescription_insert_year_val,
          age_group = age_group_val,
          sex_id = sex_id_val,
          county = county_val,
          atc_code = atc_code_val
        )

        result <- DBI::dbGetQuery(con, sql_query)
        result
      },
      ignoreNULL = FALSE
    )

    initial_data <- shiny::reactive({
      shiny::req(con)
      result <- DBI::dbGetQuery(con, "SELECT * FROM idika LIMIT 1000")
      result
    })

    shiny::observeEvent(input$reset_btn, {
      shiny::updateSelectizeInput(
        session, "prescription_insert_year",
        selected = character(0)
      )
      shiny::updateSelectizeInput(
        session, "age_group",
        selected = character(0)
      )
      shiny::updateSelectizeInput(
        session, "sex_id",
        selected = character(0)
      )
      shiny::updateSelectizeInput(
        session, "county",
        selected = character(0)
      )
      shiny::updateSelectizeInput(
        session, "atc_code",
        selected = character(0)
      )
    })

    list(
      filtered_data = filtered_data,
      initial_data = initial_data,
      filter_btn = shiny::reactive(input$filter_btn)
    )

  })
}

## To be copied in the UI
# mod_data_filters_ui("data_filters_1")
    
## To be copied in the server
# mod_data_filters_server("data_filters_1")
