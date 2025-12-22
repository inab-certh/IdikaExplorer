mod_extractdata_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get distinct values for each filter on module initialization
    shiny::observe({
      shiny::req(con)
      filter_columns <- c("age_group", "sex_id", "county", "atc_code")

      filter_values <- purrr::map(
        filter_columns,
        ~ {
          DBI::dbGetQuery(
            con,
            glue::glue("SELECT DISTINCT {.x} FROM idika ORDER BY {.x}")
          )[[1]]
        }
      )
      names(filter_values) <- filter_columns

      # Update selectizeInputs using filter_values
      shiny::updateSelectizeInput(session, "age_group", choices = filter_values$age_group, server = TRUE)
      shiny::updateSelectizeInput(session, "sex_id", choices = filter_values$sex_id, server = TRUE)
      shiny::updateSelectizeInput(session, "county", choices = filter_values$county, server = TRUE)
      shiny::updateSelectizeInput(session, "atc_code", choices = filter_values$atc_code, server = TRUE)
    })

    # Reactive value to store filtered data
    filtered_data <- shiny::eventReactive(
      input$filter_btn,
      {
        shiny::req(con)
        age_group_val <- if (length(input$age_group) == 0) "all" else input$age_group
        sex_id_val <- if (length(input$sex_id) == 0) "all" else input$sex_id
        county_val <- if (length(input$county) == 0) "all" else input$county
        atc_code_val <- if (length(input$atc_code) == 0) "all" else input$atc_code

        sql_query <- generate_sql_query(
          age_group = age_group_val,
          sex_id = sex_id_val,
          county = county_val,
          atc_code = atc_code_val
        )

        DBI::dbGetQuery(con, sql_query)
      },
      ignoreNULL = FALSE
    )

    # Initial data load (all data)
    initial_data <- shiny::reactive({
      shiny::req(con)
      DBI::dbGetQuery(con, "SELECT * FROM idika LIMIT 1000")
    })

    # Render DataTable
    output$table <- DT::renderDataTable({
      data <- if (input$filter_btn > 0) {
        filtered_data()
      } else {
        initial_data()
      }
      
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

    # Reset filters - MOVED INSIDE moduleServer
    shiny::observeEvent(input$reset_btn, {
      shiny::updateSelectizeInput(session, "age_group", selected = character(0))
      shiny::updateSelectizeInput(session, "sex_id", selected = character(0))
      shiny::updateSelectizeInput(session, "county", selected = character(0))
      shiny::updateSelectizeInput(session, "atc_code", selected = character(0))
    })
  })
}
