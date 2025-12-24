#' code_lookup UI Function
#'
#' @description A shiny Module for looking up code definitions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_code_lookup_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tabsetPanel(
      id = ns("code_tabs"),
      type = "tabs",
      
      # ATC Codes Tab
      shiny::tabPanel(
        title = "ATC Codes",
        value = "atc",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::h4("Search ATC Codes"),
            shiny::helpText("Search by ATC code or drug name to find definitions, DDD values, and administration routes")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::textInput(
              ns("atc_search"),
              "Search:",
              placeholder = "Enter ATC code (e.g., A10BA02) or drug name (e.g., metformin)..."
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("atc_level"),
              "Filter by Level:",
              choices = c(
                "All Levels" = "all",
                "Level 1 (Anatomical)" = "1",
                "Level 2 (Therapeutic)" = "2",
                "Level 3 (Pharmacological)" = "3",
                "Level 4 (Chemical)" = "4",
                "Level 5 (Substance)" = "5"
              ),
              selected = "all"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            DT::dataTableOutput(ns("atc_table"))
          )
        )
      ),
      
      # ICD-10 Codes Tab
      shiny::tabPanel(
        title = "ICD-10 Codes",
        value = "icd10",
        shiny::br(),
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::h4("Search ICD-10 Codes"),
            shiny::helpText("Search by ICD-10 code or diagnosis description to find disease classifications")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::textInput(
              ns("icd10_search"),
              "Search:",
              placeholder = "Enter ICD-10 code (e.g., E11) or diagnosis (e.g., diabetes)..."
            )
          ),
          shiny::column(
            3,
            shiny::selectInput(
              ns("icd10_type"),
              "Code Type:",
              choices = c(
                "All Types" = "all",
                "Valid Codes Only" = "valid",
                "Headers Only" = "header"
              ),
              selected = "valid"
            )
          ),
          shiny::column(
            3,
            shiny::checkboxInput(
              ns("icd10_short_desc"),
              "Use Short Descriptions",
              value = FALSE
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            DT::dataTableOutput(ns("icd10_table"))
          )
        )
      )
    )
  )
}

#' code_lookup Server Functions
#'
#' @noRd 
mod_code_lookup_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression for ATC data
    atc_data <- shiny::reactive({
      shiny::req(con)
      
      search_term <- input$atc_search
      level_filter <- input$atc_level
      
      # Base query
      query <- "SELECT atc_code, atc_name, ddd, uom, adm_r, note FROM atc_codes"
      
      conditions <- c()
      params <- list()
      
      # Add search filter
      if (!is.null(search_term) && nchar(search_term) >= 2) {
        conditions <- c(conditions, 
                       "(LOWER(atc_code) LIKE LOWER(?) OR LOWER(atc_name) LIKE LOWER(?))")
        search_pattern <- paste0("%", search_term, "%")
        params <- c(params, search_pattern, search_pattern)
      }
      
      # Add level filter
      if (level_filter != "all") {
        level_num <- as.integer(level_filter)
        conditions <- c(conditions, "LENGTH(atc_code) = ?")
        params <- c(params, level_num)
      }
      
      # Combine conditions
      if (length(conditions) > 0) {
        query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
      }
      
      # Add ordering
      query <- paste(query, "ORDER BY atc_code LIMIT 1000")
      
      # Execute query
      if (length(params) > 0) {
        result <- DBI::dbGetQuery(con, query, params = params)
      } else {
        result <- DBI::dbGetQuery(con, query)
      }
      
      result
    })
    
    # Reactive expression for ICD-10 data
    icd10_data <- shiny::reactive({
      shiny::req(con)
      
      search_term <- input$icd10_search
      type_filter <- input$icd10_type
      use_short <- input$icd10_short_desc
      
      # Determine which description column to use
      desc_col <- if (use_short) "short_description" else "long_description"
      
      # Base query
      query <- glue::glue("SELECT code, {desc_col} as description, code_type, is_valid_code FROM icd_10_cm")
      
      conditions <- c()
      params <- list()
      
      # Add search filter
      if (!is.null(search_term) && nchar(search_term) >= 2) {
        conditions <- c(conditions, 
                       "(LOWER(code) LIKE LOWER(?) OR LOWER(short_description) LIKE LOWER(?) OR LOWER(long_description) LIKE LOWER(?))")
        search_pattern <- paste0("%", search_term, "%")
        params <- c(params, search_pattern, search_pattern, search_pattern)
      }
      
      # Add type filter
      if (type_filter != "all") {
        conditions <- c(conditions, "code_type = ?")
        params <- c(params, type_filter)
      }
      
      # Combine conditions
      if (length(conditions) > 0) {
        query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
      }
      
      # Add ordering
      query <- paste(query, "ORDER BY order_number LIMIT 1000")
      
      # Execute query
      if (length(params) > 0) {
        result <- DBI::dbGetQuery(con, query, params = params)
      } else {
        result <- DBI::dbGetQuery(con, query)
      }
      
      result
    })
    
    # Render ATC table
    output$atc_table <- DT::renderDataTable({
      data <- atc_data()
      
      # Create nice column names
      column_names <- c(
        "ATC Code",
        "Name",
        "DDD",
        "Unit",
        "Route",
        "Note"
      )
      
      names(data) <- column_names
      
      DT::datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(width = '100px', targets = 0),  # ATC Code
            list(width = '300px', targets = 1),  # Name
            list(width = '80px', targets = 2),   # DDD
            list(width = '60px', targets = 3),   # Unit
            list(width = '80px', targets = 4),   # Route
            list(width = '200px', targets = 5)   # Note
          )
        ),
        extensions = 'Buttons',
        filter = "top",
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      ) |>
        DT::formatStyle(
          'ATC Code',
          fontWeight = 'bold',
          color = DT::styleInterval(
            cuts = c(2, 4, 5, 7),  # Based on string length
            values = c('#2c3e50', '#34495e', '#7f8c8d', '#95a5a6', '#000000')
          )
        )
    })
    
    # Render ICD-10 table
    output$icd10_table <- DT::renderDataTable({
      data <- icd10_data()
      
      # Create nice column names
      column_names <- c(
        "ICD-10 Code",
        "Description",
        "Type",
        "Valid"
      )
      
      names(data) <- column_names
      
      # Convert logical to readable text
      data$Valid <- ifelse(data$Valid, "Yes", "No")
      
      DT::datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(width = '100px', targets = 0),  # ICD-10 Code
            list(width = '400px', targets = 1),  # Description
            list(width = '80px', targets = 2),   # Type
            list(width = '60px', targets = 3),   # Valid
            list(className = 'dt-center', targets = c(2, 3))  # Center align Type and Valid
          )
        ),
        extensions = 'Buttons',
        filter = "top",
        rownames = FALSE,
        class = 'cell-border stripe hover compact'
      ) |>
        DT::formatStyle(
          'ICD-10 Code',
          fontWeight = 'bold',
          color = DT::styleInterval(
            cuts = c(3.5, 4.5, 5.5),  # Based on code length
            values = c('#2c3e50', '#34495e', '#7f8c8d', '#95a5a6')
          )
        ) |>
        DT::formatStyle(
          'Type',
          backgroundColor = DT::styleEqual(
            c('valid', 'header'),
            c('#d5f4e6', '#fff3cd')  # Green for valid, yellow for header
          )
        ) |>
        DT::formatStyle(
          'Valid',
          backgroundColor = DT::styleEqual(
            c('Yes', 'No'),
            c('#d5f4e6', '#f8d7da')  # Green for Yes, light red for No
          )
        )
    })
  })
}

## To be copied in the UI
# mod_code_lookup_ui("code_lookup_1")

## To be copied in the server
# mod_code_lookup_server("code_lookup_1")
