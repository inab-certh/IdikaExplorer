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
          ns("sex"),
          "Sex:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select sex...")
        )
      ),
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("region"),
          "Region:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select regions...")
        )
      ),
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("icd10"),
          "ICD-10:",
          choices = NULL,
          multiple = TRUE,
          # create = TRUE allows typing arbitrary values (including wildcards)
          options = list(
            placeholder = "Select or type ICD-10 codes...",
            create = TRUE,
            createOnBlur = TRUE,
            persist = FALSE
          )
        ),
        shiny::tags$small(
          class = "text-muted",
          shiny::icon("circle-info", style = "font-size:0.8em;"),
          " Use * as wildcard, e.g. ",
          shiny::tags$code("E11*")
        )
      ),
      shiny::column(
        2,
        shiny::selectizeInput(
          ns("atc_code"),
          "ATC Code:",
          choices = NULL,
          multiple = TRUE,
          # create = TRUE allows typing arbitrary values (including wildcards)
          options = list(
            placeholder = "Select or type ATC codes...",
            create = TRUE,
            createOnBlur = TRUE,
            persist = FALSE
          )
        ),
        shiny::tags$small(
          class = "text-muted",
          shiny::icon("circle-info", style = "font-size:0.8em;"),
          " Use * as wildcard, e.g. ",
          shiny::tags$code("N06AB*")
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

      filter_tables <- c("age_group", "sex", "region")
      values_1 <- purrr::map(
        filter_tables,
        ~ {
          result <- DBI::dbGetQuery(
            con,
            glue::glue("SELECT DISTINCT {.x}_name FROM {.x} ORDER BY {.x}_name")
          ) |> dplyr::pull()
          result
        }
      )

      names(values_1) <- filter_tables

      filter_columns <- c(
        "prescription_insert_year", "icd10", "atc_code"
      )

      values_2 <- purrr::map(
        filter_columns,
        ~ {
          result <- DBI::dbGetQuery(
            con,
            glue::glue("SELECT DISTINCT {.x} FROM idika ORDER BY {.x}")
          ) |> dplyr::pull()
          result
        }
      )
      names(values_2) <- filter_columns

      filter_values <- c(values_1, values_2)

      shiny::updateSelectizeInput(
        session, "prescription_insert_year",
        choices = filter_values$prescription_insert_year
      )
      shiny::updateSelectizeInput(
        session, "age_group",
        choices = filter_values$age_group
      )
      shiny::updateSelectizeInput(
        session, "sex",
        choices = filter_values$sex
      )
      shiny::updateSelectizeInput(
        session, "region",
        choices = filter_values$region
      )
      shiny::updateSelectizeInput(
        session, "icd10",
        choices = filter_values$icd10
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

        if (length(input$age_group) == 0) {
          age_group_val <- "all"
        } else {
          age_group_val <- get_field_ids(con, "age_group", input$age_group)
        }

        if (length(input$sex) == 0) {
          sex_val <- "all"
        } else {
          sex_val <- get_field_ids(con, "sex", input$sex)
        }

        if (length(input$region) == 0) {
          region_val <- "all"
        } else {
          region_val <- get_field_ids(con, "region", input$region)
        }

        # ICD-10 and ATC accept plain values AND wildcard patterns.
        # Wildcards are passed through as-is; build_condition() handles them.
        icd10_val     <- if (length(input$icd10) == 0)     "all" else input$icd10
        atc_code_val  <- if (length(input$atc_code) == 0)  "all" else input$atc_code
        prescription_insert_year_val <-
          if (length(input$prescription_insert_year) == 0) "all"
          else input$prescription_insert_year

        # Log wildcard usage for debugging
        has_icd10_wildcards   <- any(grepl("[*?]", icd10_val))
        has_atc_wildcards     <- any(grepl("[*?]", atc_code_val))
        if (has_icd10_wildcards)
          message("ICD-10 wildcard patterns detected: ",
                  paste(icd10_val[grepl("[*?]", icd10_val)], collapse = ", "))
        if (has_atc_wildcards)
          message("ATC wildcard patterns detected: ",
                  paste(atc_code_val[grepl("[*?]", atc_code_val)], collapse = ", "))

        sql_query <- generate_sql_query(
          table = "idika",
          prescription_insert_year = prescription_insert_year_val,
          age_group = age_group_val,
          sex = sex_val,
          region = region_val,
          icd10 = icd10_val,
          atc_code = atc_code_val
        )

        message("Executing query: ", sql_query)

        result <- DBI::dbGetQuery(con, sql_query)
        result |>
          dplyr::mutate(
            sex = get_field_names(con, "sex", sex_id),
            age_group = get_field_names(con, "age_group", age_group_id),
            region = get_field_names(con, "region", region_id)
          ) |>
          dplyr::select(
            c(
              "prescription_insert_year", "region", "age_group",
              "sex", "icd10", "atc_code", "total_prescriptions",
              "unique_patients"
            )
          )
      },
      ignoreNULL = FALSE
    )

    initial_data <- shiny::reactive({
      shiny::req(con)
      result <- DBI::dbGetQuery(con, "SELECT * FROM idika LIMIT 1000")
      result |>
        dplyr::mutate(
          sex = get_field_names(con, "sex", sex_id),
          age_group = get_field_names(con, "age_group", age_group_id),
          region = get_field_names(con, "region", region_id)
        ) |>
        dplyr::select(
          c(
            "prescription_insert_year", "region", "age_group",
            "sex", "icd10", "atc_code", "total_prescriptions",
            "unique_patients"
          )
        )
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
        session, "sex",
        selected = character(0)
      )
      shiny::updateSelectizeInput(
        session, "region",
        selected = character(0)
      )
      shiny::updateSelectizeInput(
        session, "icd10",
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
