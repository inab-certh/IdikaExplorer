mod_summary_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(ns("summary_table"))
  )
}

mod_summary_table_server <- function(id, data_reactive, grouping_vars_reactive, denominator_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$summary_table <- DT::renderDataTable({
      data <- data_reactive()
      grouping_vars <- grouping_vars_reactive()
      denominator_data <- denominator_reactive()
      
      shiny::req(nrow(data) > 0)
      
      message("Computing summary statistics...")
      message("Grouping by: ", paste(grouping_vars, collapse = ", "))
      
      # Calculate summary data
      if (length(grouping_vars) == 0) {
        summary_data <- data |>
          dplyr::summarise(
            total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
            unique_patients = sum(unique_patients, na.rm = TRUE)
          ) |>
          dplyr::mutate(group = "Overall")
        
        summary_data$population <- denominator_data$denominator$population
        
        summary_data <- summary_data |>
          dplyr::select(group, total_prescriptions, unique_patients, population)
        
      } else {
        summary_data <- data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) |>
          dplyr::summarise(
            total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
            unique_patients = sum(unique_patients, na.rm = TRUE),
            .groups = "drop"
          )
        
        join_vars <- intersect(
          grouping_vars,
          names(denominator_data$denominator)
        )
        
        message("Join variables: ", paste(join_vars, collapse = ", "))
        
        if (length(join_vars) > 0) {
          summary_data <- summary_data |>
            dplyr::left_join(
              denominator_data$denominator,
              by = join_vars
            )
        } else {
          summary_data$population <- sum(denominator_data$denominator$population)
        }
        
        summary_data <- summary_data |>
          dplyr::mutate(
            prescriptions_per_100k = (total_prescriptions / population) * 100000,
            patients_per_100k = (unique_patients / population) * 100000
          ) |>
          dplyr::arrange(dplyr::desc(total_prescriptions))
      }
      
      message("Summary table has ", nrow(summary_data), " rows")
      
      # Create nice column names mapping
      column_name_map <- c(
        "prescription_insert_year" = "Year",
        "age_group" = "Age",
        "sex" = "Sex",
        "region" = "Region",
        "icd10" = "ICD-10",
        "atc_code" = "ATC Code",
        "group" = "Group",
        "total_prescriptions" = "Total Prescriptions",
        "unique_patients" = "Unique Patients",
        "population" = "Population",
        "prescriptions_per_100k" = "Prescriptions per 100K",
        "patients_per_100k" = "Patients per 100K"
      )
      
      # Rename columns to nice names
      current_names <- names(summary_data)
      nice_names <- sapply(current_names, function(name) {
        if (name %in% names(column_name_map)) {
          column_name_map[name]
        } else {
          name
        }
      }, USE.NAMES = FALSE)
      
      names(summary_data) <- nice_names
      
      # Identify column types for formatting (using NEW nice names)
      count_cols <- intersect(
        c("Total Prescriptions", "Unique Patients", "Population"),
        names(summary_data)
      )
      
      rate_cols <- intersect(
        c("Prescriptions per 100K", "Patients per 100K"),
        names(summary_data)
      )
      
      # Create base datatable
      dt <- DT::datatable(
        summary_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          autoWidth = TRUE,
          scrollCollapse = FALSE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          ),
          initComplete = DT::JS(
            "function(settings, json) {",
            "  setTimeout(function() {",
            "    $(settings.nTable).DataTable().columns.adjust();",
            "  }, 100);",
            "}"
          )
        ),
        extensions = 'Buttons',
        filter = "top",
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) |>
        DT::formatRound(columns = count_cols, digits = 0, mark = ",")
      
      # Add styled bars for prescriptions per 100k (starting from zero)
      if ("Prescriptions per 100K" %in% names(summary_data)) {
        dt <- dt |>
          DT::formatStyle(
            "Prescriptions per 100K",
            background = DT::styleColorBar(
              c(0, max(summary_data$`Prescriptions per 100K`, na.rm = TRUE)),
              color = "rgba(70, 130, 180, 0.35)"  # Steel blue with transparency
            ),
            backgroundSize = "95% 80%",
            backgroundRepeat = "no-repeat",
            backgroundPosition = "center",
            fontWeight = "500"
          ) |>
          DT::formatRound(columns = "Prescriptions per 100K", digits = 1, mark = ",")
      }
      
      # Add styled bars for patients per 100k (starting from zero)
      if ("Patients per 100K" %in% names(summary_data)) {
        dt <- dt |>
          DT::formatStyle(
            "Patients per 100K",
            background = DT::styleColorBar(
              c(0, max(summary_data$`Patients per 100K`, na.rm = TRUE)),
              color = "rgba(46, 204, 113, 0.35)"  # Emerald green with transparency
            ),
            backgroundSize = "95% 80%",
            backgroundRepeat = "no-repeat",
            backgroundPosition = "center",
            fontWeight = "500"
          ) |>
          DT::formatRound(columns = "Patients per 100K", digits = 1, mark = ",")
      }
      
      dt
    })
  })
}
