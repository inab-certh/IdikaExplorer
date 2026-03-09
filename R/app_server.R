#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Database connection
  con <- get_duckdb_connection()

  session$onSessionEnded(function() {
    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  # ── Tab 1: Overall data with filters ──────────────────────────────────────

  overall_data <- mod_data_filters_server("filters_overall", con = con)

  overall_display_data <- shiny::reactive({
    if (overall_data$filter_btn() > 0) {
      overall_data$filtered_data()
    } else {
      overall_data$initial_data()
    }
  })

  mod_raw_table_server("raw_table_1", data_reactive = overall_display_data)

  # ── Groups (defined in Overall data tab) ─────────────────────────────────
  # groups() returns list(atc = list(...), icd10 = list(...))
  # Applied only after the user clicks "Apply groups".
  groups <- mod_groups_server("groups")

  # ── Grouped display data ─────────────────────────────────────────────────
  # Replace raw atc_code / icd10 values with group names wherever a group
  # pattern matches.  When no groups are defined the data is returned as-is.
  grouped_display_data <- shiny::reactive({
    data <- overall_display_data()
    grps <- groups()

    if (length(grps$atc) == 0L && length(grps$icd10) == 0L) {
      return(data)
    }

    data |>
      dplyr::mutate(
        atc_code = apply_groups_to_codes(atc_code, grps$atc,  strip_dots = FALSE),
        icd10    = apply_groups_to_codes(icd10,    grps$icd10, strip_dots = TRUE)
      )
  })

  # ── Tab 2: Subgroup Analysis ───────────────────────────────────────────────

  selected_grouping_vars <- mod_grouping_selector_server(
    "grouping_selector",
    filtered_data_reactive = grouped_display_data
  )

  pop_by_region <- shiny::reactive({
    display_data <- grouped_display_data()
    shiny::req(nrow(display_data) > 0)
    grouping_vars <- selected_grouping_vars()

    sex       <- get_field_ids(con, "sex",       display_data$sex       |> unique())
    age_group <- get_field_ids(con, "age_group", display_data$age_group |> unique())
    region    <- get_field_ids(con, "region",    display_data$region    |> unique())

    sql_query <- generate_sql_query(
      table     = "population_by_region",
      age_group = age_group,
      sex       = sex,
      region    = region
    )

    applied_grouping_vars <- intersect(
      grouping_vars,
      c("age_group", "sex", "region")
    )

    denominator_raw <- DBI::dbGetQuery(con, sql_query)

    if (length(applied_grouping_vars) > 0) {
      grouping_id_cols <- paste0(applied_grouping_vars, "_id")

      denominator <- denominator_raw |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_id_cols))) |>
        dplyr::summarise(population = sum(group_total), .groups = "drop") |>
        dplyr::mutate(
          dplyr::across(
            dplyr::ends_with("_id"),
            ~ {
              col_name <- sub("_id$", "", dplyr::cur_column())
              get_field_names(con, col_name, .x)
            },
            .names = "{sub('_id$', '', .col)}"
          )
        ) |>
        dplyr::select(-dplyr::ends_with("_id"))
    } else {
      denominator <- data.frame(
        population = sum(denominator_raw$group_total)
      )
    }

    list(
      denominator           = denominator,
      grouping_vars         = grouping_vars,
      applied_grouping_vars = applied_grouping_vars
    )
  })

  shiny::observe({
    pp <- pop_by_region()
    message("\n=== Denominator Data ===")
    message("Applied grouping vars: ", paste(pp$applied_grouping_vars, collapse = ", "))
    print(pp$denominator)
    message("========================\n")
  })

  # General tab (Summary Table)
  mod_summary_table_server(
    "summary_table",
    data_reactive         = grouped_display_data,
    grouping_vars_reactive = selected_grouping_vars,
    denominator_reactive  = pop_by_region
  )

  # Time Series tab
  mod_time_series_server(
    "time_series",
    data_reactive         = grouped_display_data,
    grouping_vars_reactive = selected_grouping_vars,
    denominator_reactive  = pop_by_region,
    con                   = con
  )

  # Map tab
  mod_map_server(
    "map_view",
    data_reactive = grouped_display_data,
    con           = con
  )

  # Heatmap tab
  # The heatmap module has its own internal grouping logic (mod_groups).
  # Now that groups live in the Overall tab we pass them in directly so the
  # heatmap can still collapse codes into named groups on the plot axes,
  # while the *input* data already has grouped codes from grouped_display_data.
  mod_heatmap_server(
    "heatmap_view",
    data_reactive = grouped_display_data,
    groups        = groups,
    con           = con
  )

  # Tab 3: Code Definitions
  mod_code_lookup_server("code_lookup", con = con)
}
