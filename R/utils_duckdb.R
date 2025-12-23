#' duckdb 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_duckdb_connection <- function(db_path = NULL) {
  if (is.null(db_path)) {
    # Default path - you can set this in config
    db_path <- golem::get_golem_options("db_path")
  }

  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = TRUE
  )

  con
}

generate_sql_query <- function(
  prescription_insert_year = "all",
  age_group = "all",
  sex_id = "all",
  county = "all",
  atc_code = "all"
) {

  .build_condition <- function(values, column_name) {
    if (paste(values, collapse = ",") == "all") {
      return("")
    }

    quoted_values <- paste0("'", values, "'")
    glue::glue("{column_name} IN ({glue::glue_collapse(quoted_values, sep = ',')})")
  }

  conditions <- c(
    .build_condition(prescription_insert_year, "prescription_insert_year"),
    .build_condition(age_group, "age_group"),
    .build_condition(sex_id, "sex_id"),
    .build_condition(county, "county"),
    .build_condition(atc_code, "atc_code")
  )
  conditions <- conditions[conditions != ""]

  if (length(conditions) > 0) {
    where_clause <- glue::glue_collapse(conditions, sep = " AND ")
    sql_query <- glue::glue("SELECT * FROM idika WHERE {where_clause}")
  } else {
    sql_query <- "SELECT * FROM idika"
  }

  sql_query
}
