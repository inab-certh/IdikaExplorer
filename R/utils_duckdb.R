#' duckdb 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
get_duckdb_connection <- function(db_path = NULL) {
  if (is.null(db_path)) {
    db_path <- get_db_path()
  }
  
  if (!file.exists(db_path)) {
    stop("Database file not found at: ", db_path)
  }

  message("Connecting to DuckDB at: ", db_path)
  
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = TRUE
  )

  con
}

#' Get database path with multiple fallbacks
#' @return Path to idika.duckdb
#' @noRd
get_db_path <- function() {
  # Priority 1: Environment variable (Docker)
  db_path <- Sys.getenv("DB_PATH", unset = NA)
  
  if (!is.na(db_path)) {
    if (file.exists(db_path)) {
      message("Using database from DB_PATH environment variable")
      return(db_path)
    } else {
      warning("DB_PATH set but file not found: ", db_path)
    }
  }
  
  # Priority 2: Golem options (passed from run_app)
  golem_path <- golem::get_golem_options("db_path")
  if (!is.null(golem_path) && file.exists(golem_path)) {
    message("Using database from golem options")
    return(golem_path)
  }
  
  # Priority 3: Docker mount path
  docker_path <- "/app/data/idika.duckdb"
  if (file.exists(docker_path)) {
    message("Using database from Docker mount")
    return(docker_path)
  }
  
  # Priority 4: Package inst/extdata (for development)
  pkg_path <- system.file("extdata", "idika.duckdb", package = "IdikaExplorer")
  if (pkg_path != "" && file.exists(pkg_path)) {
    message("Using database from package inst/extdata")
    return(pkg_path)
  }
  
  # Priority 5: Common local paths
  local_paths <- c(
    "data/idika.duckdb",
    "inst/extdata/idika.duckdb",
    "idika.duckdb"
  )
  
  for (path in local_paths) {
    if (file.exists(path)) {
      message("Using database from: ", path)
      return(path)
    }
  }
  
  # If nothing found, show helpful error
  stop(
    "Database 'idika.duckdb' not found. Searched:\n",
    "  - DB_PATH environment variable: ", ifelse(is.na(db_path), "not set", db_path), "\n",
    "  - Golem options db_path: ", ifelse(is.null(golem_path), "not set", golem_path), "\n",
    "  - Docker mount: /app/data/idika.duckdb\n",
    "  - Package extdata: ", system.file("extdata", package = "IdikaExplorer"), "\n",
    "  - Local paths: data/, inst/extdata/, ./\n",
    "\nPlease ensure database is available in one of these locations."
  )
}

generate_sql_query <- function(
  table = "idika",
  prescription_insert_year = "all",
  age_group = "all",
  sex = "all",
  region = "all",
  icd10 = "all",
  atc_code = "all"
) {

  if (table == "idika") {
    conditions <- c(
      build_condition(prescription_insert_year, "prescription_insert_year"),
      build_condition(age_group, "age_group_id"),
      build_condition(sex, "sex_id"),
      build_condition(region, "region_id"),
      build_condition(icd10, "icd10", use_like = TRUE),
      build_condition(atc_code, "atc_code", use_like = TRUE)
    )
  } else if (table == "population_by_region") {
    conditions <- c(
      build_condition(age_group, "age_group_id"),
      build_condition(sex, "sex_id"),
      build_condition(region, "region_id")
    )
  }

  conditions <- conditions[conditions != ""]

  if (length(conditions) > 0) {
    where_clause <- glue::glue_collapse(conditions, sep = " AND ")
    sql_query <- glue::glue("SELECT * FROM { table } WHERE {where_clause}")
  } else {
    sql_query <- glue::glue("SELECT * FROM { table }")
  }

  sql_query
}

#' Check if a value string contains a wildcard pattern
#' @param value A single character string
#' @return TRUE if the value contains * or ? wildcards
#' @noRd
is_wildcard_pattern <- function(value) {
  grepl("[*?]", value, fixed = FALSE)
}

#' Convert a user-supplied wildcard pattern to a SQL LIKE pattern
#' Replaces * with % and ? with _
#' @param pattern A wildcard pattern string
#' @return A SQL LIKE-compatible pattern string
#' @noRd
wildcard_to_sql_like <- function(pattern) {
  # Escape any existing SQL special chars first (%, _) that aren't our wildcards
  pattern <- gsub("%", "\\%", pattern, fixed = TRUE)
  pattern <- gsub("_", "\\_", pattern, fixed = TRUE)
  # Now convert user wildcards
  pattern <- gsub("*", "%", pattern, fixed = TRUE)
  pattern <- gsub("?", "_", pattern, fixed = TRUE)
  pattern
}

#' Build a SQL WHERE condition for a column, supporting wildcards
#'
#' For columns where \code{use_like = TRUE}, each value is checked:
#' - If it contains \code{*} or \code{?}, it is emitted as a
#'   \code{LIKE} clause (case-insensitive via \code{ILIKE} or \code{UPPER()}).
#' - Otherwise it is treated as a plain equality / \code{IN} value.
#'
#' Multiple values are combined with \code{OR} and wrapped in parentheses.
#'
#' @param values Character vector of filter values, or \code{"all"}.
#' @param column_name Name of the database column.
#' @param use_like Logical; enable wildcard expansion for this column.
#' @return A SQL condition string, or \code{""} if no filtering is needed.
#' @noRd
build_condition <- function(values, column_name, use_like = FALSE) {
  values <- unlist(values)

  if (length(values) == 0 || identical(as.character(values), "all")) {
    return("")
  }

  if (!use_like) {
    # Original behaviour: simple IN clause
    quoted_values <- paste0("'", values, "'")
    return(glue::glue(
      "{column_name} IN ({glue::glue_collapse(quoted_values, sep = ',')})"
    ))
  }

  # Split values into plain (exact match) and wildcard (LIKE match)
  has_wildcard <- vapply(values, is_wildcard_pattern, logical(1))

  exact_values   <- values[!has_wildcard]
  pattern_values <- values[has_wildcard]

  clauses <- character(0)

  # Exact matches → IN (...)
  if (length(exact_values) > 0) {
    quoted <- paste0("'", exact_values, "'")
    clauses <- c(
      clauses,
      glue::glue("{column_name} IN ({glue::glue_collapse(quoted, sep = ',')})")
    )
  }

  # Wildcard matches → UPPER(col) LIKE UPPER('pattern')  [case-insensitive]
  for (pat in pattern_values) {
    sql_pat <- wildcard_to_sql_like(pat)
    clauses <- c(
      clauses,
      glue::glue("UPPER({column_name}) LIKE UPPER('{sql_pat}')")
    )
  }

  if (length(clauses) == 0) return("")
  if (length(clauses) == 1) return(clauses)

  paste0("(", paste(clauses, collapse = " OR "), ")")
}
