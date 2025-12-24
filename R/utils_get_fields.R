#' get_field_ids
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_field_ids <- function(con, table, names) {
  if (length(names) == 0) {
    return(integer(0))
  }

  unique_names <- unique(names)
  quoted_names <- paste0("'", unique_names, "'")

  result <- DBI::dbGetQuery(
    conn = con,
    glue::glue(
      "
      SELECT {table}_id, {table}_name
      FROM {table}
      WHERE {table}_name IN ({glue::glue_collapse(quoted_names, sep = ',')});
      "
    )
  )

  lookup <- setNames(
    result[[paste0(table, "_id")]],
    result[[paste0(table, "_name")]]
  )

  lookup[names] |> unname()
}

#' get_field_ids
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_field_names <- function(con, table, ids) {
  if (length(ids) == 0) {
    return(character(0))
  }

  unique_ids <- unique(ids)

  result <- DBI::dbGetQuery(
    conn = con,
    glue::glue(
      "
      SELECT {table}_id, {table}_name
      FROM {table}
      WHERE {table}_id IN ({glue::glue_collapse(unique_ids, sep = ',')});
      "
    )
  )

  lookup <- setNames(
    result[[paste0(table, "_name")]], 
    as.character(result[[paste0(table, "_id")]])
  )

  unname(lookup[as.character(ids)])
}
