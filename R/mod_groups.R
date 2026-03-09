# mod_groups.R
#
# Module: user-defined code groups for ATC (drug) and ICD-10 (disease) codes.
#
# Each group has:
#   - a name  (e.g. "SSRIs")
#   - one or more comma-separated patterns  (e.g. "N06AB*, N06AX21")
#     * → any sequence of characters
#     ? → exactly one character
#     Plain values (no wildcard) are matched exactly, case-insensitive.
#
# The module server returns a reactive list:
#   list(
#     atc   = list("SSRIs" = c("N06AB*", "N06AX21"), ...),
#     icd10 = list("Psychosis" = c("F20*", "F25*"),  ...)
#   )
#
# Reactivity design
# -----------------
# current_groups() only updates when the user clicks "Apply groups".
# This avoids the infinite-loop caused by reactiveValuesToList(input), which
# subscribes to every input (including button counters that increment on each
# row insertion) and triggers immediate re-invalidation.

# ── UI ─────────────────────────────────────────────────────────────────────────

mod_groups_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      shiny::column(12,
        shiny::helpText(
          shiny::icon("circle-info"),
          " Groups defined here are applied in the Heatmap tab.",
          "Grouped codes collapse into a single row / column.",
          "Ungrouped codes appear individually (subject to Top-N).",
          "Use", shiny::tags$code("*"), "as wildcard (e.g.",
          shiny::tags$code("N06AB*"), ").",
          "Separate multiple patterns with commas.",
          shiny::strong("Click \u2018Apply groups\u2019 to update the heatmap.")
        )
      )
    ),

    shiny::fluidRow(

      # ── Drug groups (ATC) ─────────────────────────────────────────────────
      shiny::column(6,
        shiny::h5(shiny::icon("pills"), " Drug groups (ATC)"),
        shiny::div(
          style = paste0("display:flex; gap:8px; margin-bottom:2px;",
                         "font-size:0.82em; color:#666; font-weight:600;"),
          shiny::div(style = "flex:0 0 150px;", "Group name"),
          shiny::div(style = "flex:1;",          "Patterns (comma-separated)"),
          shiny::div(style = "flex:0 0 36px;",   "")
        ),
        shiny::div(id = ns("atc_container")),
        shiny::actionButton(
          ns("add_atc"),
          shiny::tagList(shiny::icon("plus"), " Add drug group"),
          class = "btn-sm btn-outline-primary",
          style = "margin-top:4px;"
        )
      ),

      # ── Disease groups (ICD-10) ───────────────────────────────────────────
      shiny::column(6,
        shiny::h5(shiny::icon("stethoscope"), " Disease groups (ICD-10)"),
        shiny::div(
          style = paste0("display:flex; gap:8px; margin-bottom:2px;",
                         "font-size:0.82em; color:#666; font-weight:600;"),
          shiny::div(style = "flex:0 0 150px;", "Group name"),
          shiny::div(style = "flex:1;",          "Patterns (comma-separated)"),
          shiny::div(style = "flex:0 0 36px;",   "")
        ),
        shiny::div(id = ns("icd10_container")),
        shiny::actionButton(
          ns("add_icd10"),
          shiny::tagList(shiny::icon("plus"), " Add disease group"),
          class = "btn-sm btn-outline-primary",
          style = "margin-top:4px;"
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(12,
        shiny::br(),
        shiny::actionButton(
          ns("apply_groups"),
          shiny::tagList(shiny::icon("check"), " Apply groups"),
          class = "btn-primary"
        )
      )
    )
  )
}


# ── Server ─────────────────────────────────────────────────────────────────────

mod_groups_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Monotonically increasing counters so input names are never reused.
    atc_seq   <- shiny::reactiveVal(0L)
    icd10_seq <- shiny::reactiveVal(0L)

    # Currently visible row indices (not deleted).
    atc_active   <- shiny::reactiveVal(integer(0))
    icd10_active <- shiny::reactiveVal(integer(0))

    # ── Generic: render one group row ─────────────────────────────────────
    insert_row <- function(container_id, idx, type,
                           init_name = "", init_patterns = "") {
      name_id     <- paste0(type, "_name_",     idx)
      patterns_id <- paste0(type, "_patterns_", idx)
      remove_id   <- paste0(type, "_remove_",   idx)
      row_id      <- paste0(type, "_row_",      idx)

      shiny::insertUI(
        selector = paste0("#", ns(container_id)),
        where    = "beforeEnd",
        ui = shiny::div(
          id    = ns(row_id),
          style = "display:flex; gap:8px; align-items:center; margin-bottom:5px;",
          shiny::div(
            style = "flex:0 0 150px;",
            shiny::textInput(ns(name_id), label = NULL,
                             value = init_name,
                             placeholder = "e.g. SSRIs")
          ),
          shiny::div(
            style = "flex:1;",
            shiny::textInput(ns(patterns_id), label = NULL,
                             value = init_patterns,
                             placeholder = "e.g. N06AB*, N06AX21")
          ),
          shiny::div(
            style = "flex:0 0 36px;",
            shiny::actionButton(
              ns(remove_id), label = shiny::icon("xmark"),
              class = "btn-sm btn-outline-danger"
            )
          )
        )
      )
    }

    # ── ATC: add row ──────────────────────────────────────────────────────
    shiny::observeEvent(input$add_atc, {
      idx <- atc_seq() + 1L
      atc_seq(idx)
      insert_row("atc_container", idx, "atc")
      atc_active(c(atc_active(), idx))

      local({
        i <- idx
        shiny::observeEvent(
          input[[paste0("atc_remove_", i)]],
          {
            shiny::removeUI(
              selector = paste0("#", ns(paste0("atc_row_", i)))
            )
            atc_active(setdiff(atc_active(), i))
          },
          ignoreInit = TRUE, once = TRUE
        )
      })
    }, ignoreInit = TRUE)

    # ── ICD-10: add row ───────────────────────────────────────────────────
    shiny::observeEvent(input$add_icd10, {
      idx <- icd10_seq() + 1L
      icd10_seq(idx)
      insert_row("icd10_container", idx, "icd10")
      icd10_active(c(icd10_active(), idx))

      local({
        i <- idx
        shiny::observeEvent(
          input[[paste0("icd10_remove_", i)]],
          {
            shiny::removeUI(
              selector = paste0("#", ns(paste0("icd10_row_", i)))
            )
            icd10_active(setdiff(icd10_active(), i))
          },
          ignoreInit = TRUE, once = TRUE
        )
      })
    }, ignoreInit = TRUE)

    # ── Snapshot: stored group definitions, updated only on Apply ─────────
    # This reactiveVal holds the last applied snapshot.  The heatmap depends
    # on this, NOT on the live input fields, so it only re-renders when the
    # user explicitly clicks Apply.
    applied_groups <- shiny::reactiveVal(list(atc = list(), icd10 = list()))

    shiny::observeEvent(input$apply_groups, {
      atc_ids   <- atc_active()
      icd10_ids <- icd10_active()

      collect <- function(type, active_ids) {
        out <- list()
        for (i in active_ids) {
          nm   <- trimws(input[[paste0(type, "_name_",     i)]] %||% "")
          pats <- trimws(input[[paste0(type, "_patterns_", i)]] %||% "")
          if (!nzchar(nm) || !nzchar(pats)) next
          parsed <- trimws(unlist(strsplit(pats, ",", fixed = TRUE)))
          parsed <- parsed[nzchar(parsed)]
          if (length(parsed) == 0L) next
          out[[nm]] <- unique(c(out[[nm]], parsed))
        }
        out
      }

      applied_groups(list(
        atc   = collect("atc",   atc_ids),
        icd10 = collect("icd10", icd10_ids)
      ))
    }, ignoreInit = TRUE)

    return(applied_groups)
  })
}


# ── Pattern-matching helpers (used by mod_heatmap_server) ──────────────────────

#' Convert a wildcard pattern (*=any, ?=one char) to a full-match PCRE regex.
#' @noRd
wildcard_to_regex <- function(pat) {
  # Escape regex metacharacters (except * and ?, handled next).
  esc <- gsub("([.+^${}()|\\[\\]\\\\])", "\\\\\\1", pat, perl = TRUE)
  # Replace wildcard chars using fixed=TRUE so the search string is literal.
  esc <- gsub("*", ".*", esc, fixed = TRUE)
  esc <- gsub("?", ".",  esc, fixed = TRUE)
  paste0("(?i)^", esc, "$")
}

#' Test which elements of `codes` match any pattern in `patterns`.
#' @param codes      Character vector of raw codes.
#' @param patterns   Character vector of wildcard/exact patterns.
#' @param strip_dots Strip "." before matching (ICD-10: data="F20.0", table="F200").
#' @return Logical vector, same length as `codes`.
#' @noRd
codes_match_any <- function(codes, patterns, strip_dots = FALSE) {
  cmp <- if (strip_dots) gsub(".", "", codes, fixed = TRUE) else codes
  matched <- rep(FALSE, length(codes))
  for (pat in patterns) {
    cmp_pat <- if (strip_dots) gsub(".", "", pat, fixed = TRUE) else pat
    if (grepl("[*?]", cmp_pat, perl = TRUE)) {
      matched <- matched | grepl(wildcard_to_regex(cmp_pat), cmp, perl = TRUE)
    } else {
      matched <- matched | (tolower(cmp) == tolower(cmp_pat))
    }
  }
  matched
}

#' Map codes to group names, leaving ungrouped codes unchanged.
#' First-match-wins when a code belongs to multiple groups.
#' @param codes      Character vector of raw codes.
#' @param groups     Named list; each element is a character vector of patterns.
#' @param strip_dots Passed to `codes_match_any` (TRUE for ICD-10).
#' @return Character vector, same length as `codes`.
#' @noRd
apply_groups_to_codes <- function(codes, groups, strip_dots = FALSE) {
  if (length(groups) == 0L) return(codes)
  result <- codes
  for (grp_name in names(groups)) {
    still_original <- (result == codes)
    hit <- still_original &
           codes_match_any(codes, groups[[grp_name]], strip_dots = strip_dots)
    result[hit] <- grp_name
  }
  result
}
