#' heatmap UI Function
#'
#' @description A shiny Module for ICD-10 x ATC heatmap visualization.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

    shiny::fluidRow(
      shiny::column(12,
        shiny::h4("Prescription Heatmap"),
        shiny::helpText(
          "Each cell shows prescriptions of a given ATC code per patient",
          "diagnosed with a given ICD-10 condition.",
          "The denominator is the total number of unique patients carrying",
          "that ICD-10 code in the current filtered dataset.",
          "Define custom groups in the", shiny::strong("Groups"), "panel above",
          "to collapse multiple codes into a single row / column."
        )
      )
    ),

    # ── Controls ────────────────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(3,
        shiny::numericInput(
          ns("top_n_icd10"),
          "Top N ICD-10 / disease groups:",
          value = 20, min = 1, max = 100, step = 1
        )
      ),
      shiny::column(3,
        shiny::numericInput(
          ns("top_n_atc"),
          "Top N ATC / drug groups:",
          value = 20, min = 1, max = 100, step = 1
        )
      ),
      shiny::column(3,
        shiny::selectInput(
          ns("color_scale"),
          "Colour scale:",
          choices = c(
            "Red \u2192 Yellow" = "RdYlOrRev",
            "Blue \u2192 Red"   = "RdBu",
            "Viridis"           = "Viridis",
            "Blues"             = "Blues",
            "Greens"            = "Greens"
          ),
          selected = "RdYlOrRev"
        )
      ),
      shiny::column(3,
        shiny::checkboxInput(
          ns("log_scale"),
          "Log\u2081\u2080 colour scale",
          value = FALSE
        ),
        shiny::br(),
        shiny::downloadButton(ns("download_data"), "Download data (CSV)",
                              class = "btn-sm btn-secondary")
      )
    ),

    shiny::hr(),

    # ── Plot ─────────────────────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("heatmap_plot"), height = "700px")
        )
      )
    ),

    # ── Detail table (appears on cell click) ─────────────────────────────────
    shiny::fluidRow(
      shiny::column(12,
        shiny::uiOutput(ns("click_info_header")),
        DT::dataTableOutput(ns("click_detail_table"))
      )
    )
  )
}


#' heatmap Server Functions
#'
#' @param id        Module id.
#' @param data_reactive Reactive returning the filtered prescription data frame.
#' @param groups    Reactive returned by mod_groups_server(); list(atc=…, icd10=…).
#' @param con       DBI connection to the DuckDB database.
#'
#' @noRd
mod_heatmap_server <- function(id, data_reactive, groups, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── 0. Lookup tables (fetched once per session) ──────────────────────────

    atc_lookup <- shiny::reactive({
      DBI::dbGetQuery(con,
        "SELECT atc_code, atc_name FROM atc_codes ORDER BY atc_code"
      ) |>
        dplyr::distinct(atc_code, .keep_all = TRUE)
    })

    icd10_lookup <- shiny::reactive({
      DBI::dbGetQuery(con,
        "SELECT code, short_description FROM icd_10_cm ORDER BY code"
      ) |>
        dplyr::distinct(code, .keep_all = TRUE)
    })

    # ── Label helpers ────────────────────────────────────────────────────────
    # For grouped entries the "code" IS already the group name — just return it.
    # For raw codes, append the human-readable name from the lookup table.

    make_atc_label <- function(codes, lookup) {
      atc_grp_names <- names(groups()$atc)
      is_group <- codes %in% atc_grp_names
      m    <- match(codes, lookup$atc_code)
      name <- lookup$atc_name[m]
      dplyr::case_when(
        is_group        ~ codes,                              # already a name
        !is.na(name)    ~ paste0(codes, " \u2013 ", name),   # code + name
        TRUE            ~ codes                               # fallback
      )
    }

    make_icd10_label <- function(codes, lookup) {
      icd_grp_names <- names(groups()$icd10)
      is_group    <- codes %in% icd_grp_names
      codes_nodot <- gsub(".", "", codes, fixed = TRUE)
      m    <- match(codes_nodot, lookup$code)
      desc <- lookup$short_description[m]
      dplyr::case_when(
        is_group        ~ codes,
        !is.na(desc)    ~ paste0(codes, " \u2013 ", desc),
        TRUE            ~ codes
      )
    }

    # ── 1. Build the heatmap data ────────────────────────────────────────────
    heatmap_data <- shiny::reactive({
      data <- data_reactive()
      shiny::req(nrow(data) > 0)

      top_n_icd <- as.integer(input$top_n_icd10)
      top_n_atc <- as.integer(input$top_n_atc)
      shiny::req(!is.na(top_n_icd), top_n_icd >= 1)
      shiny::req(!is.na(top_n_atc), top_n_atc  >= 1)

      grps      <- groups()
      atc_lkp   <- atc_lookup()
      icd10_lkp <- icd10_lookup()

      # ── Step 1: apply grouping to raw codes ─────────────────────────────
      # A grouped code is replaced by its group name; ungrouped codes keep
      # their original value.  This must happen BEFORE any aggregation so
      # that grouped codes are counted together.
      data_grouped <- data |>
        dplyr::mutate(
          atc_key  = apply_groups_to_codes(atc_code, grps$atc,
                                           strip_dots = FALSE),
          icd10_key = apply_groups_to_codes(icd10, grps$icd10,
                                            strip_dots = TRUE)
        )

      # ── Step 2: denominator — unique patients per ICD-10 key ─────────────
      # For grouped ICD-10 rows we sum unique_patients across ALL ATC codes,
      # so the denominator is the total patient pool for that disease group.
      icd10_patients <- data_grouped |>
        dplyr::group_by(icd10_key) |>
        dplyr::summarise(
          icd10_patients = sum(unique_patients, na.rm = TRUE),
          .groups = "drop"
        )

      # ── Step 3: top-N ICD-10 keys ────────────────────────────────────────
      top_icd10 <- icd10_patients |>
        dplyr::slice_max(order_by = icd10_patients, n = top_n_icd,
                         with_ties = FALSE) |>
        dplyr::pull(icd10_key)

      # ── Step 4: cell numerator ────────────────────────────────────────────
      cell_rx <- data_grouped |>
        dplyr::filter(icd10_key %in% top_icd10) |>
        dplyr::group_by(icd10_key, atc_key) |>
        dplyr::summarise(
          cell_prescriptions = sum(total_prescriptions, na.rm = TRUE),
          .groups = "drop"
        )

      # ── Step 5: top-N ATC keys ────────────────────────────────────────────
      top_atc <- cell_rx |>
        dplyr::group_by(atc_key) |>
        dplyr::summarise(atc_total = sum(cell_prescriptions), .groups = "drop") |>
        dplyr::slice_max(order_by = atc_total, n = top_n_atc,
                         with_ties = FALSE) |>
        dplyr::pull(atc_key)

      # ── Step 6: build final cell table ────────────────────────────────────
      result <- cell_rx |>
        dplyr::filter(atc_key %in% top_atc) |>
        dplyr::left_join(icd10_patients, by = "icd10_key") |>
        dplyr::mutate(
          rx_per_patient = dplyr::if_else(
            icd10_patients > 0,
            cell_prescriptions / icd10_patients,
            NA_real_
          ),
          atc_label   = make_atc_label(atc_key,   atc_lkp),
          icd10_label = make_icd10_label(icd10_key, icd10_lkp)
        )

      # Factor levels: ICD-10 sorted by patient count desc, ATC by Rx count desc
      top_icd10_labels <- make_icd10_label(top_icd10, icd10_lkp)
      top_atc_labels   <- make_atc_label(top_atc,     atc_lkp)

      result |>
        dplyr::mutate(
          icd10_label = factor(icd10_label, levels = rev(top_icd10_labels)),
          atc_label   = factor(atc_label,   levels = top_atc_labels),
          icd10_key   = factor(icd10_key,   levels = rev(top_icd10)),
          atc_key     = factor(atc_key,     levels = top_atc)
        )
    })

    # ── 2. Render the plotly heatmap ─────────────────────────────────────────
    output$heatmap_plot <- plotly::renderPlotly({
      df <- heatmap_data()
      shiny::req(nrow(df) > 0)

      use_log     <- isTRUE(input$log_scale)
      color_scale <- input$color_scale

      icd_levels <- levels(df$icd10_label)
      atc_levels <- levels(df$atc_label)
      n_icd      <- length(icd_levels)
      n_atc      <- length(atc_levels)

      # Build lookup keyed on label strings (characters, not factors)
      cell_lookup <- df |>
        dplyr::mutate(
          icd10_label = as.character(icd10_label),
          atc_label   = as.character(atc_label)
        )

      # Pre-allocate matrices indexed exactly by (i = icd row, j = atc col)
      z_raw    <- matrix(NA_real_,    nrow = n_icd, ncol = n_atc,
                         dimnames = list(icd_levels, atc_levels))
      rx_count <- matrix(NA_real_,    nrow = n_icd, ncol = n_atc)
      pt_count <- matrix(NA_real_,    nrow = n_icd, ncol = n_atc)
      icd_lbl_m <- matrix(icd_levels, nrow = n_icd, ncol = n_atc)
      atc_lbl_m <- matrix(atc_levels, nrow = n_icd, ncol = n_atc, byrow = TRUE)

      for (i in seq_len(n_icd)) {
        for (j in seq_len(n_atc)) {
          cell <- cell_lookup[
            cell_lookup$icd10_label == icd_levels[i] &
            cell_lookup$atc_label   == atc_levels[j], ]
          if (nrow(cell) == 1L) {
            z_raw[i, j]    <- cell$rx_per_patient
            rx_count[i, j] <- cell$cell_prescriptions
            pt_count[i, j] <- cell$icd10_patients
          }
        }
      }

      z_matrix <- if (use_log) log10(z_raw + 1) else z_raw

      hover_matrix <- matrix(
        sprintf(
          "<b>%s</b><br><b>%s</b><br><b>Rx per patient:</b> %s<br><b>Total Rx:</b> %s<br><b>ICD-10 patients:</b> %s",
          icd_lbl_m,
          atc_lbl_m,
          ifelse(is.na(z_raw), "No data",
                 formatC(z_raw, format = "f", digits = 3, big.mark = ",")),
          ifelse(is.na(rx_count), "\u2014",
                 format(rx_count, big.mark = ",")),
          ifelse(is.na(pt_count), "\u2014",
                 format(pt_count, big.mark = ","))
        ),
        nrow = n_icd, ncol = n_atc
      )

      reversed_ylorrd <- list(
        list(0,     "#800026"), list(0.125, "#bd0026"),
        list(0.25,  "#e31a1c"), list(0.375, "#fc4e2a"),
        list(0.5,   "#fd8d3c"), list(0.625, "#feb24c"),
        list(0.75,  "#fed976"), list(0.875, "#ffeda0"),
        list(1,     "#ffffcc")
      )

      colorscale_value <- switch(
        color_scale,
        RdYlOrRev = reversed_ylorrd,
        RdBu      = "RdBu",
        Viridis   = "Viridis",
        Blues     = "Blues",
        Greens    = "Greens",
        reversed_ylorrd
      )

      color_bar_title <- if (use_log) "log\u2081\u2080(Rx/patient+1)" else "Rx per patient"

      plotly::plot_ly(
        x          = atc_levels,
        y          = icd_levels,
        z          = z_matrix,
        type       = "heatmap",
        source     = ns("heatmap_click"),
        colorscale = colorscale_value,
        text       = hover_matrix,
        hoverinfo  = "text",
        colorbar   = list(
          title      = color_bar_title,
          titleside  = "right",
          tickformat = if (use_log) ".2f" else ".3f"
        ),
        xgap = 1,
        ygap = 1
      ) |>
        plotly::layout(
          xaxis = list(
            title      = "Drug (ATC / group)",
            tickangle  = -45,
            tickfont   = list(size = 10),
            automargin = TRUE
          ),
          yaxis = list(
            title      = "Disease (ICD-10 / group)",
            tickfont   = list(size = 10),
            automargin = TRUE
          ),
          margin = list(l = 120, b = 140, t = 40, r = 80),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          toImageButtonOptions = list(
            format   = "png",
            filename = "heatmap",
            width    = 1400,
            height   = 900,
            scale    = 2
          )
        )
    })

    # ── 3. Click → detail table ──────────────────────────────────────────────
    selected_cell <- shiny::reactiveVal(NULL)

    shiny::observeEvent(plotly::event_data("plotly_click", source = ns("heatmap_click")), {
      click <- plotly::event_data("plotly_click", source = ns("heatmap_click"))
      if (is.null(click)) return()

      df <- heatmap_data()
      # click$x = atc_label string, click$y = icd10_label string
      row <- df[as.character(df$atc_label)   == click$x &
                as.character(df$icd10_label) == click$y, ]
      if (nrow(row) == 0L) return()

      selected_cell(list(
        atc_key     = as.character(row$atc_key[1]),
        icd10_key   = as.character(row$icd10_key[1]),
        atc_label   = click$x,
        icd10_label = click$y,
        is_atc_group   = as.character(row$atc_key[1]) %in% names(groups()$atc),
        is_icd10_group = as.character(row$icd10_key[1]) %in% names(groups()$icd10)
      ))
    })

    output$click_info_header <- shiny::renderUI({
      cell <- selected_cell()
      if (is.null(cell)) return(NULL)
      shiny::tagList(
        shiny::hr(),
        shiny::h5(
          shiny::icon("table"),
          sprintf(" Detail: %s \u00d7 %s",
                  cell$icd10_label, cell$atc_label)
        )
      )
    })

    output$click_detail_table <- DT::renderDataTable({
      cell <- selected_cell()
      shiny::req(!is.null(cell))

      data  <- data_reactive()
      grps  <- groups()
      shiny::req(nrow(data) > 0)

      # Resolve which raw codes belong to the clicked key
      if (cell$is_atc_group) {
        atc_codes_sel <- data$atc_code[
          codes_match_any(data$atc_code, grps$atc[[cell$atc_key]],
                          strip_dots = FALSE)
        ] |> unique()
      } else {
        atc_codes_sel <- cell$atc_key
      }

      if (cell$is_icd10_group) {
        icd10_codes_sel <- data$icd10[
          codes_match_any(data$icd10, grps$icd10[[cell$icd10_key]],
                          strip_dots = TRUE)
        ] |> unique()
      } else {
        icd10_codes_sel <- cell$icd10_key
      }

      detail <- data |>
        dplyr::filter(
          atc_code %in% atc_codes_sel,
          icd10    %in% icd10_codes_sel
        ) |>
        dplyr::group_by(prescription_insert_year, region, age_group, sex) |>
        dplyr::summarise(
          total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
          unique_patients     = sum(unique_patients,     na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(total_prescriptions))

      if (nrow(detail) == 0L) return(NULL)

      names(detail) <- c("Year", "Region", "Age group", "Sex",
                         "Total Prescriptions", "Unique Patients")

      DT::datatable(
        detail,
        options    = list(pageLength = 10, scrollX = TRUE,
                          dom = "Bfrtip", buttons = c("copy", "csv")),
        extensions = "Buttons",
        rownames   = FALSE,
        class      = "cell-border stripe hover compact"
      ) |>
        DT::formatRound(c("Total Prescriptions", "Unique Patients"),
                        digits = 0, mark = ",")
    })

    # ── 4. Download ───────────────────────────────────────────────────────────
    output$download_data <- shiny::downloadHandler(
      filename = function() paste0("heatmap_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        df <- heatmap_data()
        readr::write_csv(
          df |>
            dplyr::select(icd10_key, icd10_label, atc_key, atc_label,
                          cell_prescriptions, icd10_patients, rx_per_patient) |>
            dplyr::rename(
              `ICD-10 / Group`      = icd10_key,
              `ICD-10 Label`        = icd10_label,
              `ATC / Group`         = atc_key,
              `ATC Label`           = atc_label,
              `Total Prescriptions` = cell_prescriptions,
              `ICD-10 Patients`     = icd10_patients,
              `Rx per Patient`      = rx_per_patient
            ) |>
            dplyr::arrange(`ICD-10 / Group`, `ATC / Group`),
          file
        )
      }
    )

  })
}
