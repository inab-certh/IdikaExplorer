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
      shiny::column(
        12,
        shiny::h4("Prescription Heatmap"),
        shiny::helpText(
          "Each cell shows prescriptions of a given ATC code per patient",
          "diagnosed with a given ICD-10 condition.",
          "The denominator is the total number of unique patients carrying that ICD-10 code",
          "in the current filtered dataset."
        )
      )
    ),

    # ── Controls ────────────────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::numericInput(
          ns("top_n_icd10"),
          "Top N ICD-10 codes (by patient count):",
          value = 20, min = 1, max = 100, step = 1
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          ns("top_n_atc"),
          "Top N ATC codes (by prescription count):",
          value = 20, min = 1, max = 100, step = 1
        )
      ),
      shiny::column(
        3,
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
      shiny::column(
        3,
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

    # ── Plot ────────────────────────────────────────────────────────────────
    shiny::fluidRow(
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("heatmap_plot"), height = "700px")
        )
      )
    ),

    # ── Detail table (appears on cell click) ────────────────────────────────
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns("click_info_header")),
        DT::dataTableOutput(ns("click_detail_table"))
      )
    )
  )
}


#' heatmap Server Functions
#'
#' @noRd
mod_heatmap_server <- function(id, data_reactive, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── 0. Lookup tables (fetched once per session) ────────────────────────

    # ATC code → name  (atc_codes table; one row per code+route, take first name)
    atc_lookup <- shiny::reactive({
      DBI::dbGetQuery(con,
        "SELECT atc_code, atc_name
         FROM atc_codes
         ORDER BY atc_code"
      ) |>
        dplyr::distinct(atc_code, .keep_all = TRUE)
    })

    # ICD-10 code → short_description  (icd_10_cm table)
    icd10_lookup <- shiny::reactive({
      DBI::dbGetQuery(con,
        "SELECT code, short_description
         FROM icd_10_cm
         ORDER BY code"
      ) |>
        dplyr::distinct(code, .keep_all = TRUE)
    })

    # Helper: "CODE – Name" label, falling back to just the code if not found
    make_atc_label <- function(codes, lookup) {
      m <- match(codes, lookup$atc_code)
      name <- lookup$atc_name[m]
      dplyr::if_else(
        !is.na(name),
        paste0(codes, " \u2013 ", name),
        codes
      )
    }

    make_icd10_label <- function(codes, lookup) {
      # The icd_10_cm table stores codes without dots (e.g. "F200" not "F20.0").
      # Strip dots before matching, but keep the original dotted code for display.
      codes_nodot <- gsub(".", "", codes, fixed = TRUE)
      m <- match(codes_nodot, lookup$code)
      desc <- lookup$short_description[m]
      dplyr::if_else(
        !is.na(desc),
        paste0(codes, " \u2013 ", desc),
        codes
      )
    }

    # ── 1. Build the heatmap data ──────────────────────────────────────────
    heatmap_data <- shiny::reactive({
      data <- data_reactive()
      shiny::req(nrow(data) > 0)

      top_n_icd  <- as.integer(input$top_n_icd10)
      top_n_atc  <- as.integer(input$top_n_atc)

      shiny::req(!is.na(top_n_icd), top_n_icd >= 1)
      shiny::req(!is.na(top_n_atc), top_n_atc  >= 1)

      atc_lkp   <- atc_lookup()
      icd10_lkp <- icd10_lookup()

      # ── Step 1: denominator = unique patients per ICD-10 code
      #    We sum unique_patients per icd10 across all ATC codes so each
      #    icd10 row has a single patient pool size.
      icd10_patients <- data |>
        dplyr::group_by(icd10) |>
        dplyr::summarise(
          icd10_patients = sum(unique_patients, na.rm = TRUE),
          .groups = "drop"
        )

      # ── Step 2: select the top-N ICD-10 codes by patient count
      top_icd10 <- icd10_patients |>
        dplyr::slice_max(order_by = icd10_patients, n = top_n_icd,
                         with_ties = FALSE) |>
        dplyr::pull(icd10)

      # ── Step 3: numerator = total prescriptions per (ICD-10, ATC) cell
      cell_rx <- data |>
        dplyr::filter(icd10 %in% top_icd10) |>
        dplyr::group_by(icd10, atc_code) |>
        dplyr::summarise(
          cell_prescriptions = sum(total_prescriptions, na.rm = TRUE),
          cell_patients      = sum(unique_patients,     na.rm = TRUE),
          .groups = "drop"
        )

      # ── Step 4: select the top-N ATC codes by total prescriptions
      #    (within the already-filtered ICD-10 subset)
      top_atc <- cell_rx |>
        dplyr::group_by(atc_code) |>
        dplyr::summarise(atc_total = sum(cell_prescriptions), .groups = "drop") |>
        dplyr::slice_max(order_by = atc_total, n = top_n_atc,
                         with_ties = FALSE) |>
        dplyr::pull(atc_code)

      # ── Step 5: restrict cell data to top codes, join denominator,
      #            attach human-readable labels
      result <- cell_rx |>
        dplyr::filter(atc_code %in% top_atc) |>
        dplyr::left_join(icd10_patients, by = "icd10") |>
        dplyr::mutate(
          rx_per_patient = dplyr::if_else(
            icd10_patients > 0,
            cell_prescriptions / icd10_patients,
            NA_real_
          ),
          # Human-readable axis labels (code + name)
          atc_label   = make_atc_label(atc_code,   atc_lkp),
          icd10_label = make_icd10_label(icd10, icd10_lkp)
        )

      # Build label vectors in the same order as the code vectors
      top_icd10_labels <- make_icd10_label(top_icd10, icd10_lkp)
      top_atc_labels   <- make_atc_label(top_atc,     atc_lkp)

      # Keep factor levels sorted by icd10_patients (most common on top)
      result |>
        dplyr::mutate(
          icd10_label = factor(icd10_label, levels = rev(top_icd10_labels)),
          atc_label   = factor(atc_label,   levels = top_atc_labels),
          icd10       = factor(icd10,       levels = rev(top_icd10)),
          atc_code    = factor(atc_code,    levels = top_atc)
        )
    })

    # ── 2. Render the plotly heatmap ───────────────────────────────────────
    output$heatmap_plot <- plotly::renderPlotly({
      df <- heatmap_data()
      shiny::req(nrow(df) > 0)

      use_log    <- isTRUE(input$log_scale)
      color_scale <- input$color_scale

      # Build the z-matrix (ICD-10 rows × ATC columns)
      # Use human-readable label levels for axis ticks
      icd_levels <- levels(df$icd10_label)
      atc_levels <- levels(df$atc_label)

      # Build a lookup from the data for fast cell access
      cell_lookup <- df |>
        dplyr::select(icd10_label, atc_label,
                      rx_per_patient, cell_prescriptions, icd10_patients) |>
        dplyr::mutate(
          icd10_label = as.character(icd10_label),
          atc_label   = as.character(atc_label)
        )

      # Pre-allocate matrices: rows = ICD-10 levels, cols = ATC levels
      # Fill explicitly by (row, col) index to guarantee correct alignment
      n_icd <- length(icd_levels)
      n_atc <- length(atc_levels)

      z_raw     <- matrix(NA_real_,      nrow = n_icd, ncol = n_atc,
                          dimnames = list(icd_levels, atc_levels))
      rx_mat    <- matrix(NA_real_,      nrow = n_icd, ncol = n_atc)
      rx_count  <- matrix(NA_integer_,   nrow = n_icd, ncol = n_atc)
      pt_count  <- matrix(NA_integer_,   nrow = n_icd, ncol = n_atc)
      icd_lbl_m <- matrix(icd_levels,    nrow = n_icd, ncol = n_atc)       # broadcast rows
      atc_lbl_m <- matrix(atc_levels,    nrow = n_icd, ncol = n_atc,
                          byrow = TRUE)                                      # broadcast cols

      for (i in seq_len(n_icd)) {
        for (j in seq_len(n_atc)) {
          cell <- cell_lookup[
            cell_lookup$icd10_label == icd_levels[i] &
            cell_lookup$atc_label   == atc_levels[j], ]
          if (nrow(cell) == 1L) {
            z_raw[i, j]    <- cell$rx_per_patient
            rx_mat[i, j]   <- cell$rx_per_patient
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
          ifelse(is.na(rx_mat), "No data",
                 formatC(rx_mat, format = "f", digits = 3, big.mark = ",")),
          ifelse(is.na(rx_count), "\u2014",
                 format(rx_count, big.mark = ",")),
          ifelse(is.na(pt_count), "\u2014",
                 format(pt_count, big.mark = ","))
        ),
        nrow = n_icd, ncol = n_atc
      )

      # Build the colorscale: RdYlOrRev is YlOrRd reversed (high = red)
      # Plotly named scales run low→high; reversing the list flips the direction.
      reversed_ylorrd <- list(
        list(0,   "#800026"), list(0.125, "#bd0026"),
        list(0.25, "#e31a1c"), list(0.375, "#fc4e2a"),
        list(0.5,  "#fd8d3c"), list(0.625, "#feb24c"),
        list(0.75, "#fed976"), list(0.875, "#ffeda0"),
        list(1,    "#ffffcc")
      )

      colorscale_value <- switch(
        color_scale,
        RdYlOrRev = reversed_ylorrd,
        RdBu      = "RdBu",
        Viridis   = "Viridis",
        Blues     = "Blues",
        Greens    = "Greens",
        reversed_ylorrd   # fallback
      )

      color_bar_title <- if (use_log) "log\u2081\u2080(Rx/patient + 1)" else "Rx per patient"

      plotly::plot_ly(
        x           = atc_levels,
        y           = icd_levels,
        z           = z_matrix,
        type        = "heatmap",
        colorscale  = colorscale_value,
        text        = hover_matrix,
        hoverinfo   = "text",
        colorbar    = list(
          title      = color_bar_title,
          titleside  = "right",
          tickformat = if (use_log) ".2f" else ".3f"
        ),
        xgap = 1,
        ygap = 1
      ) |>
        plotly::layout(
          xaxis = list(
            title          = "ATC Code",
            tickangle      = -45,
            tickfont       = list(size = 10),
            automargin     = TRUE
          ),
          yaxis = list(
            title          = "ICD-10 Code",
            tickfont       = list(size = 10),
            automargin     = TRUE
          ),
          margin = list(l = 120, b = 120, t = 40, r = 80),
          plot_bgcolor  = "#ffffff",
          paper_bgcolor = "#ffffff"
        ) |>
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToAdd = list("hoverClosestCartesian"),
          toImageButtonOptions = list(
            format   = "png",
            filename = "heatmap",
            width    = 1400,
            height   = 900,
            scale    = 2
          )
        )
    })

    # ── 3. Click → detail table for the selected (ICD-10, ATC) cell ────────
    selected_cell <- shiny::reactiveVal(NULL)

    shiny::observeEvent(plotly::event_data("plotly_click", source = ns("heatmap")), {
      click <- plotly::event_data("plotly_click", source = ns("heatmap"))
      if (!is.null(click)) {
        df <- heatmap_data()
        # click$x / click$y are the label strings; resolve back to raw codes
        atc_raw   <- df$atc_code[match(click$x,  as.character(df$atc_label))]
        icd10_raw <- df$icd10[match(click$y, as.character(df$icd10_label))]
        selected_cell(list(
          atc       = atc_raw,
          icd10     = icd10_raw,
          atc_label = click$x,
          icd10_label = click$y
        ))
      }
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

      data <- data_reactive()
      shiny::req(nrow(data) > 0)

      detail <- data |>
        dplyr::filter(icd10 == cell$icd10, atc_code == cell$atc) |>
        dplyr::group_by(prescription_insert_year, region, age_group, sex) |>
        dplyr::summarise(
          total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
          unique_patients     = sum(unique_patients,     na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(total_prescriptions))

      if (nrow(detail) == 0) return(NULL)

      names(detail) <- c("Year", "Region", "Age group", "Sex",
                         "Total Prescriptions", "Unique Patients")

      DT::datatable(
        detail,
        options = list(pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
                       buttons = c("copy", "csv")),
        extensions = "Buttons",
        rownames   = FALSE,
        class      = "cell-border stripe hover compact"
      ) |>
        DT::formatRound(c("Total Prescriptions", "Unique Patients"),
                        digits = 0, mark = ",")
    })

    # ── 4. Download handler ────────────────────────────────────────────────
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste0("heatmap_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- heatmap_data()
        readr::write_csv(
          df |>
            dplyr::select(icd10, icd10_label, atc_code, atc_label,
                          cell_prescriptions, icd10_patients, rx_per_patient) |>
            dplyr::rename(
              `ICD-10 Code`         = icd10,
              `ICD-10 Description`  = icd10_label,
              `ATC Code`            = atc_code,
              `ATC Name`            = atc_label,
              `Total Prescriptions` = cell_prescriptions,
              `ICD-10 Patients`     = icd10_patients,
              `Rx per Patient`      = rx_per_patient
            ) |>
            dplyr::arrange(`ICD-10 Code`, `ATC Code`),
          file
        )
      }
    )

  })
}
