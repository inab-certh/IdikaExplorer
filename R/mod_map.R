# --- Hardcoded lookup: DB region name -> NUTS_ID ---
get_region_nuts_mapping <- function() {
  data.frame(
    db_region = c(
      # ΑΤΤΙΚΗ - all map to dissolved EL300
      "ΠΕΡΙΦΕΡΕΙΑ ΑΤΤΙΚΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΒΟΡΕΙΟΥ ΤΟΜΕΑ ΑΘΗΝΩΝ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΔΥΤΙΚΟΥ ΤΟΜΕΑ ΑΘΗΝΩΝ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΕΝΤΡΙΚΟΥ ΤΟΜΕΑ ΑΘΗΝΩΝ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΝΟΤΙΟΥ ΤΟΜΕΑ ΑΘΗΝΩΝ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΑΝΑΤΟΛΙΚΗΣ ΑΤΤΙΚΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΔΥΤΙΚΗΣ ΑΤΤΙΚΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΠΕΙΡΑΙΩΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΝΗΣΩΝ",
      # ΒΟΡΕΙΟ ΑΙΓΑΙΟ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΛΕΣΒΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΛΗΜΝΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΙΚΑΡΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΣΑΜΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΧΙΟΥ",
      # ΝΟΤΙΟ ΑΙΓΑΙΟ - single region entry covering both Δωδεκάνησα and Κυκλάδες
      "ΠΕΡΙΦΕΡΕΙΑ ΝΟΤΙΟΥ ΑΙΓΑΙΟΥ",
      # ΚΡΗΤΗ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΗΡΑΚΛΕΙΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΛΑΣΙΘΙΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΡΕΘΥΜΝΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΧΑΝΙΩΝ",
      # ΑΝΑΤΟΛΙΚΗ ΜΑΚΕΔΟΝΙΑ ΚΑΙ ΘΡΑΚΗ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΕΒΡΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΞΑΝΘΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΡΟΔΟΠΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΔΡΑΜΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΘΑΣΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΑΒΑΛΑΣ",
      # ΚΕΝΤΡΙΚΗ ΜΑΚΕΔΟΝΙΑ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΗΜΑΘΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΘΕΣΣΑΛΟΝΙΚΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΙΛΚΙΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΠΕΛΛΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΠΙΕΡΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΣΕΡΡΩΝ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΧΑΛΚΙΔΙΚΗΣ",
      # ΔΥΤΙΚΗ ΜΑΚΕΔΟΝΙΑ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΓΡΕΒΕΝΩΝ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΟΖΑΝΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΑΣΤΟΡΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΦΛΩΡΙΝΑΣ",
      # ΗΠΕΙΡΟΣ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΑΡΤΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΠΡΕΒΕΖΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΘΕΣΠΡΩΤΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΙΩΑΝΝΙΝΩΝ",
      # ΘΕΣΣΑΛΙΑ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΑΡΔΙΤΣΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΤΡΙΚΑΛΩΝ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΛΑΡΙΣΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΜΑΓΝΗΣΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΣΠΟΡΑΔΩΝ",
      # ΙΟΝΙΟΙ ΝΗΣΟΙ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΖΑΚΥΝΘΟΥ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΕΡΚΥΡΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΙΘΑΚΗΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΕΦΑΛΛΗΝΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΛΕΥΚΑΔΑΣ",
      # ΔΥΤΙΚΗ ΕΛΛΑΔΑ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΑΙΤΩΛΟΑΚΑΡΝΑΝΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΑΧΑΪΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΗΛΕΙΑΣ",
      # ΣΤΕΡΕΑ ΕΛΛΑΔΑ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΒΟΙΩΤΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΕΥΒΟΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΕΥΡΥΤΑΝΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΦΘΙΩΤΙΔΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΦΩΚΙΔΑΣ",
      # ΠΕΛΟΠΟΝΝΗΣΟΣ
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΑΡΓΟΛΙΔΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΑΡΚΑΔΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΚΟΡΙΝΘΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΛΑΚΩΝΙΑΣ",
      "ΠΕΡΙΦΕΡΕΙΑΚΗ ΕΝΟΤΗΤΑ ΜΕΣΣΗΝΙΑΣ"
    ),
    NUTS_ID = c(
      # ΑΤΤΙΚΗ
      "EL300", "EL300", "EL300", "EL300", "EL300",
      "EL300", "EL300", "EL300", "EL300",
      # ΒΟΡΕΙΟ ΑΙΓΑΙΟ
      "EL411", "EL411",  # Λέσβος + Λήμνος -> EL411
      "EL412", "EL412",  # Ικαρία + Σάμος  -> EL412
      "EL413",           # Χίος             -> EL413
      # ΝΟΤΙΟ ΑΙΓΑΙΟ - dissolved into EL420
      "EL420",
      # ΚΡΗΤΗ
      "EL431", "EL432", "EL433", "EL434",
      # ΑΝΑΤΟΛΙΚΗ ΜΑΚΕΔΟΝΙΑ ΚΑΙ ΘΡΑΚΗ
      "EL511", "EL512", "EL513", "EL514",
      "EL515", "EL515",  # Θάσος + Καβάλα -> EL515
      # ΚΕΝΤΡΙΚΗ ΜΑΚΕΔΟΝΙΑ
      "EL521", "EL522", "EL523", "EL524", "EL525", "EL526", "EL527",
      # ΔΥΤΙΚΗ ΜΑΚΕΔΟΝΙΑ
      "EL531", "EL531",  # Γρεβενά + Κοζάνη -> EL531
      "EL532",           # Καστοριά          -> EL532
      "EL533",           # Φλώρινα           -> EL533
      # ΗΠΕΙΡΟΣ
      "EL541", "EL541",  # Άρτα + Πρέβεζα   -> EL541
      "EL542",           # Θεσπρωτία         -> EL542
      "EL543",           # Ιωάννινα          -> EL543
      # ΘΕΣΣΑΛΙΑ
      "EL611", "EL611",  # Καρδίτσα + Τρίκαλα   -> EL611
      "EL612",           # Λάρισα                -> EL612
      "EL613", "EL613",  # Μαγνησία + Σποράδες   -> EL613
      # ΙΟΝΙΟΙ ΝΗΣΟΙ
      "EL621",           # Ζάκυνθος          -> EL621
      "EL622",           # Κέρκυρα           -> EL622
      "EL623", "EL623",  # Ιθάκη + Κεφαλληνία -> EL623
      "EL624",           # Λευκάδα           -> EL624
      # ΔΥΤΙΚΗ ΕΛΛΑΔΑ
      "EL631", "EL632", "EL633",
      # ΣΤΕΡΕΑ ΕΛΛΑΔΑ
      "EL641", "EL642", "EL643", "EL644", "EL645",
      # ΠΕΛΟΠΟΝΝΗΣΟΣ
      "EL651", "EL651",  # Αργολίδα + Αρκαδία  -> EL651
      "EL652",           # Κορινθία             -> EL652
      "EL653", "EL653"   # Λακωνία + Μεσσηνία  -> EL653
    ),
    stringsAsFactors = FALSE
  )
}

#' map UI Function
#' @importFrom shiny NS tagList
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h4("Geographic Distribution"),
        shiny::helpText("Interactive map of Greece by Regional Units.")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(
          ns("map_metric"),
          "Select Metric to Map:",
          choices = c(
            "Total Prescriptions"    = "total_prescriptions",
            "Unique Patients"        = "unique_patients",
            "Prescriptions per 100K" = "prescriptions_per_100k",
            "Patients per 100K"      = "patients_per_100k"
          ),
          selected = "total_prescriptions"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          leaflet::leafletOutput(ns("greece_map"), height = "650px")
        )
      )
    )
  )
}

#' map Server Functions
mod_map_server <- function(id, data_reactive, con) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Require sf to explicitly load S3 spatial methods for Leaflet
    require(sf)

    # 1. Fetch map data via eurostat, dissolve Attica into a single polygon
    map_data_sf <- shiny::reactiveVal(NULL)
    shiny::observe({
      gr_nuts3 <- eurostat::get_eurostat_geospatial(
        resolution = "20",
        nuts_level = "3",
        year       = "2021"
      ) |>
        dplyr::filter(CNTR_CODE == "EL") |>
        sf::st_cast("MULTIPOLYGON") |>
        sf::st_make_valid()

      # Dissolve all Attica NUTS3 units into a single polygon keyed as EL300
      attica_ids <- c("EL301", "EL302", "EL303", "EL304", "EL305", "EL306", "EL307")

      attica_dissolved <- gr_nuts3 |>
        dplyr::filter(NUTS_ID %in% attica_ids) |>
        dplyr::summarise(
          NUTS_ID   = "EL300",
          NUTS_NAME = "\u0391\u03c4\u03c4\u03b9\u03ba\u03ae",  # Αττική
          geometry  = sf::st_union(geometry)
        ) |>
        sf::st_as_sf()

      # Dissolve Δωδεκάνησα + Κυκλάδες into a single South Aegean polygon keyed as EL420
      south_aegean_ids <- c("EL421", "EL422")

      south_aegean_dissolved <- gr_nuts3 |>
        dplyr::filter(NUTS_ID %in% south_aegean_ids) |>
        dplyr::summarise(
          NUTS_ID   = "EL420",
          NUTS_NAME = "\u039d\u03cc\u03c4\u03b9\u03bf \u0391\u03b9\u03b3\u03b1\u03af\u03bf",  # Νότιο Αιγαίο
          geometry  = sf::st_union(geometry)
        ) |>
        sf::st_as_sf()

      gr_nuts3_merged <- gr_nuts3 |>
        dplyr::filter(!NUTS_ID %in% c(attica_ids, south_aegean_ids)) |>
        dplyr::bind_rows(attica_dissolved, south_aegean_dissolved) |>
        sf::st_cast("MULTIPOLYGON") |>
        sf::st_make_valid()

      map_data_sf(gr_nuts3_merged)
    })

    # 2. Compute Independent Regional Denominator based on current active filters
    map_denominator <- shiny::reactive({
      data <- data_reactive()
      shiny::req(nrow(data) > 0)

      sex_vals       <- unique(data$sex[!is.na(data$sex)])
      age_group_vals <- unique(data$age_group[!is.na(data$age_group)])

      sex_ids <- if (length(sex_vals) > 0) {
        unlist(get_field_ids(con, "sex", sex_vals))
      } else {
        "all"
      }
      age_group_ids <- if (length(age_group_vals) > 0) {
        unlist(get_field_ids(con, "age_group", age_group_vals))
      } else {
        "all"
      }

      sql_query <- generate_sql_query(
        table     = "population_by_region",
        age_group = age_group_ids,
        sex       = sex_ids,
        region    = "all"
      )

      denom_raw <- DBI::dbGetQuery(con, sql_query)

      denom_agg <- denom_raw |>
        dplyr::group_by(region_id) |>
        dplyr::summarise(population = sum(group_total, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(region_id = as.integer(region_id))

      if (nrow(denom_agg) > 0) {
        denom_agg$region <- get_field_names(con, "region", denom_agg$region_id)
      } else {
        denom_agg$region <- character()
      }

      denom_agg
    })

    # 3. Build Mapping Table from hardcoded lookup
    mapping_table <- shiny::reactive({
      data <- data_reactive()
      shiny::req(data)

      db_regions <- unique(data$region)
      db_regions <- db_regions[!is.na(db_regions) & db_regions != ""]

      if (length(db_regions) == 0) {
        return(data.frame(
          db_region = character(),
          NUTS_ID   = character(),
          stringsAsFactors = FALSE
        ))
      }

      lookup <- get_region_nuts_mapping()
      lookup[lookup$db_region %in% db_regions, ]
    })

    # 4. Aggregate active data and join to the independent map denominator
    region_aggregated_data <- shiny::reactive({
      data     <- data_reactive()
      pop_data <- map_denominator()

      shiny::req(nrow(data) > 0, "region" %in% names(data))

      summary_data <- data |>
        dplyr::group_by(region) |>
        dplyr::summarise(
          total_prescriptions = sum(total_prescriptions, na.rm = TRUE),
          unique_patients     = sum(unique_patients,     na.rm = TRUE),
          .groups = "drop"
        )

      if ("region" %in% names(pop_data) && nrow(pop_data) > 0) {
        summary_data <- summary_data |>
          dplyr::left_join(pop_data, by = "region") |>
          dplyr::mutate(
            prescriptions_per_100k = (total_prescriptions / population) * 100000,
            patients_per_100k      = (unique_patients     / population) * 100000
          )
      } else {
        summary_data$prescriptions_per_100k <- NA_real_
        summary_data$patients_per_100k      <- NA_real_
      }

      summary_data
    })

    # 5. Render Leaflet Map
    output$greece_map <- leaflet::renderLeaflet({
      sf_data <- map_data_sf()
      r_data  <- region_aggregated_data()
      mapping <- mapping_table()
      metric  <- input$map_metric

      shiny::req(sf_data, r_data, mapping)

      # Join aggregated data to NUTS_ID via lookup, then sum per NUTS polygon
      # to correctly collapse multiple DB regions sharing one NUTS unit
      r_data_mapped <- r_data |>
        dplyr::left_join(mapping, by = c("region" = "db_region")) |>
        dplyr::filter(!is.na(NUTS_ID)) |>
        dplyr::group_by(NUTS_ID) |>
        dplyr::summarise(
          total_prescriptions    = sum(total_prescriptions, na.rm = TRUE),
          unique_patients        = sum(unique_patients,     na.rm = TRUE),
          population             = sum(population,          na.rm = TRUE),
          prescriptions_per_100k = (sum(total_prescriptions, na.rm = TRUE) /
                                      sum(population,          na.rm = TRUE)) * 100000,
          patients_per_100k      = (sum(unique_patients,     na.rm = TRUE) /
                                      sum(population,          na.rm = TRUE)) * 100000,
          .groups = "drop"
        )

      map_joined <- sf_data |>
        dplyr::left_join(r_data_mapped, by = "NUTS_ID") |>
        sf::st_as_sf() |>
        sf::st_cast("MULTIPOLYGON") |>
        sf::st_make_valid()

      map_joined$plot_metric <- as.numeric(unlist(map_joined[[metric]]))
      map_joined$plot_metric[is.infinite(map_joined$plot_metric)] <- NA

      if (all(is.na(map_joined$plot_metric))) {
        return(
          leaflet::leaflet() |>
            leaflet::addTiles() |>
            leaflet::addPopups(
              lng = 23.7, lat = 37.9,
              popup = "No data available for the selected metric."
            )
        )
      }

      pal <- leaflet::colorNumeric(
        palette  = "YlOrRd",
        domain   = map_joined$plot_metric,
        na.color = "#e8e8e8"
      )

      poly_colors <- pal(map_joined$plot_metric)

      metric_label <- switch(
        metric,
        "total_prescriptions"    = "Total Prescriptions",
        "unique_patients"        = "Unique Patients",
        "prescriptions_per_100k" = "Prescriptions per 100K",
        "patients_per_100k"      = "Patients per 100K",
        metric
      )

      labels <- sprintf(
        "<strong>%s</strong><br/>%s: %s",
        map_joined$NUTS_NAME,
        metric_label,
        ifelse(
          is.na(map_joined$plot_metric),
          "No data",
          format(round(map_joined$plot_metric, 1), big.mark = ",")
        )
      ) |> lapply(htmltools::HTML)

      leaflet::leaflet(data = map_joined) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          fillColor   = poly_colors,
          weight      = 1,
          opacity     = 1,
          color       = "white",
          dashArray   = "3",
          fillOpacity = 0.7,
          highlightOptions = leaflet::highlightOptions(
            weight       = 3,
            color        = "#666",
            dashArray    = "",
            fillOpacity  = 0.9,
            bringToFront = TRUE
          ),
          label        = labels,
          labelOptions = leaflet::labelOptions(
            style     = list("font-weight" = "normal", padding = "3px 8px"),
            textsize  = "15px",
            direction = "auto"
          )
        ) |>
        leaflet::addLegend(
          pal      = pal,
          values   = map_joined$plot_metric,
          opacity  = 0.7,
          title    = metric_label,
          position = "bottomright"
        )
    })

  })
}
