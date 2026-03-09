#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),

    shinydashboardPlus::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = golem::get_golem_options("app_title")
      ),

      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "menu1",
          shinydashboard::menuItem(
            tabName = "overall",
            text = "Overall data",
            icon = shiny::icon("chart-simple")
          ),
          shinydashboard::menuItem(
            tabName = "subgroup_analysis",
            text = "Subgroup analysis",
            icon = shiny::icon("magnifying-glass-chart")
          ),
          shinydashboard::menuItem(
            tabName = "code_lookup",
            text = "Code Definitions",
            icon = shiny::icon("book-medical")
          )
        )
      ),

      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(

          # ── Tab 1: Overall data ──────────────────────────────────────────
          shinydashboard::tabItem(
            tabName = "overall",
            shiny::h2("Overall data"),

            # Filters
            mod_data_filters_ui("filters_overall"),

            # Raw data table
            mod_raw_table_ui("raw_table_1"),

            shiny::hr(),

            # ── Code Groups (setup for downstream analyses) ──────────────
            shiny::h3(
              shiny::icon("layer-group"),
              " Code Groups",
              style = "margin-top: 20px;"
            ),
            shiny::p(
              shiny::icon("circle-info"),
              "Define groups of ATC drug codes and ICD-10 disease codes here.",
              "When groups are defined, they replace individual codes in all",
              shiny::strong("Subgroup Analysis"), "tabs."
            ),
            mod_groups_ui("groups")
          ),

          # ── Tab 2: Subgroup analysis ─────────────────────────────────────
          shinydashboard::tabItem(
            tabName = "subgroup_analysis",
            shiny::h2("Subgroup analysis"),
            mod_grouping_selector_ui("grouping_selector"),
            shiny::br(),
            shiny::tabsetPanel(
              id = "subgroup_tabs",
              type = "tabs",

              shiny::tabPanel(
                title = "General",
                value = "general",
                shiny::br(),
                mod_summary_table_ui("summary_table")
              ),

              shiny::tabPanel(
                title = "Time Series",
                value = "time_series",
                shiny::br(),
                mod_time_series_ui("time_series")
              ),

              shiny::tabPanel(
                title = "Map",
                value = "map",
                shiny::br(),
                mod_map_ui("map_view")
              ),

              shiny::tabPanel(
                title = "Heatmap",
                value = "heatmap",
                shiny::br(),
                mod_heatmap_ui("heatmap_view")
              )
            )
          ),

          # ── Tab 3: Code Definitions ──────────────────────────────────────
          shinydashboard::tabItem(
            tabName = "code_lookup",
            shiny::h2("Code Definitions"),
            mod_code_lookup_ui("code_lookup")
          )
        )
      ),

      controlbar = shinydashboardPlus::dashboardControlbar(),

      title = ""
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    'www', golem::app_sys('app/www')
  )
  
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = golem::app_sys('app/www'),
      app_title = 'IdikaExplorer'
    ),
    shiny::tags$style(htmltools::HTML("
      body {
        font-family: 'Roboto', 'Helvetica Neue', Arial, sans-serif;
      }
      .dataTables_wrapper {
        width: 100% !important;
      }
      .dataTables_scrollBody {
        width: 100% !important;
      }
      table.dataTable {
        width: 100% !important;
        margin: 0 auto;
      }
      .tab-content {
        overflow: visible !important;
      }
    ")),
    shiny::tags$script(htmltools::HTML("
      $(document).ready(function() {
        $('.sidebar-menu li a').on('click', function() {
          setTimeout(function() {
            if ($.fn.DataTable) {
              $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
            }
          }, 200);
        });
      });
    "))
  )
}
