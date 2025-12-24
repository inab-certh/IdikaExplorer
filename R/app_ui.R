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
    # Golem's resource management (stays the same)
    golem_add_external_resources(),

    shinydashboardPlus::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = golem::get_golem_options("app_title")  # From config
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
          shinydashboard::tabItem(
            tabName = "overall",
            shiny::h2("Overall data"),
            mod_data_filters_ui("filters_overall"),
            mod_raw_table_ui("raw_table_1")
          ),
          shinydashboard::tabItem(
            tabName = "subgroup_analysis",
            shiny::h2("Subgroup analysis"),
            mod_grouping_selector_ui("grouping_selector"),
            mod_summary_table_ui("summary_table")
          ),
          shinydashboard::tabItem(
            tabName = "code_lookup",
            shiny::h2("Code Definitions"),
            mod_code_lookup_ui("code_lookup")
          )
        )
      ),

      # shinydashboardPlus exclusive features
      controlbar = shinydashboardPlus::dashboardControlbar(),

      title = ""
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
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
    # Custom CSS for Greek fonts and DataTable fixes
    shiny::tags$style(htmltools::HTML("
      body {
        font-family: 'Roboto', 'Helvetica Neue', Arial, sans-serif;
      }
      
      /* Fix DataTable alignment issues */
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
    # JavaScript to recalculate DataTables on tab switch
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
