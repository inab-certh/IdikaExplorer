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
    # Custom CSS for Greek fonts if needed
    shiny::tags$style(htmltools::HTML("
      body {
        font-family: 'Roboto', 'Helvetica Neue', Arial, sans-serif;
      }
    "))
  )
}
