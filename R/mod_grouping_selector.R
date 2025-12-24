#' grouping_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_grouping_selector_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h4("Select grouping variables:"),
        shiny::helpText("Only variables with 2+ unique values in the filtered data are available")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        2,
        shiny::checkboxInput(
          ns("group_prescription_insert_year"),
          "Group by prescription year",
          value = FALSE
        )
      ),
      shiny::column(
        2,
        shiny::checkboxInput(
          ns("group_age_group"),
          "Group by Age Group",
          value = FALSE
        )
      ),
      shiny::column(
        2,
        shiny::checkboxInput(
          ns("group_sex"),
          "Group by Sex",
          value = FALSE
        )
      ),
      shiny::column(
        2,
        shiny::checkboxInput(
          ns("group_region"),
          "Group by region",
          value = FALSE
        )
      ),
      shiny::column(
        2,
        shiny::checkboxInput(
          ns("group_atc_code"),
          "Group by ATC Code",
          value = FALSE
        )
      )
    ),
    shiny::br()
  )
}


    
#' grouping_selector Server Functions
#'
#' @noRd 
mod_grouping_selector_server <- function(id, filtered_data_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    available_groups <- shiny::reactive({
      data <- filtered_data_reactive()
      shiny::req(nrow(data) > 0)
      
      available <- list(
        prescription_insert_year = length(unique(data$prescription_insert_year)) >= 2,
        age_group = length(unique(data$age_group)) >= 2,
        sex = length(unique(data$sex)) >= 2,
        region = length(unique(data$region)) >= 2,
        atc_code = length(unique(data$atc_code)) >= 2
      )
      
      
      available
    })
    
    # Enable/disable checkboxes based on available groups
    
    # Return selected grouping variables
    selected_groups <- shiny::reactive({
      groups <- c()
      if (input$group_prescription_insert_year) groups <- c(groups, "prescription_insert_year")
      if (input$group_age_group) groups <- c(groups, "age_group")
      if (input$group_sex) groups <- c(groups, "sex")
      if (input$group_region) groups <- c(groups, "region")
      if (input$group_atc_code) groups <- c(groups, "atc_code")
      
      message("Selected grouping variables: ", paste(groups, collapse = ", "))
      groups
    })
    
    return(selected_groups)
  })
}
