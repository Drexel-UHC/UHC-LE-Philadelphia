{
  ## Dependencies
  library(shiny)
  library(plotly)
  library(dplyr)
  library(tidyverse)
  library(highcharter)
  library(sf)
  library(RColorBrewer)
  library(reactable)
  library(tmap)
  library(ggiraph)
  
  ## Helpers (for now you can just source files but eventually refactor to modules is best for maintainability)
  source("R/helpers/le_by_decile_plots.R")
  source("R/helpers/le_maps.R")
  }

ui <- fluidPage(
  tags$head(tags$style(HTML(".leaflet-container {background: none;}"))),
  tags$head(includeCSS("CSS/Header.css")),
  tags$head(includeCSS("CSS/NavbarPage.css")),
  tags$head(includeCSS("CSS/Home.css")),
  tags$head(includeCSS("CSS/figures.css")),
  tags$head(includeHTML("HTML/FontAwesomeLoader.html")),
  includeHTML("HTML/Header.html"),
  navbarPage( id = 'navbar',
              title = "Title",
              tabPanel("Project Details",includeHTML("HTML/Home.html")),
              tabPanel("Tables", 
                       fluidRow(
                         column(width = 3,
                                div(style = "margin-bottom: 60px;",
                                    selectInput(
                                      choices = c("Total Disparities", "Income-Based Disparities"),
                                      inputId="disparity_type",
                                      label="Disparity Type")
                                    )
                                )
                         ),
                       reactableOutput("table")),
              tabPanel("Plots", 
                       sidebarPanel(
                         selectInput(choices=c("Mean", "Standard Deviation"),
                                       inputId="LE_plots", label="Select a statistic"),
                         selectInput(choices=c(0,25,65),
                                     inputId="LE_plots_age", label="Age"),
                         selectInput(choices=c("2017-2019", "2020-2022"),
                                     inputId="LE_plots_years", label="Years")),
                       mainPanel(
                         girafeOutput("le_by_decile_ggiraph", width="800px", height="600px")
                         )
                       ),
              tabPanel("Maps",
                       fluidRow(
                         column(width = 3,
                                div(style = "margin-bottom: 60px;",
                                    selectInput(
                                      choices = c("Life Expectancy at Birth",
                                                  "Change in LE at Birth",
                                                  "Posterior Probability of a Decrease in LE"),
                                      inputId="LE_maps_measure",
                                      label="Measure"
                                    ),
                                    conditionalPanel(
                                      condition = "input.LE_maps_measure == 'Life Expectancy at Birth'",
                                      selectInput(
                                        choices = c("2017-2019", "2020-2022"),
                                        inputId = "LE_maps_years", 
                                        label = "Years"))))),
                       
                       # Spacer div adding vertical space
                       tags$div(style = "margin-top: 20px;"),
                       
                       
                       fluidRow(
                         column(width = 6,
                                tmapOutput('map_male', height = "600px")
                         ),
                         column(width = 6,
                                tmapOutput('map_female', height = "600px"))))),
  includeHTML("HTML/Footer.html")
  )
 

server <- function(input, output, session) {
  
  #Load data
  load("Data/summary_table.Rdata")
  load("Data/income_ineq_table.Rdata")
  load("Data/le_birth.Rdata")
  load("Data/le_birth_change.Rdata")
  load("Data/le_by_decile.Rdata")
  load("Data/post_prob_sf.Rdata")
  
  # Render disparities table
  output$table <- renderReactable({
    if (input$disparity_type == "Total Disparities") {
      reactable(
        summary_table %>%
          rename("Gender" = "Sex",
                 "Absolute Disparity" = "AbsDif",
                 "Relative Disparity" = "RelDif",
                 "Coefficient of Variation" = "CV",
                 "Gini Coefficient" = "Gini",
                 "Mean Log Deviation" = "MLD") %>%
          mutate(Gender = case_when(
            Gender == "Male" ~ "Men",
            Gender == "Female" ~ "Women"
          )),
        columnGroups = list(
          colGroup(
            name = "Total Disparities",
            headerClass = "reactableTopTierTitle reactableTopTierTitle1",
            headerStyle = "border-left: #c7c7c7 solid 1px;",
            columns = c("Age", "Years", "Gender", "Absolute Disparity", 
                        "Relative Disparity", "Coefficient of Variation", 
                        "Gini Coefficient", "Mean Log Deviation")
          )
        ), 
        defaultPageSize = 12,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE, 
        defaultColDef = colDef(
          align = "center"
        ),
      )
    } else if (input$disparity_type == "Income-Based Disparities") {
      reactable(
        income_ineq_table %>%
          rename("Gender" = "Sex",
                 "Absolute Disparity" = "AbsDif",
                 "Relative Disparity" = "RelDif",
                 "Slope Index of Inequality" = "SII",
                 "Relative Index of Inequality" = "RII") %>%
          mutate(Gender = case_when(
            Gender == "Male" ~ "Men",
            Gender == "Female" ~ "Women"
          )),
        columnGroups = list(
          colGroup(
            name = "Income Based Disparities",
            headerClass = "reactableTopTierTitle reactableTopTierTitle1",
            headerStyle = "border-left: #c7c7c7 solid 1px;",
            columns = c("Age", "Years", "Gender", "Absolute Disparity", "Relative Disparity", 
                        "Slope Index of Inequality", "Relative Index of Inequality")
          )
        ), 
        defaultPageSize = 12,
        striped = TRUE,
        highlight = TRUE,
        bordered = TRUE, 
        defaultColDef = colDef(
          align = "center"
        ),
      )
    } else {
      NULL
    }
  })
  
  # Render LE by decile plot
  output$le_by_decile_ggiraph <- renderGirafe(
    decile_plots(le_by_decile, input$LE_plots, input$LE_plots_age, input$LE_plots_years)
  )
  
  # Get data for map
  map_data <- reactive ({
    if(input$LE_maps_measure == "Life Expectancy at Birth") {
      le_birth
    } else if (input$LE_maps_measure == "Change in LE at Birth") {
      le_birth_change
    } else if (input$LE_maps_measure == "Posterior Probability of a Decrease in LE") {
      post_prob_sf
    } else {
      NULL
    }
  })
  
  # Render maps using tmap
  tmap_mode("view")
  
  output$map_male <- renderTmap({
    le_map(data = map_data(), measure = input$LE_maps_measure, yrs = input$LE_maps_years, sex_input = "Male") 
  })
  
  output$map_female <- renderTmap({
    le_map(data = map_data(), measure = input$LE_maps_measure, yrs = input$LE_maps_years, sex_input = "Female")
  })
  
}

shinyApp(ui, server)

