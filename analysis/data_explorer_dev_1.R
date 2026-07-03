# Load required libraries
library(shiny)
library(dplyr)
library(tidyverse)
library(here)

# Load and setup data ----
data_path <- here("data/final_data_working_paper_1.RData")
load(data_path)

# Correct top journal classification for "Quantitative Economics" (Rank 47 in SJR 2022)
data <- data %>% 
  mutate(is_top_tier = ifelse(`publication title` == "Quantitative Economics", 1, top_5_or_tier))

# Define stylized time horizons ----
vsr <- 1
smr <- 12
mlr <- 36
horizons <- c(
  paste0("impact (0m)"),
  paste0("short run (", vsr, "m - ", smr, "m)"),
  paste0("medium run (", smr + 1, "m - ", mlr, "m)"),
  paste0("long run (> ", mlr, "m)")
)

# Helper function for data summary table
filtered_data_summary <- function(data) {
  # Remove duplicates for unique counting
  unique_studies <- data %>% 
    distinct(key) %>% 
    nrow()
  
  unique_models <- data %>% 
    distinct(model_id) %>% 
    nrow()
  
  # Create summary table
  summary_table <- data.frame(
    Metric = c("Observations", "Studies", "Models"),
    Count = c(nrow(data), unique_studies, unique_models),
    stringsAsFactors = FALSE
  )
  
  return(summary_table)
}

# Define UI
ui <- fluidPage(
  titlePanel("META CMP Data Explorer - Simplified"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Current selection"),
        tableOutput("filteredDataSummary")
      ),
      
      h4("Response Variable Filters"),
      
      selectInput("filter_outcome", 
                  "Show data for specific response variable", 
                  choices = c("All", unique(data$outcome)), 
                  selected = "All"),
      
      selectInput("filter_transformation", 
                  "Select the transformation of the response variable", 
                  choices = c("All", unique(data$transformation)), 
                  selected = "All"),
      
      selectInput("filter_periodicity", 
                  "Select the periodicity of the response variable", 
                  choices = c("All", unique(data$periodicity)), 
                  selected = "All"),
      
      conditionalPanel(
        condition = "input.filter_outcome != 'All'",
        selectInput("filter_outcome_measure", 
                    "Select the outcome measure of the response variable", 
                    choices = c("All", unique(data$outcome_measure)), 
                    selected = "All")
      )
    ),
    
    mainPanel(
      h3("Data Explorer"),
      p("This simplified version displays filtered data statistics based on response variable selections."),
      br(),
      h4("Filter Summary"),
      verbatimTextOutput("filterSummary")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    data_filtered <- data
    
    # Outcome variable filter
    if (input$filter_outcome != "All") {
      data_filtered <- data_filtered %>% 
        filter(outcome == input$filter_outcome)
    }
    
    # Outcome measure filter (only applies when specific outcome is selected)
    if (input$filter_outcome != "All" && input$filter_outcome_measure != "All") {
      data_filtered <- data_filtered %>% 
        filter(outcome_measure == input$filter_outcome_measure)
    }
    
    # Transformation filter
    if (input$filter_transformation != "All") {
      data_filtered <- data_filtered %>% 
        filter(transformation == input$filter_transformation)
    }
    
    # Periodicity filter
    if (input$filter_periodicity != "All") {
      data_filtered <- data_filtered %>% 
        filter(periodicity == input$filter_periodicity)
    }
    
    return(data_filtered)
  })
  
  # Update outcome_measure choices based on selected outcome
  observe({
    if (input$filter_outcome != "All") {
      outcome_measures <- c("All", unique(data$outcome_measure[data$outcome == input$filter_outcome]))
    } else {
      outcome_measures <- c("All", unique(data$outcome_measure))
    }
    updateSelectInput(session, "filter_outcome_measure", 
                      choices = outcome_measures, 
                      selected = input$filter_outcome_measure)
  })
  
  # Update transformation choices based on selected outcome
  observe({
    if (input$filter_outcome != "All") {
      transformations <- c("All", unique(data$transformation[data$outcome == input$filter_outcome]))
    } else {
      transformations <- c("All", unique(data$transformation))
    }
    updateSelectInput(session, "filter_transformation", 
                      choices = transformations, 
                      selected = input$filter_transformation)
  })
  
  # Update periodicity choices based on selected outcome
  observe({
    if (input$filter_outcome != "All") {
      periodicities <- c("All", unique(data$periodicity[data$outcome == input$filter_outcome]))
    } else {
      periodicities <- c("All", unique(data$periodicity))
    }
    updateSelectInput(session, "filter_periodicity", 
                      choices = periodicities, 
                      selected = input$filter_periodicity)
  })
  
  # Render data summary table
  output$filteredDataSummary <- renderTable({
    filtered_data_summary(filtered_data())
  }, align = "l")
  
  # Render filter summary
  output$filterSummary <- renderText({
    summary <- "Active Filters:\n"
    
    if (input$filter_outcome != "All") {
      summary <- paste0(summary, "- Response Variable: ", input$filter_outcome, "\n")
    }
    
    if (input$filter_transformation != "All") {
      summary <- paste0(summary, "- Transformation: ", input$filter_transformation, "\n")
    }
    
    if (input$filter_periodicity != "All") {
      summary <- paste0(summary, "- Periodicity: ", input$filter_periodicity, "\n")
    }
    
    if (input$filter_outcome != "All" && input$filter_outcome_measure != "All") {
      summary <- paste0(summary, "- Outcome Measure: ", input$filter_outcome_measure, "\n")
    }
    
    if (summary == "Active Filters:\n") {
      summary <- paste0(summary, "No filters applied - showing all data")
    }
    
    summary
  })
}

# Run the application
shinyApp(ui = ui, server = server)