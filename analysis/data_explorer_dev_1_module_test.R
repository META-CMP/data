# test_response_filter_module.R
# Standalone test app for the response filter module

library(shiny)
library(dplyr)
library(tidyverse)
library(here)

# Source the module (adjust path as needed)
# source("modules/mod_response_filter.R")

# For testing, I'll include the module code here
# In practice, you'd source it from a separate file

# ===== MODULE CODE (normally in separate file) =====
responseFilterUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Response Variable Filters"),
    
    selectInput(
      ns("outcome"),
      "Show data for specific response variable",
      choices = c("All"),
      selected = "All"
    ),
    
    selectInput(
      ns("transformation"),
      "Select the transformation of the response variable",
      choices = c("All"),
      selected = "All"
    ),
    
    selectInput(
      ns("periodicity"),
      "Select the periodicity of the response variable",
      choices = c("All"),
      selected = "All"
    ),
    
    conditionalPanel(
      condition = sprintf("input['%s'] != 'All'", ns("outcome")),
      selectInput(
        ns("outcome_measure"),
        "Select the outcome measure of the response variable",
        choices = c("All"),
        selected = "All"
      )
    )
  )
}

responseFilterServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Update choices when data changes
    observe({
      req(data())
      current_data <- data()
      
      updateSelectInput(
        session, "outcome",
        choices = c("All", unique(current_data$outcome)),
        selected = input$outcome
      )
      
      updateSelectInput(
        session, "transformation",
        choices = c("All", unique(current_data$transformation)),
        selected = input$transformation
      )
      
      updateSelectInput(
        session, "periodicity",
        choices = c("All", unique(current_data$periodicity)),
        selected = input$periodicity
      )
    })
    
    # Update outcome_measure choices based on selected outcome
    observe({
      req(data())
      current_data <- data()
      
      if (input$outcome != "All") {
        outcome_measures <- c("All", 
                              unique(current_data$outcome_measure[current_data$outcome == input$outcome]))
      } else {
        outcome_measures <- c("All", unique(current_data$outcome_measure))
      }
      
      updateSelectInput(
        session, "outcome_measure",
        choices = outcome_measures,
        selected = input$outcome_measure
      )
    })
    
    # Apply filters to data
    filtered_data <- reactive({
      req(data())
      data_filtered <- data()
      
      if (input$outcome != "All") {
        data_filtered <- data_filtered %>%
          filter(outcome == input$outcome)
      }
      
      if (input$outcome != "All" && input$outcome_measure != "All") {
        data_filtered <- data_filtered %>%
          filter(outcome_measure == input$outcome_measure)
      }
      
      if (input$transformation != "All") {
        data_filtered <- data_filtered %>%
          filter(transformation == input$transformation)
      }
      
      if (input$periodicity != "All") {
        data_filtered <- data_filtered %>%
          filter(periodicity == input$periodicity)
      }
      
      data_filtered
    })
    
    # Generate filter summary
    filter_summary <- reactive({
      summary_parts <- character()
      
      if (input$outcome != "All") {
        summary_parts <- c(summary_parts, 
                           paste("Response Variable:", input$outcome))
      }
      
      if (input$transformation != "All") {
        summary_parts <- c(summary_parts, 
                           paste("Transformation:", input$transformation))
      }
      
      if (input$periodicity != "All") {
        summary_parts <- c(summary_parts, 
                           paste("Periodicity:", input$periodicity))
      }
      
      if (input$outcome != "All" && input$outcome_measure != "All") {
        summary_parts <- c(summary_parts, 
                           paste("Outcome Measure:", input$outcome_measure))
      }
      
      if (length(summary_parts) == 0) {
        return("No filters applied - showing all data")
      } else {
        return(paste(summary_parts, collapse = "\n"))
      }
    })
    
    # Return both filtered data and summary
    return(list(
      filtered_data = filtered_data,
      filter_summary = filter_summary
    ))
  })
}

# ===== END MODULE CODE =====

# ===== TEST APP CODE =====

# Load and prepare data
data_path <- here("data/final_data_working_paper_1.RData")
load(data_path)

# Apply data corrections
data <- data %>% 
  mutate(is_top_tier = ifelse(`publication title` == "Quantitative Economics", 1, top_5_or_tier))

# Define UI for test app
ui <- fluidPage(
  titlePanel("Module Test: Response Filter"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      # Call the module UI
      responseFilterUI("response_filter_1")
    ),
    
    mainPanel(
      width = 8,
      h3("Module Outputs"),
      
      # Display filter summary
      wellPanel(
        h4("Active Filters"),
        verbatimTextOutput("filter_summary")
      ),
      
      # Display data summary
      wellPanel(
        h4("Data Summary"),
        tableOutput("data_summary")
      ),
      
      # Display first few rows of filtered data
      wellPanel(
        h4("Sample of Filtered Data"),
        p("First 5 rows, selected columns:"),
        tableOutput("sample_data")
      )
    )
  )
)

# Define server for test app
server <- function(input, output, session) {
  
  # Wrap data in reactive for module
  data_reactive <- reactive({ data })
  
  # Call the module server and store returned values
  filter_results <- responseFilterServer("response_filter_1", data_reactive)
  
  # Display filter summary from module
  output$filter_summary <- renderText({
    filter_results$filter_summary()
  })
  
  # Display data summary
  output$data_summary <- renderTable({
    filtered_data <- filter_results$filtered_data()
    
    # Count unique studies and models
    unique_studies <- filtered_data %>% 
      distinct(key) %>% 
      nrow()
    
    unique_models <- filtered_data %>% 
      distinct(model_id) %>% 
      nrow()
    
    data.frame(
      Metric = c("Total Observations", "Unique Studies", "Unique Models"),
      Count = c(nrow(filtered_data), unique_studies, unique_models),
      stringsAsFactors = FALSE
    )
  }, align = "l")
  
  # Display sample of filtered data
  output$sample_data <- renderTable({
    filtered_data <- filter_results$filtered_data()
    
    # Select a few relevant columns and first 5 rows
    filtered_data %>%
      select(key, model_id, outcome, transformation, periodicity, outcome_measure) %>%
      head(5)
  })
}

# Run the test app
shinyApp(ui = ui, server = server)