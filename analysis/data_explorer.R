# Load necessary libraries
library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(here)

# Load the data
data_path <- here("data/preliminary_data.RData")
load(data_path)

# setwd("~/data")
# #Load data by running data_prep script
# source("analysis/data_prep.R")
# remove(rate)

# ---- THIS SHOULD SOON BE DONE DIRECTLY IN THE PACKAGE ----
# Calculate new confidence bounds for 68%, 90%, and 95% intervals
crit_val_68 <- qnorm(0.84)  # crit_val for 68% confidence interval
crit_val_90 <- qnorm(0.95)  # crit_val for 90% confidence interval
crit_val_95 <- qnorm(0.975)  # crit_val for 95% confidence interval
data$approx.CI.lower_68 <- data$mean.effect - crit_val_68 * data$SE.lower
data$approx.CI.upper_68 <- data$mean.effect + crit_val_68 * data$SE.upper
data$approx.CI.lower_90 <- data$mean.effect - crit_val_90 * data$SE.lower
data$approx.CI.upper_90 <- data$mean.effect + crit_val_90 * data$SE.upper
data$approx.CI.lower_95 <- data$mean.effect - crit_val_95 * data$SE.lower
data$approx.CI.upper_95 <- data$mean.effect + crit_val_95 * data$SE.upper

# ---- THIS SHOULD SOON BE DONE DIRECTLY IN THE PACKAGE ----
# Extracting start and end year
# Create a function to extract the year from the start and end columns
extract_year <- function(date_str) {
  if (grepl("-", date_str)) {
    year <- sub(".*-(\\d{4})$", "\\1", date_str)
  } else {
    year <- date_str
  }
  return(year)
}
# Apply the function to create the new columns
data$start_year <- sapply(data$start, extract_year)
data$end_year <- sapply(data$end, extract_year)
# Convert the extracted years to numeric
data$start_year <- as.numeric(data$start_year)
data$end_year <- as.numeric(data$end_year)
# Check the result
data$end_year

ui <- fluidPage(
  titlePanel("Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("filter_month", "Filter to show only monthly data", value = FALSE),
      checkboxInput("filter_quarter", "Filter to show only quarterly data", value = FALSE),
      checkboxInput("filter_annual", "Filter to show only annual data", value = FALSE),
      selectInput("filter_outcome", "Filter to show specific outcome", choices = c("All", unique(data$outcome)), selected = "All"),
      sliderInput("filter_years", "Filter for study sample years",
                  min = min(data$start_year, na.rm = TRUE),
                  max = max(data$end_year, na.rm = TRUE),
                  value = c(min(data$start_year, na.rm = TRUE), max(data$end_year, na.rm = TRUE)),
                  sep = ""),
      selectInput("country_filter_type", "Country Filter Type:",
                  choices = c("All countries" = "all",
                              "Unique combinations in primary studies" = "specific_country",
                              "One country only" = "one_country",
                              "Country in sample" = "country_in_sample",
                              "Country group only" = "country_group_only",
                              "Country group in sample" = "country_group_in_sample",
                              "Exclude countries" = "exclude_countries"),
                  selected = "all"),
      
      conditionalPanel(
        condition = "input.country_filter_type == 'specific_country'",
        selectInput("filter_country", "Show data only for specific unique combinations in the primary studies",
                    choices = c("All", unique(data$list_of_countries)), selected = "All")
      ),
      
      conditionalPanel(
        condition = "input.country_filter_type == 'one_country'",
        textInput("one_country", "Filter models with only one specific country")
      ),
      
      conditionalPanel(
        condition = "input.country_filter_type == 'country_in_sample'",
        textInput("country_in_sample", "Filter models with a specific country in their sample")
      ),
      
      conditionalPanel(
        condition = "input.country_filter_type == 'country_group_only'",
        textInput("country_group_only", "Filter models with only a specific group of countries (comma-separated)")
      ),
      
      conditionalPanel(
        condition = "input.country_filter_type == 'country_group_in_sample'",
        textInput("country_group_in_sample", "Filter models with a specific group of countries in their sample (comma-separated)")
      ),
      
      conditionalPanel(
        condition = "input.country_filter_type == 'exclude_countries'",
        textInput("exclude_countries", "Filter models excluding specific countries (comma-separated)")
      ),
      hr(),
      textInput("filter_exclude_keys", "Excluding specific studies (comma-separated keys)", value = "")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Basic plot of effects",
                 fluidRow(
                   # Add a new input for selecting the x-axis variable
                   selectInput("x_axis_var", "X-Axis Variable:",
                               choices = c("In order of models" = "model_index",
                                           "In order of IRF periods" = "period_month"),
                               selected = "model_index"),
                   column(12, plotlyOutput("meanEffectPlot")),
                   column(12, checkboxInput("show_extreme_table", "Show table of extreme value IRFs", value = FALSE)),
                   column(12, conditionalPanel(
                     condition = "input.show_extreme_table == true",
                     numericInput("filter_value", "Filter mean.effect greater than:", value = 100, min = 0),
                     dataTableOutput("extremeValueTable")
                   ))
                 )
        ),
        tabPanel("Study IRFs",
                 fluidRow(
                   column(12,
                          textInput("study_keys", "Enter study keys (comma-separated)", value = "")
                   ),
                   column(12,
                          checkboxInput("show_approx_bounds", "Show approximated CI bounds", value = FALSE)
                   ),
                   column(12,
                          selectInput("selected_model", "Select a specific model", choices = NULL)
                   )
                 ),
                 plotlyOutput("studyIRFsPlot")
        ),
        tabPanel("Average IRFs",
                 fluidRow(
                   column(12,
                          numericInput("period_limit", "Restrict period (months):", value = max(data$period.month), min = 1)
                   ),
                   column(12,
                          selectInput("selected_outcome_var", "Select outcome_var:", choices = c("All", unique(data$outcome_var)))
                   ),
                   column(12,
                          checkboxInput("random_model", "Include one random model per study", value = FALSE)
                   ),
                   column(12,
                          actionButton("redo_random", "Redo Random Selection")
                   )
                 ),
                 plotlyOutput("averageIRFsPlot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Sidebar filters
  filtered_data <- reactive({
    data_filtered <- data
    
    # Country filters
    if (input$filter_country != "All") {
      data_filtered <- data_filtered %>% filter(list_of_countries == input$filter_country)
    }
    
    if (input$one_country != "") {
      data_filtered <- one_country(data_filtered, input$one_country)
    }
    
    if (input$country_in_sample != "") {
      data_filtered <- country_in_sample(data_filtered, input$country_in_sample)
    }
    
    if (input$country_group_only != "") {
      countries <- strsplit(input$country_group_only, ",")[[1]]
      data_filtered <- country_group_only(data_filtered, countries)
    }
    
    if (input$country_group_in_sample != "") {
      countries <- strsplit(input$country_group_in_sample, ",")[[1]]
      data_filtered <- country_group_in_sample(data_filtered, countries)
    }
    
    if (input$exclude_countries != "") {
      countries <- strsplit(input$exclude_countries, ",")[[1]]
      data_filtered <- exclude_countries(data_filtered, countries)
    }
    
    # Outcome variable filter
    if (input$filter_outcome != "All") {
      data_filtered <- data_filtered %>% filter(outcome == input$filter_outcome)
    }
    
    # Data frequency filter
    if (input$filter_month || input$filter_quarter || input$filter_annual) {
      data_filtered <- data_filtered %>%
        filter((input$filter_month & month == TRUE) |
                 (input$filter_quarter & quarter == TRUE) |
                 (input$filter_annual & annual == TRUE))
    }
    
    # Sample start / end filter
    data_filtered <- data_filtered %>%
      filter(start_year >= input$filter_years[1]) %>%
      filter(end_year <= input$filter_years[2])
    
    # Excluding specific studies
    if (input$filter_exclude_keys != "") {
      exclude_keys <- strsplit(input$filter_exclude_keys, ",")[[1]]
      data_filtered <- data_filtered %>% filter(!(key %in% exclude_keys))
    }
    
    # Option for one randomly selected model per study
    if (input$random_model) {
      if (is.null(random_data())) {
        random_model_data <- random_model_selection(data_filtered)
        random_data(random_model_data)
      }
      data_filtered <- random_data()
    }
    
    return(data_filtered)
  })
  
  random_data <- reactiveVal(NULL)
  
  observeEvent(input$redo_random, {
    random_data(NULL)
  })
  
  # Country filter reset when selected filter changes
  observeEvent(input$country_filter_type, {
    if (input$country_filter_type != "specific_country") {
      updateSelectInput(session, "filter_country", selected = "All")
    }
    if (input$country_filter_type != "one_country") {
      updateTextInput(session, "one_country", value = "")
    }
    if (input$country_filter_type != "country_in_sample") {
      updateTextInput(session, "country_in_sample", value = "")
    }
    if (input$country_filter_type != "country_group_only") {
      updateTextInput(session, "country_group_only", value = "")
    }
    if (input$country_filter_type != "country_group_in_sample") {
      updateTextInput(session, "country_group_in_sample", value = "")
    }
    if (input$country_filter_type != "exclude_countries") {
      updateTextInput(session, "exclude_countries", value = "")
    }
  })
  
  # Basic plot of effects
  output$meanEffectPlot <- renderPlotly({
    plot_mean_effect(filtered_data(), filter_outcome = input$filter_outcome, x_axis = input$x_axis_var)
  })
  
  # Extreme data table
  extreme_data <- reactive({
    filtered_data() %>%
      filter(abs(mean.effect) > input$filter_value) %>%
      group_by(key, model_id, outcome_var) %>%
      summarise(most.extreme.value = max(mean.effect, na.rm = TRUE)) %>%
      ungroup()
  })
  
  output$extremeValueTable <- renderDataTable({
    extreme_data()
  }, options = list(pageLength = 10))
  
  # Study-specific IRFs 
  observe({
    study_keys <- strsplit(input$study_keys, ",")[[1]]
    study_keys <- trimws(study_keys)
    
    if (length(study_keys) > 0 && study_keys != "") {
      filtered_data <- filtered_data() %>% filter(key %in% study_keys)
      model_ids <- unique(filtered_data$model_id)
      updateSelectInput(session, "selected_model", choices = c("All", model_ids))
    } else {
      updateSelectInput(session, "selected_model", choices = NULL)
    }
  })
  
  output$studyIRFsPlot <- renderPlotly({
    study_keys <- strsplit(input$study_keys, ",")[[1]]
    study_keys <- trimws(study_keys)
    
    if (length(study_keys) == 0 || study_keys == "") {
      return(NULL)
    }
    
    filtered_data <- filtered_data() %>% filter(key %in% study_keys)
    
    if (input$selected_model != "All") {
      filtered_data <- filtered_data %>% filter(model_id == input$selected_model)
    }
    
    plot_study_irfs(filtered_data, study_keys, selected_model = input$selected_model, show_approx_bounds = input$show_approx_bounds)
  })
  
  # Average IRF
  observe({
    if (input$filter_outcome != "All") {
      outcome_vars <- c("All", unique(data$outcome_var[data$outcome == input$filter_outcome]))
    } else {
      outcome_vars <- c("All", unique(data$outcome_var))
    }
    updateSelectInput(session, "selected_outcome_var", choices = outcome_vars, selected = "All")
  })
  
  output$averageIRFsPlot <- renderPlotly({
    plot_average_irfs(filtered_data(), selected_outcome_var = input$selected_outcome_var, period_limit = input$period_limit)
  })
}

shinyApp(ui = ui, server = server)
