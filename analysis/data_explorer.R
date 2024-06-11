# Load necessary libraries
library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(here)
library(JWileymisc)
library(viridis)

# Load the data 
# data_path <- here("preliminary_data.RData")
# data_path <- here("data/preliminary_data.RData")
# data_path <- here("data/data_test.RData")
data_path <- here("data/data_test_new.RData")
load(data_path)

# ---- THIS SHOULD SOON BE DONE DIRECTLY IN THE PACKAGE ----
# Splitting emp and unemp
data$outcome <- ifelse(data$outcome_measure == "une_rate", "unemp", data$outcome)
data$outcome <- ifelse(is.na(data$outcome_measure), "rate", data$outcome)
# Renaming gdp to output
data$outcome <- ifelse(data$outcome == "gdp", "output", data$outcome)
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

# ---- THIS SHOULD SOON BE DONE DIRECTLY IN THE PACKAGE ----
# Calculate the average standard error and precision options
data$SE.avg <- (data$SE.upper + data$SE.lower) / 2
data$precision.avg <- 1 / data$SE.avg
data$precision.lower <- 1 / data$SE.lower
data$precision.upper <- 1 / data$SE.upper

# Define choices for moderator filters
moderator_groups <- list(
  "General" = list("cum"),
  "Impulse and Response Variables" = list("prefer"),
  "Identification Strategy" = list("iv", "forecast_based", "nr", "event", "chol", "svar", "signr", "hf", "heteroskedas", "longrun", "idother"),
  "Estimation Method" = list("var", "lp", "vecm", "dyn_ols", "fvar", "tvar", "gvar", "bayes", "dsge", "varother"),
  "Regime Dependence" = list("lor", "upr", "scr", "dcr", "hike", "cut"),
  "Data Frequency" = list("annual", "quarter", "month"),
  "Further Data Characteristics" = list("panel"),
  "Control Variables" = list("comprice", "outpgap", "find", "eglob", "cbind", "fexch", "inflexp", "foreignir", "fx", "lrir"),
  "Econometric Details" = list("pure_rate_shock", "convent", "decomposition"),
  "Publication Characteristics" = list("cbanker")
)

ui <- fluidPage(
  titlePanel("META CMP Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Current Selection"),
        tableOutput("filteredDataSummary"),
        h5("Required:"),
        verbatimTextOutput("filterSummary")
      ),
      h4("Filters"),
      sliderInput("filter_years", "Sample years",
                  min = min(data$start_year, na.rm = TRUE),
                  max = max(data$end_year, na.rm = TRUE),
                  value = c(min(data$start_year, na.rm = TRUE), max(data$end_year, na.rm = TRUE)),
                  sep = ""),
      tabsetPanel(
        tabPanel("Response variable",
                 selectInput("filter_outcome", "Show data for specific response variable", choices = c("All", unique(data$outcome)), selected = "All"),
                 conditionalPanel(
                   condition = "input.filter_outcome != 'All'",
                   selectInput("filter_outcome_measure", "Select the outcome measure of the response variable", 
                               choices = c("All", unique(data$outcome_measure)), selected = "All")
                 ),
                 selectInput("filter_transformation", "Select the transformation of the response variable", choices = c("All", unique(data$transformation)), selected = "All"),
                 selectInput("filter_periodicity", "Select the periodicity of the response variable", choices = c("All", unique(data$periodicity)), selected = "All")
        ),
        tabPanel("Countries", 
                 selectInput("country_filter_type", "Country filter type:",
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
                   textInput("country_in_sample", "Filter models with a specific country in their sample. Specify country:")
                 ),
                 
                 conditionalPanel(
                   condition = "input.country_filter_type == 'country_group_only'",
                   textInput("country_group_only", "Filter models with only a specific group of countries. Specify countries (comma-separated):")
                 ),
                 
                 conditionalPanel(
                   condition = "input.country_filter_type == 'country_group_in_sample'",
                   textInput("country_group_in_sample", "Filter models with a specific group of countries in their sample. Specify countries (comma-separated):")
                 ),
                 
                 conditionalPanel(
                   condition = "input.country_filter_type == 'exclude_countries'",
                   textInput("exclude_countries", "Filter models excluding specific countries. Specify countries (comma-separated):")
                 )
        ),
        tabPanel("Other moderators",
                 selectInput(
                   "include_filter",
                   "Required model characteristics:",
                   choices = moderator_groups,
                   multiple = TRUE
                 ),
                 selectInput(
                   "exclude_filter",
                   "Excluded model characteristics:",
                   choices = moderator_groups,
                   multiple = TRUE
                 )
        ),
        tabPanel("Studies",
                 selectInput("study_filter_type", "Select filter type:",
                             choices = c("No filter" = "no_filter",
                                         "Include specific studies" = "include_studies",
                                         "Exclude specific studies" = "exclude_studies"),
                             selected = "no_filter"),
                 
                 conditionalPanel(
                   condition = "input.study_filter_type == 'include_studies'",
                   textInput("filter_include_keys", "Specify studies to include (comma-separated keys):", value = "")
                 ),
                 conditionalPanel(
                   condition = "input.study_filter_type == 'exclude_studies'",
                   textInput("filter_exclude_keys", "Specify studies to exclude (comma-separated keys):", value = "")
                 )
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 tabsetPanel(
                   tabPanel("Basic plot of effects",
                            fluidRow(
                              # Add a new input for selecting the x-axis variable
                              selectInput("x_axis_var", "X-Axis Variable:",
                                          choices = c("In order of models" = "model_index",
                                                      "In order of IRF periods" = "period_month"),
                                          selected = "model_index"),
                              column(12, plotlyOutput("meanEffectPlot")),
                              column(12, checkboxInput("show_extreme_table", 'Show table of extreme value & potentially "explosive" IRFs', value = FALSE)),
                              column(12, conditionalPanel(
                                condition = "input.show_extreme_table == true",
                                numericInput("filter_value", "Filter mean.effect greater than:", value = 100, min = 0),
                                h3("Studies with extreme values:"),
                                verbatimTextOutput("studyKeys_extreme"),
                                dataTableOutput("extremeValueTable"),
                                h3("Studies with max values in last period (potentially explosive):"),
                                verbatimTextOutput("studyKeys_explosive"),
                                dataTableOutput("maxLastPeriodTable")
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
                            plotlyOutput("studyIRFsPlot"),
                            checkboxInput("show_moderators", "Show study characteristics", value = FALSE),
                            conditionalPanel(
                              condition = "input.show_moderators == true",
                              dataTableOutput("moderatorTable")
                            )
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
                            plotlyOutput("averageIRFsPlot"),
                            checkboxInput("show_counts_plot", "Show Model/Study Counts Plot", value = FALSE),
                            conditionalPanel(
                              condition = "input.show_counts_plot == true",
                              selectInput("plot_type", "Select Plot Type:",
                                          choices = c("Model Counts" = "model", "Study Counts" = "study"),
                                          selected = "model"),
                              plotlyOutput("countsPlot")
                            )
                  ),
                  tabPanel("Funnel Plot",
                           br(),
                           fluidRow(
                             column(12,
                                    fluidRow(
                                      column(4,
                                             numericInput("funnel_prd", "Period (Months):", value = 3, min = 0)
                                      ),
                                      column(4,
                                             selectInput("funnel_se_option", "Standard Error Option:",
                                                         choices = c("avg", "lower", "upper"))
                                      ),
                                      column(4,
                                             numericInput("funnel_wins", "Winsorization Parameter:", value = 0.02, min = 0, max = 1, step = 0.01)
                                      )
                                    ),
                                    checkboxInput("show_additional_plots", "Show additional plots", value = FALSE),
                                    conditionalPanel(
                                      condition = "input.show_additional_plots == false",
                                      plotlyOutput("funnel_plot")
                                    ),
                                    conditionalPanel(
                                      condition = "input.show_additional_plots == true",
                                      fluidRow(
                                        column(4, plotlyOutput("funnel_plot_1")),
                                        column(4, plotlyOutput("funnel_plot_2")),
                                        column(4, plotlyOutput("funnel_plot_3"))
                                      ),
                                      fluidRow(
                                        column(4, plotlyOutput("funnel_plot_4")),
                                        column(4, plotlyOutput("funnel_plot_5")),
                                        column(4, plotlyOutput("funnel_plot_6"))
                                      )
                                    )
                             )
                           )
                  )
                )
                ),
        tabPanel("Stats",
                 tabsetPanel(
                   tabPanel("Descriptive statistics",
                            # ...
                            ),
                   tabPanel("Meta-analysis",
                            # ...
                            )
                   )
                 )
      ),
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
    
    # Outcome variable filter
    if (input$filter_outcome != "All" & input$filter_outcome_measure != "All") {
      data_filtered <- data_filtered %>% filter(outcome_measure == input$filter_outcome_measure)
    }
    
    # Transformation filter
    if (input$filter_transformation != "All") {
      data_filtered <- data_filtered %>% filter(transformation == input$filter_transformation)
    }
    
    # Periodicity filter
    if (input$filter_periodicity != "All") {
      data_filtered <- data_filtered %>% filter(periodicity == input$filter_periodicity)
    }
    
    # Sample start / end filter
    data_filtered <- data_filtered %>%
      filter(start_year >= input$filter_years[1]) %>%
      filter(end_year <= input$filter_years[2])
    
    # Include or exclude specific studies based on the selected study filter type
    if (input$study_filter_type == "include_studies") {
      if (input$filter_include_keys != "") {
        include_keys <- strsplit(input$filter_include_keys, ",")[[1]]
        data_filtered <- data_filtered %>% filter(key %in% include_keys)
      }
    } else if (input$study_filter_type == "exclude_studies") {
      if (input$filter_exclude_keys != "") {
        exclude_keys <- strsplit(input$filter_exclude_keys, ",")[[1]]
        data_filtered <- data_filtered %>% filter(!(key %in% exclude_keys))
      }
    }
    
    # Moderator filters
    # Include
    if (!is.null(input$include_filter)) {
      for (variable in input$include_filter) {
        data_filtered <- data_filtered[data_filtered[[variable]] == TRUE, ]
      }
    }
    # Exclude
    if (!is.null(input$exclude_filter)) {
      for (variable in input$exclude_filter) {
        data_filtered <- data_filtered[data_filtered[[variable]] == FALSE, ]
      }
    }

    # Option for one randomly selected model per study for average IRF
    if (input$random_model) {
      if (is.null(random_data())) {
        random_model_data <- random_model_selection(data_filtered)
        random_data(random_model_data)
      }
      data_filtered <- random_data()
    }
    
    return(data_filtered)
  })
  
  # Data summary
  output$filteredDataSummary <- renderTable({
    filtered_data_summary(filtered_data())
    }, align = "l")
  # Filter summary
  filterSummary <- reactive({
    summary <- ""
    
    # Response variable filters
    if (input$filter_outcome != "All") {
      summary <- paste0(summary, "Response Variable: ", input$filter_outcome, "\n")
      if (input$filter_outcome_measure != "All") {
        summary <- paste0(summary, "  Outcome Measure: ", input$filter_outcome_measure, "\n")
      }
      if (input$filter_transformation != "All") {
        summary <- paste0(summary, "  Transformation: ", input$filter_transformation, "\n")
      }
      if (input$filter_periodicity != "All") {
        summary <- paste0(summary, "  Periodicity: ", input$filter_periodicity, "\n")
      }
    }

    # Country filters
    if (input$country_filter_type != "all") {
      if (input$country_filter_type == "specific_country" && input$filter_country != "All") {
        summary <- paste0(summary, "Countries: ", input$filter_country, "\n")
      } else if (input$country_filter_type == "one_country" && input$one_country != "") {
        summary <- paste0(summary, "One Country: ", input$one_country, "\n")
      } else if (input$country_filter_type == "country_in_sample" && input$country_in_sample != "") {
        summary <- paste0(summary, "Country in Sample: ", input$country_in_sample, "\n")
      } else if (input$country_filter_type == "country_group_only" && input$country_group_only != "") {
        summary <- paste0(summary, "Country Group Only: ", input$country_group_only, "\n")
      } else if (input$country_filter_type == "country_group_in_sample" && input$country_group_in_sample != "") {
        summary <- paste0(summary, "Country Group in Sample: ", input$country_group_in_sample, "\n")
      } else if (input$country_filter_type == "exclude_countries" && input$exclude_countries != "") {
        summary <- paste0(summary, "Excluded Countries: ", input$exclude_countries, "\n")
      }
    }
    
    # Moderator filters
    # Include summary
    if (length(input$include_filter) > 0) {
      summary <- paste0(summary, "\nRequired Moderators:\n")
      
      for (group in names(moderator_groups)) {
        included_moderators <- intersect(input$include_filter, moderator_groups[[group]])
        if (length(included_moderators) > 0) {
          summary <- paste0(summary, "  ", group, ": ", paste(included_moderators, collapse = ", "), "\n")
        }
      }
    }
    # Exclude summary
    if (length(input$exclude_filter) > 0) {
      summary <- paste0(summary, "Excluded Moderators:\n")
      
      for (group in names(moderator_groups)) {
        excluded_moderators <- intersect(input$exclude_filter, moderator_groups[[group]])
        if (length(excluded_moderators) > 0) {
          summary <- paste0(summary, "  ", group, ": ", paste(excluded_moderators, collapse = ", "), "\n")
        }
      }
    }
    
    # Study filters
    if (input$study_filter_type != "no_filter") {
      if (input$study_filter_type == "include_studies" && input$filter_include_keys != "") {
        summary <- paste0(summary, "\nSelected Studies: ", length(input$filter_include_keys), "\n")
      } else if (input$study_filter_type == "exclude_studies" && input$filter_exclude_keys != "") {
        summary <- paste0(summary, "\nExcluded Studies: ", length(input$filter_exclude_keys), "\n")
      }
    }
    
    summary
  })
  output$filterSummary <- renderText({
    filterSummary()
  })
  
  # Update years filter values
  observe({
    if (input$filter_years[1] < min(filtered_data()$start_year, na.rm = TRUE)) {
      updateSliderInput(session, "filter_years",
                        value = c(min(filtered_data()$start_year, na.rm = TRUE),
                                  input$filter_years[2]))
    }
    if (input$filter_years[2] > max(filtered_data()$end_year, na.rm = TRUE)) {
      updateSliderInput(session, "filter_years",
                        value = c(input$filter_years[1],
                                  max(filtered_data()$end_year, na.rm = TRUE)))
    }
  })
  
  # Update exclude_filter when include_filter changes
  observeEvent(input$include_filter, {
    updateSelectInput(session, "exclude_filter",
                      selected = setdiff(input$exclude_filter, input$include_filter))
  })
  
  # Update include_filter when exclude_filter changes
  observeEvent(input$exclude_filter, {
    updateSelectInput(session, "include_filter",
                      selected = setdiff(input$include_filter, input$exclude_filter))
  })
  
  # Update exclude_filter based on the frequency selection in include_filter
  observeEvent(input$include_filter, {
    frequency_values <- c("quarter", "month", "annual")
    selected_frequency <- intersect(input$include_filter, frequency_values)
    
    if (length(selected_frequency) > 0) {
      excluded_frequency <- setdiff(frequency_values, selected_frequency)
      updateSelectInput(session, "exclude_filter",
                        selected = union(input$exclude_filter, excluded_frequency))
    }
  })
  
  # Updating outcome_measure, transformation and periodicity filtering options
  # Updating filter_outcome_measure
  observe({
    if (input$filter_outcome != "All") {
      outcome_measures <- c("All", unique(data$outcome_measure[data$outcome == input$filter_outcome]))
    } else {
      outcome_measures <- c("All", unique(data$outcome_measure))
    }
    updateSelectInput(session, "filter_outcome_measure", choices = outcome_measures, selected = "All")
  })
  # Updating filter_transformation
  observe({
    if (input$filter_outcome != "All") {
      transformations <- c("All", unique(data$transformation[data$outcome == input$filter_outcome]))
    } else {
      transformations <- c("All", unique(data$transformation))
    }
    updateSelectInput(session, "filter_transformation", choices = transformations, selected = "All")
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
  
  # Study keys for extreme data
  output$studyKeys_extreme <- renderText({
    study_keys_extreme <- unique(extreme_data()$key)
    paste(paste(study_keys_extreme, collapse = ","))
  })
  
  # Table with potentially explosive models
  explosive_data <- reactive({
    find_max_last_period_models(filtered_data())
  })
  
  output$maxLastPeriodTable <- renderDataTable({
    explosive_data()
  }, options = list(pageLength = 10))
  
  # Study keys for porentially explosive IRFs
  output$studyKeys_explosive <- renderText({
    study_keys_explosive <- unique(explosive_data()$key)
    paste(paste(study_keys_explosive, collapse = ","))
  })
  
  # Study-specific IRFs 
  # Reactive expression to filter data based on study keys and selected model
  study_data <- reactive({
    study_keys <- strsplit(input$study_keys, ",")[[1]] %>% trimws()
    if (length(study_keys) > 0 && study_keys != "") {
      data_filtered <- data %>% filter(key %in% study_keys)
      if (input$selected_model != "All" && !is.null(input$selected_model)) {
        data_filtered <- data_filtered %>% filter(model_id == input$selected_model)
      }
      return(data_filtered)
    }
    return(data.frame())
  })
  
  # Update the selectInput for models when study keys change
  observe({
    study_keys <- strsplit(input$study_keys, ",")[[1]] %>% trimws()
    if (length(study_keys) > 0 && study_keys != "") {
      filtered_data <- data %>% filter(key %in% study_keys)
      model_ids <- unique(filtered_data$model_id)
      updateSelectInput(session, "selected_model", choices = c("All", model_ids))
    } else {
      updateSelectInput(session, "selected_model", choices = NULL)
    }
  })
  
  # Render the plot for study IRFs
  output$studyIRFsPlot <- renderPlotly({
    plot_data <- study_data()
    if (nrow(plot_data) == 0) return(NULL)
    
    plot_study_irfs(plot_data, input$study_keys, selected_model = input$selected_model, show_approx_bounds = input$show_approx_bounds)
  })
  
  # Render the table for moderator variables
  output$moderatorTable <- renderDataTable({
    table_data <- study_data()
    if (nrow(table_data) == 0) return(NULL)
    
    # Select unique rows for each model
    table_data <- table_data %>% distinct(model_id, .keep_all = TRUE)
    
    # Columns to exclude
    columns_to_exclude <- c("period", "CI.upper.raw", "mean.effect.raw", "CI.lower.raw", "CI.upper",
                            "mean.effect", "CI.lower", "SE.upper", "SE.lower", "period.month",
                            "rate_CI.upper.raw", "rate_mean.effect.raw", "rate_CI.lower.raw",
                            "rate_CI.upper", "rate_mean.effect", "rate_CI.lower", "rate_SE.upper",
                            "rate_SE.lower", "approx.CI.lower_68", "approx.CI.upper_68", 
                            "approx.CI.lower_90", "approx.CI.upper_90", "approx.CI.lower_95", 
                            "approx.CI.upper_95")
    
    # Select all columns except the ones to exclude
    table_data <- table_data %>% select(-all_of(columns_to_exclude))
    
    table_data
  }, options = list(pageLength = 10))
  
  # Average IRF
  output$averageIRFsPlot <- renderPlotly({
    plot_average_irfs(filtered_data(), period_limit = input$period_limit)
  })
  
  # Model/Study counts plot
  output$countsPlot <- renderPlotly({
    req(input$show_counts_plot)
    plot_model_study_counts(filtered_data(), plot_type = input$plot_type)
  })
  
  # Funnel plot
  output$funnel_plot <- renderPlotly({
    create_funnel_plot(filtered_data(),
                       outvar = input$filter_outcome,
                       prd = input$funnel_prd,
                       se_option = input$funnel_se_option,
                       wins = input$funnel_wins)
  })
  output$funnel_plot_1 <- renderPlotly({
    create_funnel_plot(filtered_data(),
                       outvar = input$filter_outcome,
                       prd = input$funnel_prd,
                       se_option = input$funnel_se_option,
                       wins = input$funnel_wins,
                       legend = FALSE)
  })
  output$funnel_plot_2 <- renderPlotly({
    create_funnel_plot(filtered_data(),
                       outvar = input$filter_outcome,
                       prd = input$funnel_prd*2,
                       se_option = input$funnel_se_option,
                       wins = input$funnel_wins,
                       legend = FALSE)
  })
  output$funnel_plot_3 <- renderPlotly({
    create_funnel_plot(filtered_data(),
                       outvar = input$filter_outcome,
                       prd = input$funnel_prd*3,
                       se_option = input$funnel_se_option,
                       wins = input$funnel_wins,
                       legend = FALSE)
  })
  
  output$funnel_plot_4 <- renderPlotly({
    create_funnel_plot(filtered_data(),
                       outvar = input$filter_outcome,
                       prd = input$funnel_prd*4,
                       se_option = input$funnel_se_option,
                       wins = input$funnel_wins,
                       legend = FALSE)
  })
  
  output$funnel_plot_5 <- renderPlotly({
    create_funnel_plot(filtered_data(),
                       outvar = input$filter_outcome,
                       prd = input$funnel_prd*5,
                       se_option = input$funnel_se_option,
                       wins = input$funnel_wins,
                       legend = FALSE)
  })
  
  output$funnel_plot_6 <- renderPlotly({
    create_funnel_plot(filtered_data(),
                       outvar = input$filter_outcome,
                       prd = input$funnel_prd*6,
                       se_option = input$funnel_se_option,
                       wins = input$funnel_wins,
                       legend = FALSE)
  })
}

shinyApp(ui = ui, server = server)
