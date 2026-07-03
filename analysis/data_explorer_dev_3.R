library(shiny)
library(plotly)
library(dplyr)
library(here)

# Load the data and setup
source(here::here("analysis/working_paper_1/setup_wp_1.R"))

# Source required functions
source(here::here("analysis/R/plot_average_irfs.R"))
source(here::here("analysis/R/meta_analysis.R"))
source(here::here("analysis/R/apply_winsorization.R"))
source(here::here("analysis/R/kasy_MetaStudiesFunctions.R"))
source(here::here("analysis/R/kasy_RobustVariance.R"))
source(here::here("analysis/R/kasy_MetaStudiesPlots.R"))

# Apply the same data preparations from your script
source(here::here("analysis/working_paper_1/period_0_capping_se_prec.R"))
d_no_qc$top_5_or_tier <- factor(d_no_qc$top_5_or_tier, levels = c(0, 1), 
                                labels = c("other publication", "top journal"))
d_no_qc$cbanker <- factor(d_no_qc$cbanker, levels = c(0, 1), 
                          labels = c("non-central bank affiliated", "central bank affiliated"))

ui <- fluidPage(
  titlePanel("Interactive Average IRFs with Corrections"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # Basic settings
      wellPanel(
        h4("Basic Settings"),
        selectInput("outcome_var", 
                    "Select Outcome Variable:",
                    choices = c("output", "inflation", "rate"),
                    selected = "output"),
        
        numericInput("period_limit",
                     "Period Limit (months):",
                     value = 60,
                     min = 12,
                     max = 60,
                     step = 3),
        
        selectInput("se_option", 
                    "Standard Error Option:",
                    choices = c("avg", "lower", "upper"),
                    selected = "upper"),
        
        numericInput("winsorization",
                     "Winsorization Parameter:",
                     value = 0.01,
                     min = 0,
                     max = 0.1,
                     step = 0.005)
      ),
      
      # Correction method selection
      wellPanel(
        h4("Correction Methods"),
        checkboxGroupInput("corrections",
                           "Select Corrections to Display:",
                           choices = c("PEESE" = "peese",
                                       "FAT-PET" = "fatpet", 
                                       "OLS" = "fatpet_uw",
                                       "OLS with SE²" = "peese_uw",
                                       "WAAP" = "waap",
                                       "UAAP" = "uaap",
                                       "AK" = "ak"),
                           selected = c("peese", "fatpet")),
        
        # Run button
        actionButton("run_corrections", "Run Corrections", 
                     class = "btn-primary", 
                     style = "width: 100%; margin-top: 10px;")
      ),
      
      # Interactive parameters for each method
      conditionalPanel(
        condition = "input.corrections.includes('peese') || input.corrections.includes('fatpet')",
        wellPanel(
          h4("PEESE/FAT-PET Settings"),
          checkboxInput("prec_weighted", "Precision Weighted", value = TRUE),
          checkboxInput("cluster_se", "Cluster Standard Errors", value = TRUE),
          selectInput("hc_type", "HC Type:",
                      choices = c("HC0", "HC1", "HC2", "HC3"),
                      selected = "HC1")
        )
      ),
      
      conditionalPanel(
        condition = "input.corrections.includes('waap') || input.corrections.includes('uaap')",
        wellPanel(
          h4("WAAP/UAAP Settings"),
          numericInput("ap_parameter", "Adequately Powered Parameter:", 
                       value = 2.8, min = 1, max = 5, step = 0.1),
          numericInput("ap_horizon", "AP Horizon (months):", 
                       value = 12, min = 3, max = 60, step = 3),
          helpText("Default horizons: Output = 12, Inflation = 36, Rate = 12")
        )
      ),
      
      conditionalPanel(
        condition = "input.corrections.includes('ak')",
        wellPanel(
          h4("Andrews & Kasy Settings"),
          numericInput("ak_cutoff", "Cutoff Value:", 
                       value = 1, min = 0.5, max = 3, step = 0.1),
          radioButtons("ak_modelmu", "Model for Distribution:",
                       choices = c("Normal" = "normal", "Student-t" = "t"),
                       selected = "t"),
          checkboxInput("ak_symmetric", "Symmetric p(.)", value = FALSE),
          numericInput("ak_conf_level", "Confidence Level:", 
                       value = 0.89, min = 0.8, max = 0.99, step = 0.01)
        )
      )
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Average IRF Plot",
                 plotlyOutput("avg_irf_plot", height = "600px")
        ),
        tabPanel("Method Details",
                 h4("Current Settings Summary"),
                 verbatimTextOutput("settings_summary"),
                 
                 h4("Correction Results"),
                 tableOutput("correction_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Update AP horizon when outcome variable changes
  observe({
    ap_value <- switch(input$outcome_var,
                       "output" = 12,
                       "inflation" = 36,
                       "rate" = 12,
                       12)  # default
    updateNumericInput(session, "ap_horizon", value = ap_value)
  })
  
  # Helper functions
  extract_intercepts <- function(results) {
    intercepts <- lapply(results, function(model) {
      ci <- confint(model, level = 0.89)
      c(estimate = model[[1]][1],
        estimate_se = model[1,2],
        lower = ci[1, 1],
        upper = ci[1, 2])
    })
    
    data.frame(
      period = as.numeric(names(results)),
      estimate = sapply(intercepts, function(x) x["estimate"]),
      estimate_se = sapply(intercepts, function(x) x["estimate_se"]),
      lower = sapply(intercepts, function(x) x["lower"]),
      upper = sapply(intercepts, function(x) x["upper"])
    )
  }
  
  extract_intercepts_AK <- function(results) {
    intercepts <- lapply(results, function(model) {
      c(estimate = model$tidy$estimate[1],
        estimate_se = model$tidy$std.error[1],
        lower = model$tidy$conf.low[1],
        upper = model$tidy$conf.high[1])
    })
    
    data.frame(
      period = as.numeric(names(results)),
      estimate = sapply(intercepts, function(x) x["estimate"]),
      estimate_se = sapply(intercepts, function(x) x["estimate_se"]),
      lower = sapply(intercepts, function(x) x["lower"]),
      upper = sapply(intercepts, function(x) x["upper"])
    )
  }
  
  # Generate base plot
  base_plot <- reactive({
    plot_average_irfs(
      d_no_qc |> filter(period.month %in% seq(0, input$period_limit, by = 3), 
                        outcome == input$outcome_var),
      period_limit = input$period_limit,
      winsor = TRUE,
      wins_par = input$winsorization,
      corrected_irf = NULL,
      show_legend = FALSE,
      show_median = FALSE,
      return_data = TRUE
    )
  })
  
  # Generate correction estimates - now triggered by button
  correction_data <- eventReactive(input$run_corrections, {
    req(input$corrections)
    
    out_var <- input$outcome_var
    
    corrections <- list()
    periods <- seq(0, input$period_limit, by = 3)
    
    if("peese" %in% input$corrections) {
      peese_result <- meta_analysis(d_no_qc,
                                    outvar = out_var,
                                    se_option = input$se_option,
                                    periods = periods,
                                    wins = input$winsorization,
                                    prec_weighted = input$prec_weighted,
                                    estimation = "PEESE",
                                    cluster_se = input$cluster_se,
                                    hc_type = input$hc_type)
      corrections$peese <- extract_intercepts(peese_result)
    }
    
    if("fatpet" %in% input$corrections) {
      fatpet_result <- meta_analysis(d_no_qc,
                                     outvar = out_var,
                                     se_option = input$se_option,
                                     periods = periods,
                                     wins = input$winsorization,
                                     prec_weighted = input$prec_weighted,
                                     estimation = "FAT-PET",
                                     cluster_se = input$cluster_se,
                                     hc_type = input$hc_type)
      corrections$fatpet <- extract_intercepts(fatpet_result)
    }
    
    if("fatpet_uw" %in% input$corrections) {
      fatpet_uw_result <- meta_analysis(d_no_qc,
                                        outvar = out_var,
                                        se_option = input$se_option,
                                        periods = periods,
                                        wins = input$winsorization,
                                        prec_weighted = FALSE,
                                        estimation = "FAT-PET",
                                        cluster_se = input$cluster_se,
                                        hc_type = input$hc_type)
      corrections$fatpet_uw <- extract_intercepts(fatpet_uw_result)
    }
    
    if("peese_uw" %in% input$corrections) {
      peese_uw_result <- meta_analysis(d_no_qc,
                                       outvar = out_var,
                                       se_option = input$se_option,
                                       periods = periods,
                                       wins = input$winsorization,
                                       prec_weighted = FALSE,
                                       estimation = "PEESE",
                                       cluster_se = input$cluster_se,
                                       hc_type = input$hc_type)
      corrections$peese_uw <- extract_intercepts(peese_uw_result)
    }
    
    if("waap" %in% input$corrections) {
      waap_result <- meta_analysis(d_no_qc,
                                   outvar = out_var,
                                   se_option = input$se_option,
                                   periods = periods,
                                   wins = input$winsorization,
                                   prec_weighted = FALSE,
                                   ap = TRUE,
                                   ap_horizon = input$ap_horizon,
                                   ap_prec_weighted = TRUE,
                                   ap_parameter = input$ap_parameter,
                                   estimation = "UWLS",
                                   cluster_se = input$cluster_se)
      corrections$waap <- extract_intercepts(waap_result)
    }
    
    if("uaap" %in% input$corrections) {
      uaap_result <- meta_analysis(d_no_qc,
                                   outvar = out_var,
                                   se_option = input$se_option,
                                   periods = periods,
                                   wins = input$winsorization,
                                   prec_weighted = FALSE,
                                   ap = TRUE,
                                   ap_horizon = input$ap_horizon,
                                   ap_prec_weighted = FALSE,
                                   ap_parameter = input$ap_parameter,
                                   estimation = "UWLS",
                                   cluster_se = input$cluster_se)
      corrections$uaap <- extract_intercepts(uaap_result)
    }
    
    if("ak" %in% input$corrections) {
      ak_result <- meta_analysis(d_no_qc,
                                 outvar = out_var,
                                 se_option = input$se_option,
                                 periods = periods,
                                 wins = input$winsorization,
                                 prec_weighted = FALSE,
                                 estimation = "AK",
                                 cluster_se = input$cluster_se,
                                 cutoff_val = input$ak_cutoff,
                                 AK_modelmu = input$ak_modelmu,
                                 AK_symmetric = input$ak_symmetric,
                                 AK_conf_level = input$ak_conf_level,
                                 ak_plot = "both")
      corrections$ak <- extract_intercepts_AK(ak_result)
    }
    
    corrections
  })
  
  output$avg_irf_plot <- renderPlotly({
    base <- base_plot()$plot |>
      plotly::layout(title = paste("Average IRF for", stringr::str_to_title(input$outcome_var)))
    
    # Only add corrections if the button has been clicked
    if (input$run_corrections > 0) {
      corrections <- correction_data()
      
      # Add correction lines with same styling as original
      if(!is.null(corrections$peese)) {
        base <- base |>
          add_lines(data = corrections$peese,
                    x = ~period, y = ~estimate,
                    name = "PEESE",
                    line = list(color = "darkgreen", width = 1, dash = 'solid'))
      }
      
      if(!is.null(corrections$fatpet)) {
        base <- base |>
          add_lines(data = corrections$fatpet,
                    x = ~period, y = ~estimate,
                    name = "FAT-PET",
                    line = list(color = "darkgreen", width = 2, dash = "dot"))
      }
      
      if(!is.null(corrections$fatpet_uw)) {
        base <- base |>
          add_lines(data = corrections$fatpet_uw,
                    x = ~period, y = ~estimate,
                    name = "OLS",
                    line = list(color = "darkgreen", width = 4, dash = "dot"))
      }
      
      if(!is.null(corrections$peese_uw)) {
        base <- base |>
          add_lines(data = corrections$peese_uw,
                    x = ~period, y = ~estimate,
                    name = "OLS with SE²",
                    line = list(color = "darkgreen", width = 4, dash = "solid"))
      }
      
      if(!is.null(corrections$waap)) {
        base <- base |>
          add_trace(data = corrections$waap,
                    x = ~period, y = ~estimate,
                    name = "WAAP", mode = "markers",
                    marker = list(color = "darkgreen", size = 5))
      }
      
      if(!is.null(corrections$uaap)) {
        base <- base |>
          add_trace(data = corrections$uaap,
                    x = ~period, y = ~estimate,
                    name = "UAAP", mode = "markers",
                    marker = list(color = "white", size = 5,
                                  line = list(color = 'darkgreen', width = 1)))
      }
      
      if(!is.null(corrections$ak)) {
        base <- base |>
          add_lines(data = corrections$ak,
                    x = ~period, y = ~estimate,
                    name = "AK",
                    line = list(color = "darkgreen", width = 1, dash = 'longdashdot'))
      }
    }
    
    base
  })
  
  # Settings summary for the Details tab
  output$settings_summary <- renderText({
    paste(
      "Outcome Variable:", input$outcome_var,
      "\nPeriod Limit:", input$period_limit, "months",
      "\nStandard Error Option:", input$se_option,
      "\nWinsorization:", input$winsorization,
      "\nSelected Corrections:", paste(input$corrections, collapse = ", "),
      if(length(input$corrections) > 0 && any(c("peese", "fatpet") %in% input$corrections)) {
        paste("\n\nPEESE/FAT-PET Settings:",
              "\n  - Precision Weighted:", input$prec_weighted,
              "\n  - Cluster SE:", input$cluster_se,
              "\n  - HC Type:", input$hc_type)
      },
      if("waap" %in% input$corrections || "uaap" %in% input$corrections) {
        paste("\n\nWAAP/UAAP Settings:",
              "\n  - AP Parameter:", input$ap_parameter,
              "\n  - AP Horizon:", input$ap_horizon)
      },
      if("ak" %in% input$corrections) {
        paste("\n\nAndrews & Kasy Settings:",
              "\n  - Cutoff Value:", input$ak_cutoff,
              "\n  - Model:", input$ak_modelmu,
              "\n  - Symmetric:", input$ak_symmetric,
              "\n  - Confidence Level:", input$ak_conf_level)
      }
    )
  })
  
  # Results table for the Details tab
  output$correction_table <- renderTable({
    # Only show table if button has been clicked
    if (input$run_corrections == 0) return(NULL)
    
    corrections <- correction_data()
    
    if(length(corrections) == 0) return(NULL)
    
    # Create a summary table with key statistics
    summary_data <- data.frame(
      Method = character(),
      Period_0 = numeric(),
      Period_12 = numeric(),
      Period_24 = numeric(),
      Max_Effect = numeric(),
      stringsAsFactors = FALSE
    )
    
    for(method_name in names(corrections)) {
      method_data <- corrections[[method_name]]
      
      period_0 <- method_data$estimate[method_data$period == 0]
      period_12 <- method_data$estimate[method_data$period == 12]
      period_24 <- method_data$estimate[method_data$period == 24]
      max_effect <- max(abs(method_data$estimate), na.rm = TRUE)
      
      summary_data <- rbind(summary_data, data.frame(
        Method = toupper(method_name),
        Period_0 = if(length(period_0) > 0) round(period_0, 4) else NA,
        Period_12 = if(length(period_12) > 0) round(period_12, 4) else NA,
        Period_24 = if(length(period_24) > 0) round(period_24, 4) else NA,
        Max_Effect = round(max_effect, 4),
        stringsAsFactors = FALSE
      ))
    }
    
    summary_data
  }, digits = 4)
}

shinyApp(ui = ui, server = server)