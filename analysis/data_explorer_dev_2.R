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

# Helper functions from your script
extract_intercepts <- function(results) {
  intercepts <- lapply(results, function(model) {
    ci <- confint(model, level = conflevel)
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

ui <- fluidPage(
  titlePanel("Average IRFs with Corrections"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome_var", 
                  "Select Outcome Variable:",
                  choices = c("output", "inflation", "rate"),
                  selected = "output"),
      
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
      
      numericInput("period_limit",
                   "Period Limit (months):",
                   value = 60,
                   min = 12,
                   max = 60,
                   step = 3)
    ),
    
    mainPanel(
      plotlyOutput("avg_irf_plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  # Generate base plot
  base_plot <- reactive({
    plot_average_irfs(
      d_no_qc |> filter(period.month %in% seq(0, input$period_limit, by = 3), 
                        outcome == input$outcome_var),
      period_limit = input$period_limit,
      winsor = TRUE,
      wins_par = wins_para,
      corrected_irf = NULL,
      show_legend = FALSE,
      show_median = FALSE,
      return_data = TRUE
    )
  })
  
  # Generate correction estimates
  correction_data <- reactive({
    req(input$corrections)
    
    out_var <- input$outcome_var
    se_option <- if(out_var == "rate") "avg" else "upper"
    
    corrections <- list()
    
    if("peese" %in% input$corrections) {
      peese_result <- meta_analysis(d_no_qc,
                                    outvar = out_var,
                                    se_option = se_option,
                                    periods = seq(0, input$period_limit, by = 3),
                                    wins = wins_para,
                                    prec_weighted = TRUE,
                                    estimation = "PEESE",
                                    cluster_se = TRUE)
      corrections$peese <- extract_intercepts(peese_result)
    }
    
    if("fatpet" %in% input$corrections) {
      fatpet_result <- meta_analysis(d_no_qc,
                                     outvar = out_var,
                                     se_option = se_option,
                                     periods = seq(0, input$period_limit, by = 3),
                                     wins = wins_para,
                                     prec_weighted = TRUE,
                                     estimation = "FAT-PET",
                                     cluster_se = TRUE)
      corrections$fatpet <- extract_intercepts(fatpet_result)
    }
    
    if("fatpet_uw" %in% input$corrections) {
      fatpet_uw_result <- meta_analysis(d_no_qc,
                                        outvar = out_var,
                                        se_option = se_option,
                                        periods = seq(0, input$period_limit, by = 3),
                                        wins = wins_para,
                                        prec_weighted = FALSE,
                                        estimation = "FAT-PET",
                                        cluster_se = TRUE)
      corrections$fatpet_uw <- extract_intercepts(fatpet_uw_result)
    }
    
    if("peese_uw" %in% input$corrections) {
      peese_uw_result <- meta_analysis(d_no_qc,
                                       outvar = out_var,
                                       se_option = se_option,
                                       periods = seq(0, input$period_limit, by = 3),
                                       wins = wins_para,
                                       prec_weighted = FALSE,
                                       estimation = "PEESE",
                                       cluster_se = TRUE)
      corrections$peese_uw <- extract_intercepts(peese_uw_result)
    }
    
    if("waap" %in% input$corrections) {
      ap_horizon <- switch(out_var, "output" = 12, "inflation" = 36, "rate" = 12)
      waap_result <- meta_analysis(d_no_qc,
                                   outvar = out_var,
                                   se_option = se_option,
                                   periods = seq(0, input$period_limit, by = 3),
                                   wins = wins_para,
                                   prec_weighted = FALSE,
                                   ap = TRUE,
                                   ap_horizon = ap_horizon,
                                   ap_prec_weighted = TRUE,
                                   ap_parameter = 2.8,
                                   estimation = "UWLS",
                                   cluster_se = TRUE)
      corrections$waap <- extract_intercepts(waap_result)
    }
    
    if("uaap" %in% input$corrections) {
      ap_horizon <- switch(out_var, "output" = 12, "inflation" = 36, "rate" = 12)
      uaap_result <- meta_analysis(d_no_qc,
                                   outvar = out_var,
                                   se_option = se_option,
                                   periods = seq(0, input$period_limit, by = 3),
                                   wins = wins_para,
                                   prec_weighted = FALSE,
                                   ap = TRUE,
                                   ap_horizon = ap_horizon,
                                   ap_prec_weighted = FALSE,
                                   ap_parameter = 2.8,
                                   estimation = "UWLS",
                                   cluster_se = TRUE)
      corrections$uaap <- extract_intercepts(uaap_result)
    }
    
    if("ak" %in% input$corrections) {
      ak_result <- meta_analysis(d_no_qc,
                                 outvar = out_var,
                                 se_option = se_option,
                                 periods = seq(0, input$period_limit, by = 3),
                                 wins = wins_para,
                                 prec_weighted = FALSE,
                                 estimation = "AK",
                                 cluster_se = TRUE,
                                 cutoff_val = 1,
                                 AK_modelmu = "t",
                                 AK_symmetric = FALSE,
                                 AK_conf_level = conflevel,
                                 ak_plot = "both")
      corrections$ak <- extract_intercepts_AK(ak_result)
    }
    
    corrections
  })
  
  output$avg_irf_plot <- renderPlotly({
    base <- base_plot()$plot |>
      plotly::layout(title = paste("Average IRF for", stringr::str_to_title(input$outcome_var)))
    
    corrections <- correction_data()
    
    # Add correction lines
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
    
    base
  })
}

shinyApp(ui = ui, server = server)