# Creates funnel plots

# Source the setup file ---- 
source(here::here("analysis/working_paper_2/setup_wp_2.R"))

# Load required libraries ----
library(plotly) # For interactive plots
library(JWileymisc) # For winsorizing
library(viridis) # For color palette

# Source required functions ----
source(here::here("analysis/R/funnel_plot.R"))
source(here("analysis/R/apply_winsorization.R"))

# For employment ----
out_var <- "emp"

# Selected periods
periods <- c(3, 12, 24, 36)
# periods <- c(0, 1, 12, 36) # As for horizons
# periods <- c(0, 1, 18, 36) # As for horizons

# Create a list of plots
plot_list <- lapply(periods, function(prd) {
  p <- create_funnel_plot(d_no_qc %>% filter(outcome == out_var),
                          outvar = out_var,
                          prd = prd,
                          se_option = "upper",
                          legend = FALSE,
                          wins = wins_para,
                          opac = 0.25,
                          ap = FALSE,
                          type = "standard")
  # Add annotation for period
  p <- p %>% add_annotations(
    text = paste("Period", prd),
    x = 0.5,
    y = 1,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 14)
  )
  
  return(p)
})


# Combine plots into a subplot
figure_funnel_emp <- subplot(plot_list, nrows = 4, shareX = TRUE, shareY = TRUE) %>%
  layout(title = "")

# Display the subplot
figure_funnel_emp

# Save pdf
orca(figure_funnel_emp,
     file = "analysis/working_paper_2/figures/funnel_plots/figure_funnel_emp.pdf",
     scale = NULL,
     width = 800,
     height = 1000
)

# For unemployment rate ----
out_var <- "unemp"

# Create a list of plots
plot_list <- lapply(periods, function(prd) {
  p <- create_funnel_plot(d_no_qc %>% filter(outcome == out_var),
                          outvar = out_var,
                          prd = prd,
                          se_option = "lower",
                          legend = FALSE,
                          wins = wins_para,
                          opac = 0.3,
                          ap = FALSE,
                          type = "standard")
  # Add annotation for period
  p <- p %>% add_annotations(
    text = paste("Period", prd),
    x = 0.5,
    y = 1,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 14)
  )
  
  return(p)
})

# Combine plots into a subplot
figure_funnel_unemp <- subplot(plot_list, nrows = 4, shareX = TRUE, shareY = TRUE) %>%
  layout(title = "")

# Display the subplot
figure_funnel_unemp

# Save pdf
orca(figure_funnel_unemp,
     file = "analysis/working_paper_2/figures/funnel_plots/figure_funnel_unemp.pdf",
     scale = NULL,
     width = 800,
     height = 1000
)

# For interest rate ----
out_var <- "rate"

# Create a list of plots
plot_list <- lapply(periods, function(prd) {
  p <- create_funnel_plot(d_no_qc %>% filter(outcome == out_var),
                          outvar = out_var,
                          prd = prd,
                          se_option = "avg",
                          legend = FALSE,
                          wins = wins_para,
                          opac = 0.3,
                          ap = FALSE,
                          type = "standard")
  # Add annotation for period
  p <- p %>% add_annotations(
    text = paste("Period", prd),
    x = 0.5,
    y = 1,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    showarrow = FALSE,
    font = list(size = 14)
  )
  
  return(p)
})

# Combine plots into a subplot
figure_funnel_rate <- subplot(plot_list, nrows = 4, shareX = TRUE, shareY = TRUE) %>%
  layout(title = "")

# Display the subplot
figure_funnel_rate

# Save pdf
orca(figure_funnel_rate,
     file = "analysis/working_paper_2/figures/funnel_plots/figure_funnel_rate.pdf",
     scale = NULL,
     width = 800,
     height = 1000
)

