#' Display Equation for Meta-Analysis Models
#'
#' This function generates the LaTeX representation of the equation
#' for different meta-analysis models.
#'
#' @param model A string specifying the model type. Can be "Mean", "FAT-PET", or "PEESE".
#' @param weighted A logical indicating whether the model is precision-weighted.
#'
#' @return A string containing the LaTeX representation of the equation.
#'
#' @examples
#' display_equation("Mean", FALSE)
#' display_equation("UWLS", FALSE)
#' display_equation("FAT-PET", TRUE)
#' display_equation("PEESE", FALSE)
#'
#' @export
display_equation <- function(model, weighted) {
  eq <- switch(model,
               "Mean" = "y = \\beta + \\epsilon",
               "UWLS" = "t = y/SE = \\beta (1/SE) + \\epsilon",
               "FAT-PET" = "y = \\beta_0 + \\beta_1 SE + \\epsilon",
               "PEESE" = "y = \\beta_0 + \\beta_1 SE^2 + \\epsilon",
               stop("Invalid model specified")
  )
  
  if (weighted) {
    eq <- paste0(eq, "\\quad (\\text{weighted by } 1/SE^2)")
  }
  
  paste0("$$", eq, "$$")
}