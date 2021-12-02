#'Cov_selected
#'
#'Select covariates using the significance level
#'
#'@param output the result object from mlr function
#'@param alpha the given threshold of p-value. If not provided, 0.05 is used.
#'
#'@return A vector of selected covariates names
#'
#'@examples
#'data(mtcars)
#'output <- mlr("mpg", c("wt","hp","vs"), data=mtcars)
#'Cov_selected(output)
#'
#'@export
#'

Cov_selected <- function(output, alpha=0.05) {
  cov_selected = rownames(output$summary[output$summary[,"p.value"] <= alpha, ])
  return(cov_selected)
}

