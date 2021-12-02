#'mlr
#'
#'Conduct basic analyses with multiple linear regression
#'
#'@param outcome the name of the response variable in the model
#'@param covs a vector of covariate name(s) you'd like to involve in the model
#'@param data an object of class "data.frame" such as a dataset
#'
#'@return A list containing model details, y.fitted values, y.res, summary(coefficients), FP(F statistic and P value),
#'vcov.matrix(variance-covariance matrix), ANOVA.table, R_Square
#'@return If there exists collinearity between covariates, a warning message will be given instead of above results.
#'
#'@examples
#'data=mtcars
#'mlr("mpg", c("wt","hp","vs"), data=mtcars)
#'
#'@export
#'
