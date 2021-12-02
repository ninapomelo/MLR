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
#'data = mtcars
#'mlr("mpg", c("wt","hp","vs"), data = mtcars)
#'
#'@export
#'

mlr <- function(outcome, covs, data) {
  #Regression Formula
  cat("The regression model is:\n")
  F_outcome = paste(outcome,"~ intercept +")
  F_cov = paste(covs, collapse = " + ")
  cat(paste(F_outcome,F_cov))
  cat("\n")

  y = data[,outcome]
  cov.info = as.matrix(data[,covs])
  X = cov.info
  rownames(X) <- NULL
  colnames(X) <- NULL
  X = cbind(1,X)

  # check colinearity
  check = t(X)%*%X
  if(det(check) == 0){
    error = 1
  }
  else{
    error = 0
    beta.est = solve(check)%*%t(X)%*%y
    SSE = t(y)%*%y - 2*t(beta.est)%*%t(X)%*%y + t(beta.est)%*%t(X)%*%X%*%beta.est
    sigma.sq.est = SSE/(nrow(X)-ncol(X))
    var.beta.mat = as.numeric(sigma.sq.est) * solve(t(X)%*%X)
    var.beta.est = diag(var.beta.mat)
    sd.beta.est = sqrt(var.beta.est)
    t.stat = beta.est / sd.beta.est
    p.val = 2*pt(-abs(t.stat), df=nrow(X)-ncol(X))

    # Change
    Estimate = beta.est
    Std.error = sd.beta.est
    t.value = t.stat

    # generate fitted values
    y.fitted = X%*%beta.est

    y.fitted = as.matrix(y.fitted)
    rownames(y.fitted) = rownames(cov.info)
    colnames(y.fitted) = "Fitted.value"

    # generate residuals
    # y = as.numeric(format(round(y, decimal), nsmall = decimal))
    y.res = y - y.fitted
    colnames(y.res) = "Residuals"
    #y.res <- as.matrix(y.res)

    # conduct summary table
    res.sum = cbind(Estimate, Std.error, t.value, p.val)
    rownames(res.sum) = c("(Intercept)",colnames(cov.info))
    colnames(res.sum) = c("Estimate","Std. Error","t value", "Pr(>|t|)")

    #SSE SSR SSY
    # F statistic and P value
    n<- nrow(X)
    p<- ncol(X)
    SSY <- sum((y - mean(y))^2)
    SSR <- SSY - SSE
    MSR <- SSR / (p - 1)
    MSE <- SSE / (n - p)
    f_value <- MSR / MSE
    p_value <- 1 - pf(f_value, p - 1, n - p)
    #FP_statistic <- c(f_value, p_value)
    #FP_statistic <- as.matrix(FP_statistic,2)
    #rownames(FP_statistic) <- c("F-statistic:", "P_value")
    #colnames(FP_statistic) <- ("value")
    F_statistic <- as.vector(f_value)
    names(F_statistic) <- "value"



    # Compute R-squared and adjusted R-squared
    rsq <- SSR / SSY
    rsq_adj <- 1 - (1 - rsq) * (n - 1) / (n - p)
    rsq_vec <- c(rsq,rsq_adj)
    names(rsq_vec) <- c("R_Standard", "R_Adjusted")

    # Compute variance and covariance
    vcov.mat = var.beta.mat
    rownames(vcov.mat) = c("(Intercept)",colnames(cov.info))
    colnames(vcov.mat) = c("(Intercept)",colnames(cov.info))

    # calculate the sequential SSR
    # an internal function to calculate SSR
    get.ssr = function(X,y) {
      y.bar = mean(y)
      beta.est = solve(t(X)%*%X)%*%t(X)%*%y
      y.est = X%*%beta.est
      ssr = sum((y.est-y.bar)^2)
      return(ssr)
    }

    # conduct ANOVA table
    SSR_list = c()
    for (i in 2:(length(covs)+1)) {
      SSR_list = c(SSR_list, get.ssr(X[,1:i],y))
    }
    SSR_list_1 = c()
    SSR_list_1[1] = SSR_list[1]
    for (i in 2:length(covs)) {
      SSR_list_1[i] = SSR_list[i] - SSR_list[i-1]
    }
    F.val = SSR_list_1/as.numeric(sigma.sq.est)
    p.val2 = pf(F.val, 1, nrow(X)-ncol(X), lower.tail = FALSE)
    anova = cbind(1,SSR_list_1,SSR_list_1,F.val,p.val2)
    rownames(anova) = covs
    colnames(anova) = c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    Residuals = NA
    anova = rbind(anova, Residuals)
    anova["Residuals",1] = nrow(X)-ncol(X)
    anova["Residuals",2] = SSE
    anova["Residuals",3] = sigma.sq.est
  }

  if(error == 1){
    cat("There Eexists Collinearity.")
    }
  else {
    return(list(y.fitted = t(y.fitted), y.res=t(y.res), summary = res.sum, FP = F_statistic, vcov.matrix = vcov.mat,
                ANOVA.table = anova, R_Square = rsq_vec))
  }
}



