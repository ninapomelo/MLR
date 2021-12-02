#'mlr
#'
#'Conduct basic analyses with multiple linear regression
#'
#'@param outcome the name of the response variable in the model
#'@param covs a vector of covariate name(s) you'd like to involve in the model
#'@param data an object of class "data.frame" such as a dataset
#'
#'@return A list of analytical results
#'@return summary: a table of estimated coefficients
#'@return y.fitted: model-fitted response values
#'@return y.res: residuals of response based on the model
#'@return vcov.matrix: variance-covariance matrix
#'@return ANOVA.table: ANOVA results
#'@return A warning message will be given instead of the analytical results, if collinearity is found in the input dataset.
#'
#'@examples
#'data(longley)
#'mlr("Employed", c("GNP.deflator","GNP","Unemployed","Armed.Forces","Population"), longley)
#'
#'@export
#'

mlr = function(outcome, covs, data) {
  #Regression Formula
  #cat("The regression model is:\n")
  print("The regression model is:\n")
  F_outcome = paste(outcome,"~ intercept +")
  F_cov = paste(covs, collapse = " + ")
  #cat(paste(F_outcome,F_cov))
  print(paste(F_outcome,F_cov))
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

    # conduct summary table
    res.sum = cbind(Estimate, Std.error, t.value, p.val)
    rownames(res.sum) = c("Intercept",colnames(cov.info))
    colnames(res.sum) = c("Estimate","Std.error","t.value", "p.value")

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
    FP_statistic <- c(f_value, p_value)
    names(FP_statistic) <- c("F_statistic", "P_value")

    # Compute R-squared and adjusted R-squared
    rsq <- SSR / SSY
    rsq_adj <- 1 - (1 - rsq) * (n - 1) / (n - p)
    rsq_vec <- c(rsq,rsq_adj)
    names(rsq_vec) <- c("Standard", "Adjusted")


    # Compute variance and covariance
    vcov.mat = var.beta.mat
    rownames(vcov.mat) = c("Intercept",colnames(cov.info))
    colnames(vcov.mat) = c("Intercept",colnames(cov.info))


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
    colnames(anova) = c("Df", "Sum.Sq", "Mean.Sq", "F.value", "p.value")
    Residuals = NA
    anova = rbind(anova, Residuals)
    anova["Residuals",1] = nrow(X)-ncol(X)
    anova["Residuals",2] = SSE
    anova["Residuals",3] = sigma.sq.est
  }

  if(error == 1) {cat("Collinearity existed!")} else {
    #return(list(summary = res.sum, y.fitted = t(y.fitted), y.res=t(y.res), vcov.matrix = vcov.mat, ANOVA.table = anova))
    return(list(y.fitted = t(y.fitted), y.res=t(y.res), summary = res.sum, FP = FP_statistic, vcov.matrix = vcov.mat, ANOVA.table = anova,
                R_Square = rsq_vec))
  }
}


