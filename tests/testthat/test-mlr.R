test_that("mlr works", {

  #check coefficients
  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$summary,
                (summary(lm(formula = mpg ~ wt + hp +vs, data = mtcars)))$coefficients )

  #check variance-covariance matrix
  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$vcov.matrix,
                vcov(lm(formula = mpg ~ wt + hp +vs, data = mtcars)) )
  #check F statistic value
  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$FP[1],
                (summary(lm(formula = mpg ~ wt + hp +vs, data = mtcars))$fstatistic["value"]) )

  #check anova table
  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$ANOVA.table,
                as.matrix(anova(lm(formula = mpg ~ wt + hp +vs, data = mtcars))) )



  })

