test_that("mlr works", {
  expect_equal(round((mlr("mpg", c("wt","hp","vs"), data=mtcars))$summary,5),round((summary(lm(formula = mpg ~ wt + hp +vs, data = mtcars)))$coefficients,5) )

  })



