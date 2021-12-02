test_that("mlr works", {

  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$summary,
                (summary(lm(formula = mpg ~ wt + hp +vs, data = mtcars)))$coefficients )

  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$vcov.matrix,
                vcov(lm(formula = mpg ~ wt + hp +vs, data = mtcars)) )

  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$FP[1],
                (summary(lm(formula = mpg ~ wt + hp +vs, data = mtcars))$fstatistic["value"]) )

  expect_equal( (mlr("mpg", c("wt","hp","vs"), data=mtcars))$ANOVA.table,
                as.matrix(anova(lm(formula = mpg ~ wt + hp +vs, data = mtcars))) )



  })

