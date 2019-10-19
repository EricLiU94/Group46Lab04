test_check("ridgereg") 
data("iris")

test_that("lenreg rejects errounous input", {
  expect_error(linreg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linreg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})

test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})