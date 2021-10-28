test_that("scientific_notation returns a formatted string", {
  expect_type(scientific_notation(signif(0.00001, 1), 3), "character")
  expect_equal(scientific_notation(signif(0.00001, 1), 3), "'1'%*%10^-05")
})

test_that("scientific_notation returns error with incorrect input format", {
  expect_error(scientific_notation(signif("0.00001", 1), 3))
  expect_error(scientific_notation(signif(0.00001, "1"), 3))
})
