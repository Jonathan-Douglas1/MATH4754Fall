test_that("myncurve works", {
  list = myncurve(mu = 10, sigma = 5, a = 6)
  expect_length(list, 4)
  expect_setequal(list["mu"], 10)
  expect_setequal(list['prob'], 0.2119)
})
