test_that("myncurve works", {
  output = FALL244753doug0080::myncurve(mu = 10, sigma = 5, a = 6)
  expect_length(output, 4)
  expect_setequal(output["mu"], 10)
  expect_setequal(output['prob'], 0.2119)
})
