test_that("euclidean handles positive numbers correctly", {
  expect_equal(euclidean(123612, 13892347912), 4)
  expect_equal(euclidean(100, 1000), 100)
  expect_equal(euclidean(13892347912, 123612), 4)
})

test_that("euclidean throws errors on invalid input", {
  expect_error(euclidean("a", 100))
  expect_error(euclidean(100, c(1, 2)))
})
