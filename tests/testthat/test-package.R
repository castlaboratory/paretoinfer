test_that("package loads and has no exports yet", {
  expect_true(requireNamespace("paretoinfer", quietly = TRUE))
  expect_length(getNamespaceExports("paretoinfer"), 0L)
})
