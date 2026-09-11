test_that("nonexistent version", {
  expect_error(getDataset("GSE41197", v=1000000))
})

test_that("getDataset returns dataset", {
    result <- getDataset("GSE41197")
    expect_s4_class(result, "SummarizedExperiment")
})

test_that("nonexistent dataset", {
    expect_error(getDataset("hello world"))
})