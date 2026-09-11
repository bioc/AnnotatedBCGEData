test_that("get metadata tibble for a dataset", {
  result <- getDatasetMetadata(c("GSE41197"))
  expect_s3_class(result, "tbl_df")
})

test_that("nonexistent dataset metadata", {
    result <- getDatasetMetadata(c("kitty", "cat"))
    expect_equal(nrow(result), 0)
})

test_that("existing dataset metadata", {
    result <- getDatasetMetadata(c("GSE41197"))
    expect_true(nrow(result)>0)
})
