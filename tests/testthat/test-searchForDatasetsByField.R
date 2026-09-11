test_that("searching by field returns tibble", {
  result <- searchForDatasetsByField("C16149")
  expect_s3_class(result, "tbl_df")
})

test_that("nonexistent field code", {
    result <- searchForDatasetsByField("kittycat")
    expect_equal(nrow(result), 0)
})

test_that("existing field code returns tibble with rows", {
    result <- searchForDatasetsByField("C16149")
    expect_true(nrow(result)>0)
})
