test_that("search by value returns tibble", {
  result <- searchForDatasetsByValue("C15496")
  expect_s3_class(result, "tbl_df")
})

test_that("nonexistent code", {
    result <- searchForDatasetsByValue("meow!")
    expect_equal(nrow(result), 0)
})

test_that("existent code returns tibble with rows", {
    result <- searchForDatasetsByValue("C15496")
    expect_true(nrow(result)>0)
})
