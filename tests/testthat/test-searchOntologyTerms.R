test_that("get a tibble from searching ontology", {
  result <- searchOntologyTerms("progesterone")
  expect_s3_class(result, "tbl_df")
})

test_that("nonexistent term", {
    result <- searchOntologyTerms("i love kitty cats")
    expect_equal(nrow(result), 0)
})

test_that("nonexistent term type", {
    expect_error(searchOntologyTerms("progesterone status", term_type="kitty cat"))
})

test_that("searching ontology finds terms", {
    result <- searchOntologyTerms("PgR Status")
    expect_true(nrow(result)>0)
})