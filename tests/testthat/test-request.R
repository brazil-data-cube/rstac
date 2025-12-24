test_that("simplify_vector_argument returns TRUE when no argument is supplied", {
  testthat::expect_true(simplify_vector_argument())
})

test_that("simplify_vector_argument returns TRUE when simplify_vector = TRUE", {
  testthat::expect_true(simplify_vector_argument(TRUE))
})

test_that("simplify_vector_argument returns FALSE when simplify_vector = FALSE", {
  testthat::expect_false(simplify_vector_argument(FALSE))
})

test_that("simplify_vector_argument returns TRUE when no argument is supplied, but option is specified as TRUE", {
  testthat::expect_true({
    options(rstac.simplify_vector = TRUE)
    simplify_vector_argument()
  })

  # Reset option
  options(rstac.simplify_vector = NULL)
})

test_that("simplify_vector_argument returns FALSE when no argument is supplied, but option is specified as FALSE", {
  testthat::expect_false({
    options(rstac.simplify_vector = FALSE)
    simplify_vector_argument()
  })

  # Reset option
  options(rstac.simplify_vector = NULL)
})

test_that("simplify_vector_argument respects argument over option -- returning TRUE when argument is TRUE but option is FALSE, and FALSE when argument is FALSE but option is TRUE", {
  testthat::expect_true({
    options(rstac.simplify_vector = FALSE)
    simplify_vector_argument(TRUE)
  })

  testthat::expect_false({
    options(rstac.simplify_vector = TRUE)
    simplify_vector_argument(FALSE)
  })

  # Reset option
  options(rstac.simplify_vector = NULL)
})
