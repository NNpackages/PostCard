# get_fun_args
test_that("`get_fun_args` gives argument names as vector", {
  extpkg_args <- get_fun_args(base::t)
  thispkg_args <- get_fun_args(get01args)
  f <- function(x, y) x+y
  predef_args <- get_fun_args(f)
  anon_args <- get_fun_args(\(a, b) a-b)

  expect_equal(extpkg_args, "x")
  expect_equal(thispkg_args, "fun")
  expect_equal(predef_args, c("x", "y"))
  expect_equal(anon_args, c("a", "b"))
})

# deparse_fun_body
test_that("`deparse_fun_body` produces character string of function body", {
  extpkg_body <- deparse_fun_body(base::t)
  thispkg_body <- deparse_fun_body(get01args)
  f <- function(x, y) {
    z <- x+y
    return(z)
  }
  predef_body <- deparse_fun_body(f)
  anon_body <- deparse_fun_body(\(a, b) a-b)

  expect_type(extpkg_body, "character")
  expect_length(extpkg_body, 1)
  expect_snapshot(extpkg_body)

  expect_type(thispkg_body, "character")
  expect_length(thispkg_body, 1)

  expect_type(predef_body, "character")
  expect_length(predef_body, 1)
  expect_snapshot(predef_body)

  expect_type(anon_body, "character")
  expect_length(anon_body, 1)
  expect_equal(anon_body, "a - b")
})

# get_response_from_formula
test_that("`get_response_from_formula` works for different formula specifiations", {
  expect_equal(get_response_from_formula(Y ~ A),
               "Y")
  expect_equal(get_response_from_formula(formula("testing~bla")),
               "testing")
})

# is_response_in_data
cli::test_that_cli("`is_response_in_data` correctly gives error when column not in data", {
  dat <- data.frame(A = 1:2, B = 4:5)
  expect_snapshot(
    error = TRUE,
    is_response_in_data(formula = C ~ A, data = dat))
})

test_that("`is_response_in_data` does not give error when column is in data", {
  dat <- data.frame(A = 1:2, B = 4:5)
  expect_no_error(
    is_response_in_data(formula = B ~ A, data = dat)
  )
})

# formula_everything
test_that("`formula_everything` returns the correct formula object", {
  expect_equal(
    formula_everything(Y ~ A),
    Y ~ .)
  expect_equal(
    formula_everything(resp ~ A + B + mph * fpg),
    resp ~ .)
})

# get01args
test_that("`get01args` returns list of arguments with 0 and 1 in them", {
  ate <- function(psi0, psi1) psi1 - psi0
  expect_equal(
    get01args(fun = ate),
    list(arg0 = "psi0", arg1 = "psi1")
  )
  some_fun <- function(blablabla0, c1) sum(log(blablabla0), sqrt(c1))
  expect_equal(
    get01args(fun = some_fun),
    list(arg0 = "blablabla0", arg1 = "c1")
  )
  fun_with_additionalargs <- function(psi0, psi1, add_arg, ...) psi1 - psi0
  expect_equal(
    get01args(fun = fun_with_additionalargs),
    list(arg0 = "psi0", arg1 = "psi1")
  )
})

cli::test_that_cli("`get01args` gives error when arguments with 0 and 1 are missing", {
  argsnotendingwith0and1 <- function(psi0, psi18) psi18 - psi0
  expect_error({
    get01args(argsnotendingwith0and1)
    },
    regexp = "need to end in"
  )
  missing1 <- function(a0, b) a0 - b
  expect_error({
    get01args(missing1)
  },
  regexp = "need to end in"
  )
  missing0 <- function(a, b1) a - b1
  expect_error({
    get01args(missing0)
  },
  regexp = "need to end in"
  )
  # missing both
  expect_error({
    get01args(sum)
  },
  regexp = "need to end in"
  )
})

# print_symbolic_differentiation
test_that("`print_symbolic_differentiation` returns the result of Deriv", {
  ate <- function(psi0, psi1) psi1 - psi0
  expect_equal(
    print_symbolic_differentiation(ate, "psi0"),
    Deriv::Deriv(ate, "psi0")
  )
})

cli::test_that_cli("`print_symbolic_differentiation` provides message", {
  withr::local_options(PostCard.verbose = 1)
  ate <- function(psi0, psi1) psi1 - psi0
  # Note we are using the transform argument to remove printing of
  # the environment which changes between each run
  expect_snapshot(
    print_symbolic_differentiation(
      ate,
      "psi1",
      add_string = "test string add"
    ),
    transform = function(x) gsub("^<environment:.*>$", "", x)
  )
})
