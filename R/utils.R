# Get names of arguments of a function
get_fun_args <- function(fun) {
  names(formals(fun))
}

# To enable nice character priting of a function definition
deparse_fun_body <- function(fun) {
  body_as_char <- gsub(
    "\\[\\d*\\]\\s*", "", utils::capture.output(body(fun))
  )
  out <- paste(body_as_char, collapse = "\n")
  return(out)
}

# Transform character or family function to a call
check_formula <- function(formula) {
  if (is.character(formula)) {
    formula <- formula(formula)
  }
  if (!inherits(formula, "formula")) {
    cli::cli_abort("{.arg formula} needs to have class `formula` or `character`")
  }

  return(formula)
}

formula_to_str <- function(formula) {
  deparse(formula)
}

# Extract response from formula to
get_response_from_formula <- function(formula) {
  formula <- check_formula(formula)

  formula <- formula_to_str(formula)
  lhs_oftilde <- gsub("\\s*~.*", "", formula)
  return(lhs_oftilde)
}

# Checks if response is present in data from a formula
is_response_in_data <- function(formula, data) {
  response_var_name <- get_response_from_formula(formula)

  if (!response_var_name %in% colnames(data))
    cli::cli_abort("Tried to create formula to fit prognostic model but did not find the response variable {.var {response_var_name}} specified in the primary formula.\nProvide a formula manually through the argument {.arg prog_formula}.")

  return(invisible())
}

# Create formula that is function of
formula_everything <- function(formula) {
  response_var_name <- get_response_from_formula(formula)
  formula(
    paste0(response_var_name, " ~ ."),
    env = parent.frame()
  )
}

# Get names of arguments containing 0 and 1 from function
get01args <- function(fun) {

  arg0 <- grep("0$", get_fun_args(fun), value = TRUE)
  arg1 <- grep("1$", get_fun_args(fun), value = TRUE)

  if (length(arg0) == 0 | length(arg1) == 0) {
    cli::cli_abort("Arguments of the {.var estimand_fun} need to end in {.code 0} and {.code 1} to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_fun_deriv0} and {.var estimand_fun_deriv1}, manually.")
  }

  return(list(arg0 = arg0, arg1 = arg1))
}

# Perform symbolic differentiation of function and print message
print_symbolic_differentiation <- function(fun, arg, add_string = "", verbose = options::opt("verbose")) {
  derivative <- Deriv::Deriv(fun, arg)

  body_of_fun <- deparse_fun_body(fun)
  body_of_derivative <- deparse_fun_body(derivative)

  if (verbose >= 1) {
    cli::cli_alert_info("Symbolically deriving partial derivative of the function '{body_of_fun}' with respect to '{arg}' as: '{body_of_derivative}'.\n")
    if (stringr::str_length(add_string) > 0)
      cli::cli_ul(add_string)
  }

  return(derivative)
}
