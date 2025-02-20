# `deparse_fun_body` produces character string of function body

    Code
      extpkg_body
    Output
      [1] "UseMethod(\"t\")"

---

    Code
      predef_body
    Output
      [1] "{\n    z <- x + y\n    return(z)\n}"

# `is_response_in_data` correctly gives error when column not in data [plain]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      Error in `is_response_in_data()`:
      ! Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `is_response_in_data` correctly gives error when column not in data [ansi]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      [1m[33mError[39m in `is_response_in_data()`:[22m
      [1m[22m[33m![39m Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `is_response_in_data` correctly gives error when column not in data [unicode]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      Error in `is_response_in_data()`:
      ! Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `is_response_in_data` correctly gives error when column not in data [fancy]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      [1m[33mError[39m in `is_response_in_data()`:[22m
      [1m[22m[33m![39m Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `print_symbolic_differentiation` provides message [plain]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * test string add
    Output
      function (psi0, psi1) 
      1
      

# `print_symbolic_differentiation` provides message [ansi]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      [36mi[39m Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * test string add
    Output
      function (psi0, psi1) 
      1
      

# `print_symbolic_differentiation` provides message [unicode]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      ℹ Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      • test string add
    Output
      function (psi0, psi1) 
      1
      

# `print_symbolic_differentiation` provides message [fancy]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      [36mℹ[39m Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      • test string add
    Output
      function (psi0, psi1) 
      1
      

