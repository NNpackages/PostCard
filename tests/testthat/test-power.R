test_that("`power_gs` works", {
  withr::local_seed(42)
  expect_snapshot(power_gs(rho = 0.7))
  expect_snapshot(power_gs(R2 = 0.8))
})

test_that("`power_nc` works", {
  withr::local_seed(42)
  expect_snapshot(power_nc(rho = 0.7))
  expect_snapshot(power_nc(R2 = 0.8, n.adj = 4))
})

test_that("`power_nc` gives error when `n.adj` not given", {
  expect_error(power_nc(R2 = 0.8),
               regexp = "Adjust the specification")
})
