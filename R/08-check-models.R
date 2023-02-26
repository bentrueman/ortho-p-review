
source(here::here("R/02-fit-gam-non-seasonal.R"))
source(here::here("R/03-fit-gam-seasonal.R"))
source(here::here("R/07-fit-time-to-event.R"))

library("posterior")
library("bayesplot")
library("withr")
library("patchwork")
library("testthat")

# ---------------------- seasonal gam ----------------------

loo_cv(data_seasonal, model_seasonal)

draws_seasonal <- as_draws_df(model_seasonal) %>% 
  posterior::summarize_draws()

test_that("rhats are < 1.05", {
  expect_true(all(draws_seasonal$rhat < 1.05))
})

test_that("ess bulk greater than 400", {
  expect_true(all(draws_seasonal$ess_bulk > 400))
})

test_that("ess tail greater than 400", {
  expect_true(all(draws_seasonal$ess_tail > 400))
})

# ---------------------- non-seasonal gam ----------------------

loo_cv(data_nonseasonal, model_nonseasonal)

draws_nonseasonal <- as_draws_df(model_nonseasonal) %>% 
  posterior::summarize_draws()

test_that("rhats are < 1.05", {
  expect_true(all(draws_nonseasonal$rhat < 1.05))
})

test_that("ess bulk greater than 400", {
  expect_true(all(draws_nonseasonal$ess_bulk > 400))
})

test_that("ess tail greater than 400", {
  expect_true(all(draws_nonseasonal$ess_tail > 400))
})

# ---------------------- seasonal gam diss ----------------------

loo_cv(data_seasonal_diss, model_seasonal_diss)

draws_seasonal <- as_draws_df(model_seasonal_diss) %>% 
  posterior::summarize_draws()

test_that("rhats are < 1.05", {
  expect_true(all(draws_seasonal$rhat < 1.05))
})

test_that("ess bulk greater than 400", {
  expect_true(all(draws_seasonal$ess_bulk > 400))
})

test_that("ess tail greater than 400", {
  expect_true(all(draws_seasonal$ess_tail > 400))
})

# ---------------------- non-seasonal gam diss ----------------------

loo_cv(data_nonseasonal_diss, model_nonseasonal_diss)

draws_nonseasonal <- as_draws_df(model_nonseasonal_diss) %>% 
  posterior::summarize_draws()

test_that("rhats are < 1.05", {
  expect_true(all(draws_nonseasonal$rhat < 1.05))
})

test_that("ess bulk greater than 400", {
  expect_true(all(draws_nonseasonal$ess_bulk > 400))
})

test_that("ess tail greater than 400", {
  expect_true(all(draws_nonseasonal$ess_tail > 400))
})

# ---------------------- time-to-event ----------------------

loo(fit)

draws_time <- as_draws_df(fit) %>% 
  posterior::summarize_draws()

test_that("rhats are < 1.05", {
  expect_true(all(draws_time$rhat < 1.05))
})

test_that("ess bulk greater than 400", {
  expect_true(all(draws_time$ess_bulk > 400))
})

test_that("ess tail greater than 400", {
  expect_true(all(draws_time$ess_tail > 400))
})

