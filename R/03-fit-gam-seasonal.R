
#------------------ setup ------------------

library("brms")
library("bgamcar1")
library("dplyr")
library("readr")

options(mc.cores = parallel::detectCores())

#------------------ inputs ------------------

data_full <- read_csv(here::here("data/data-simulated.csv"))

data_seasonal <- data_full %>% 
  filter(type == "seasonal") %>% 
  mutate(scaled_value = scale(log(value))[,1])

data_seasonal_diss <- data_full %>% 
  filter(type == "seasonal_dissolved") %>% 
  mutate(scaled_value = scale(log(value))[,1])

knots_yday <- c(0, 1)

stan_seed <- 2147 # random seed

# formula:

form_tot <- bf(
  scaled_value | cens(censored) ~ 
    1 + (1 | series) + 
    s(date_numeric, by = series) + 
    s(date_yday, by = study, bs = "cc") + 
    ar(time = date_numeric, gr = series) 
)

form_diss <- bf(
  scaled_value | cens(censored) ~ 
    1 + (1 | series) + 
    s(date_numeric, by = study) +
    s(date_numeric, by = series, m = 1) + 
    s(date_yday, by = study, bs = "cc") + 
    ar(time = date_numeric, gr = series) 
)

# prior:

prior_tot <- c(
  prior(normal(0.5, .25), class = ar, lb = 0, ub = 1),
  prior(student_t(3, 0, 2.5), class = b),
  prior(gamma(2, .5), class = nu),
  prior(normal(0, .5), class = sds, coef = `s(date_yday, by = study, bs = "cc")`)
)

#------------------ model ------------------

# n.b., takes ~1.3 hrs to fit on a 2017 Macbook Pro

model_seasonal <- fit_stan_model(
  here::here("models/model-seasonal"),
  seed = stan_seed,
  form_tot, data_seasonal, prior_tot,
  save_warmup = FALSE,
  control = list(max_treedepth = 12),
  knots = list(date_yday = knots_yday)
)

# n.b., takes ~0.35 hrs to fit on a 2017 Macbook Pro

model_seasonal_diss <- fit_stan_model(
  here::here("models/model-seasonal-diss"),
  seed = stan_seed,
  form_diss, data_seasonal_diss, prior_tot,
  save_warmup = FALSE,
  control = list(max_treedepth = 12),
  knots = list(date_yday = knots_yday)
)

