
#------------------ setup ------------------

library("brms")
library("bgamcar1")
library("dplyr")
library("readr")
library("assertr", include.only = "verify")

options(mc.cores = parallel::detectCores())

#------------------ inputs ------------------

data_full <- read_csv(here::here("data/data-simulated.csv"))

data_nonseasonal <- data_full %>%
  filter(type == "nonseasonal") %>% 
  mutate(scaled_value = scale(log(value))[,1]) %>% 
  arrange(study, series)

data_nonseasonal_diss <- data_full %>% 
  filter(type == "nonseasonal_dissolved") %>% 
  mutate(scaled_value = scale(log(value))[,1]) %>% 
  arrange(study, series)
  
stan_seed <- 3256147 # random seed

# formula:

form_tot <- bf(
  scaled_value | cens(censored) ~ 
    1 + (1 | series) + 
    s(date_numeric, by = series) + 
    ar(time = date_numeric, gr = series) 
)

form_diss <- bf(
  scaled_value | cens(censored) ~ 
    1 + (1 | series) + 
    s(date_numeric, by = study) +
    s(date_numeric, by = series, m = 1) + 
    ar(time = date_numeric, gr = series) 
)

# prior:

prior_tot <- c(
  prior(normal(0.5, .25), class = ar, lb = 0, ub = 1),
  prior(student_t(3, 0, 2.5), class = b),
  prior(gamma(2, .5), class = nu)
)

#------------------ model ------------------

# n.b., takes ~ 2.2 hrs to fit on a 2017 Macbook Pro

model_nonseasonal <- fit_stan_model(
  here::here("models/model-non-seasonal"),
  seed = stan_seed,
  form_tot, data_nonseasonal, prior_tot,
  save_warmup = FALSE,
  iter = 2500
)

# n.b., takes ~5.2 hrs to fit on a 2017 Macbook Pro

model_nonseasonal_diss <- fit_stan_model(
  here::here("models/model-non-seasonal-diss"),
  seed = stan_seed,
  form_diss, data_nonseasonal_diss, prior_tot,
  save_warmup = FALSE,
  control = list(max_treedepth = 13, adapt_delta = 0.99),
  iter = 3000
)

