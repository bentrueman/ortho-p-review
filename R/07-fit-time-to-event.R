
#------------------ setup ------------------

library("brms")
library("survival")
library("tidyverse")

options(mc.cores = 4)

#------------------ inputs ------------------

model_in <- read_csv(here::here("data/time-to-event-input.csv"))

#------------------ model ------------------

bform <- bf(data | cens(cens) ~ 1 + (1 | study))

# fit time-to-event model:

priors <- c(
  prior(normal(5.8, 1), class = Intercept),
  prior(normal(0, 1), class = sd),
  prior(gamma(4, 2), class = shape)
)

flat_priors <- c(
  prior("", class = Intercept),
  prior("", class = sd),
  prior("", class = shape)
)

fit <- brm(
  bform,
  family = Gamma(link = "log"),
  data = model_in,
  seed = 2145,
  prior = priors,
  control = list(adapt_delta = .95),
  file = here::here("models/model-gamma"),
  file_refit = "on_change"
)

fit_noprior <- brm(
  bform,
  family = Gamma(link = "log"),
  data = model_in,
  seed = 2145,
  prior = priors,
  control = list(adapt_delta = .95),
  file = here::here("models/model-gamma-noprior"),
  file_refit = "on_change"
)

# fit prior simulation (time-to-event model):

fit_prior <- brm(
  bform,
  family = Gamma(link = "log"),
  data = model_in,
  cores = 4,
  seed = 2145,
  prior = priors,
  sample_prior = "only",
  control = list(adapt_delta = .95),
  file = here::here("models/prior-gamma"),
  file_refit = "on_change"
)

#------------------ survival function ------------------

surv_mod <- survfit(Surv(data, event) ~ 1, data = mutate(model_in, event = cens == "none"))
surv_fun <- summary(surv_mod)

with(surv_fun, data.frame(time, surv)) %>% 
  write_csv(here::here("data/survival-fun-km.csv"))

# explanation of 'surv_fun' object:

# ----------- surv_fun$time -----------
# these are the identified stabilization times

# ----------- surv_fun$n.risk -----------
# these are the numbers of pipes that still might be determined 
# stable at each identified stabilization time

# ----------- surv_fun$n.event -----------
# these are the numbers of pipes determined 
# stable at each identified stabilization time

# ----------- surv_fun$surv -----------
# these are the probabilities of remaining unstable beyond each identified 
# stabilization time

# calculate survival function "by hand":

# by_hand <- model_in %>%
#   # remove missings:
#   filter(!is.na(data)) %>%
#   group_by(data, cens) %>%
#   # count number of events for each unique time and status:
#   summarize(q_k = n()) %>%
#   ungroup() %>%
#   mutate(
#     cum_q_k = cumsum(q_k), # cumulative number of events (censored observations count as "events" here)
#     r_k = sum(q_k) - lag(cum_q_k), # total events less cumulative events at prev. timestep
#     r_k = replace_na(r_k, sum(q_k)), # start at total events (all surviving at t < t_min)
#     s_k = (r_k - q_k) / r_k # probability of survival
#   ) %>%
#   filter(cens == "none") %>%
#   mutate(s_k = cumprod(s_k)) # prob of survival at each event time
# 
# all.equal(by_hand$s_k, surv_fun$surv) # compare
# 
# 
# 
