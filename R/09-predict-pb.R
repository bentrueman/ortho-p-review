
#------------------ setup ------------------

library("tidyverse")
library("pbcusol")
library("chemr")
library("assertr")
library("glue")
library("withr")

#------------------ data ------------------

stable_here <- read_csv("data/time-to-event-input-diss.csv") %>% 
  # (if stabilization time is right-censored, use last data point form each series)
  transmute(study, stable_here = data) 

# the initial P dose in study 4:

stable_here_study4 <- read_csv("data/time-to-event-input-diss-study4.csv") %>% 
  transmute(study, stable_here = data)

data_raw <- read_csv(here::here("data/data-simulated.csv")) %>% 
  filter(type %in% c("nonseasonal_dissolved", "seasonal_dissolved"))

wq <- read_csv(here::here("data/data-wq-summary.csv"))

#------------------ combine ------------------

data_filtered <- data_raw %>%
  left_join(stable_here, by = "study") %>%
  left_join(stable_here_study4, by = "study", suffix = c("", "_study4")) %>%
  verify(is.na(stable_here_study4)) %>% 
  group_by(study) %>% 
  mutate(
    # if stable period is NA, replace with first date:
    min_date = min(date_numeric),
    stable_here = if_else(is.na(stable_here), pmax(0, min_date), stable_here),
    # if stable period is NA, replace with first date:
    stable_here_study4 = if_else(is.na(stable_here_study4), min_date, as.numeric(stable_here_study4))
  ) %>% 
  ungroup() %>%
  filter(
    date_numeric >= stable_here | 
      # include data representing initial 0.5 mg PO4/L dose:
      (date_numeric >= stable_here_study4 & date_numeric <= 0 & study == "Study 4")
  ) %>% 
  verify(date_numeric >= 0 | study == "Study 4") %>% 
  mutate(phase = if_else(date_numeric < 0, 1, 2)) %>% 
  left_join(wq, by = c("study", "series", "phase")) %>% 
  verify(!is.na(ph_mean))

with_seed(1, {
  data <- data_filtered %>% 
    # simiulate water quality (p, ph, dic) from summary statistics:
    group_by(study, series, phase) %>%
    mutate(
      ph = rnorm(length(value), ph_mean, ph_sd),
      dic = rnorm(length(value), dic_mean, dic_sd),
      p_ppm = rnorm(length(value), p_ppm_mean, p_ppm_sd),
    ) %>%
    ungroup() %>% 
    select(study, series, date_numeric, ph, dic, p_ppm, value)
})

#------------------ predictions ------------------

preds <- data %>% 
  rowwise() %>% 
  mutate(
    leadsol = list(pb_sol(ph, dic, phosphate = p_ppm, phase = "Hxypyromorphite")),
    # minteq = list(pb_sol(ph, dic, phosphate = p_ppm, phase = "Hxypyromorphite", db = phreeqc::minteq.dat)),
    minteq_v4 = list(pb_sol(ph, dic, phosphate = p_ppm, phase = "Hydroxylpyromorphite", db = phreeqc::minteq.v4.dat))
  ) %>% 
  ungroup() %>% 
  mutate(
    # rename:
    leadsol = map(leadsol, ~ rename_all(.x, \(y) paste0(y, "_leadsol"))),
    # minteq = map(minteq, ~ rename_all(.x, \(y) paste0(y, "_minteq"))),
    minteq_v4 = map(minteq_v4, ~ rename_all(.x, \(y) paste0(y, "_minteqv4")))
  ) %>% 
  unnest(c(leadsol, minteq_v4))

preds_long <- preds %>%
  # verify that calculated pH matches measured pH:
  verify(near(ph, pH_leadsol)) %>% 
  # verify(near(ph, pH_minteq)) %>% 
  verify(near(ph, pH_minteqv4)) %>%
  # verify that calculated DIC matches measured DIC:
  verify(near(dic, dic_ppm_leadsol, tol = 1e-1)) %>% 
  # verify(near(dic, dic_ppm_minteq, tol = 1e-1)) %>% 
  verify(near(dic, dic_ppm_minteqv4, tol = 1e-1)) %>% 
  pivot_longer(starts_with("pb_ppb"), names_to = "database", values_to = "prediction")

#------------------ write ------------------

write_csv(preds_long, "data/preds-pbcusol.csv")

