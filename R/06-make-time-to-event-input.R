
#------------------ setup ------------------

library("tidyverse")
library("testthat")

slopes_combined <- read_csv(here::here("data/gam-derivatives.csv"))
limits <- read_csv(here::here("data/series-limits.csv"))

slopes_combined_diss <- read_csv(here::here("data/gam-derivatives-diss.csv"))
limits_diss <- read_csv(here::here("data/series-limits-diss.csv"))

#------------------ functions ------------------

make_model_input <- function(data, limits, g_var = "series", change = "decrease", start = 0) {
  
  # stability is defined as the last day when the slope is negative, 
  # for any series that had a negative slope at some point after the increase in orthophosphate AND
  # a zero slope at the end of the series
  
  # (n.b., this function includes the possibility of an increase in 
  # Pb followed by stabilization with argument change = "increase")
  
  # negative means that both the lower 
  # and upper bound of the 95% confidence interval on the local slope are less than zero
  
  # series with zero slope at end of series:
  
  stable <- data %>% 
    group_by(.data[[g_var]]) %>% 
    filter(
      .lower_slope[which.max(date_numeric)] < 0,
      .upper_slope[which.max(date_numeric)] > 0
    ) %>% 
    ungroup() %>% 
    distinct(.data[[g_var]])
  
  # series with negative/positive slope after day 0 and zero slope at end:
  
  trend <- if (change == "decrease") {
    # series with negative slopes at some point after P increase:
    id_change(data, g_var, start, .lower_slope < 0, .upper_slope < 0)
  } else 
    if (change == "increase") {
      # series with positive slopes at some point after P increase:
      id_change(data, g_var, start, .lower_slope > 0, .upper_slope > 0)
    } else 
      stop("options for argument 'change' are 'increase' or 'decrease'")
  
  stable_trend <- intersect(stable, trend)
  
  # find time to stabilization:
  
  time_stable <- data %>% 
    filter(
      .data[[g_var]] %in% stable_trend[[g_var]],
      sign(.lower_slope) == sign(.upper_slope)
    ) %>% 
    group_by(.data[[g_var]]) %>% 
    summarize(date_numeric = max(date_numeric)) %>% 
    ungroup()
  
  out <- time_stable %>% 
    # add series that decreased (increased) but have not stabilized:
    bind_rows(setdiff(trend, stable_trend)) %>% 
    left_join(limits) %>% 
    mutate(
      cens = if_else(is.na(date_numeric), "right", "none"),
      data = coalesce(date_numeric, max)
    ) %>% 
    # add series that did not decrease:
    right_join(limits) %>% 
    select(all_of(g_var), data, cens)
  
  if(g_var != "study") {
    mutate(out, study = str_extract(.data[[g_var]], ".+?(?=\\.)"))
  } else out
  
}

id_change <- function(data, g_var, start, ...) {
  data %>% 
    filter(
      date_numeric >= start,
      ...
    ) %>% 
    distinct(.data[[g_var]])
}

#------------------ calculate stabilization time (total) ------------------

model_in <- make_model_input(slopes_combined, limits)

#------------------ calculate stabilization time (dissolved) ------------------

model_in_diss <- make_model_input(slopes_combined_diss, limits_diss, g_var = "study")

# determine a stabilization time for the initial P dose in study 4:
slopes_combined_diss_study4 <- slopes_combined_diss %>% 
  filter(study == "Study 4", date_numeric <= 0)

limits_diss_study4 <- slopes_combined_diss_study4 %>% 
  group_by(study) %>% 
  summarize(
    min = min(date_numeric), 
    max = max(date_numeric)
  ) %>% 
  ungroup()

model_in_diss_study4 <- make_model_input(
  slopes_combined_diss_study4, 
  limits_diss_study4,
  g_var = "study", 
  change = "increase", 
  start = limits_diss_study4$min
)
  
#------------------ tests ------------------

test_that("model_in is unchanged", {
  target <- read_csv(here::here("data/time-to-event-input.csv"))
  expect_equal(data.frame(target), data.frame(model_in))
})

test_that("model_in_diss is unchanged", {
  target <- read_csv(here::here("data/time-to-event-input-diss.csv"))
  expect_equal(data.frame(target), data.frame(model_in_diss))
})

test_that("model_in_diss_study4 is unchanged", {
  target <- read_csv(here::here("data/time-to-event-input-diss-study4.csv"))
  target$cens <- as.character(target$cens)
  target$data <- as.numeric(target$data)
  expect_equal(data.frame(target), data.frame(model_in_diss_study4))
})

#------------------ write model input ------------------

write_csv(model_in, here::here("data/time-to-event-input.csv"))
write_csv(model_in_diss, here::here("data/time-to-event-input-diss.csv"))
write_csv(model_in_diss_study4, here::here("data/time-to-event-input-diss-study4.csv"))
