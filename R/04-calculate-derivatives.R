
#------------------ setup ------------------

source(here::here("R/02-fit-gam-non-seasonal.R"))
source(here::here("R/03-fit-gam-seasonal.R"))
library("purrr")
library("testthat")

#------------------ functions ------------------

calculate_derivs <- function(
    data, models, 
    smooth = "s(date_numeric, by = series)", 
    g_var = "series", 
    add_vars = FALSE
    ) {
  
  make_add_vars <- function(add_vars, data) {
    if(add_vars) {
      study_index <- seq_len(n_distinct(data$study))
      list(series = rep(unique(data$series)[study_index], each = 500))
    } else NULL
  }
  
  limits <- bind_rows(data) %>% 
    group_by(.data[[g_var]]) %>% 
    summarize(
      min = min(date_numeric), 
      max = max(date_numeric)
    ) %>% 
    ungroup()
  
  slopes <- map2(
    .x = data,
    .y = models,
    .f = ~ local_slope(
      .x, .y,                     
      x_var = "date_numeric", 
      smooth = smooth,
      g_var = g_var,
      pts = 500,
      add_vars = make_add_vars(add_vars, .x)
    )
  ) %>% 
    set_names(names(data)) %>% 
    bind_rows(.id = "type")
  
  slopes_sum <- slopes %>% 
    group_by(type, .data[[g_var]], date_numeric) %>% 
    summarize(
      .median_slope = median(slope),
      .lower_slope = quantile(slope, .025),
      .upper_slope = quantile(slope, .975),
      .median_smooth = median(smooth),
      .lower_smooth = quantile(smooth, .025),
      .upper_smooth = quantile(smooth, .975)
    ) %>% 
    ungroup() %>% 
    left_join(limits, by = g_var) %>% 
    filter(date_numeric >= min, date_numeric <= max) 
  
  list("slopes_sum" = slopes_sum, "limits" = limits)
  
}

#------------------ calculate derivatives (total) ------------------

derivs_tot <- calculate_derivs(
  data = list("seasonal" = data_seasonal, "nonseasonal" = data_nonseasonal),
  models = list(model_seasonal, model_nonseasonal)
)

#------------------ calculate derivatives (dissolved) ------------------

derivs_diss <- calculate_derivs(
  data = list("seasonal" = data_seasonal_diss, "nonseasonal" = data_nonseasonal_diss),
  models = list(model_seasonal_diss, model_nonseasonal_diss),
  smooth = "s(date_numeric, by = study)", 
  g_var = "study",
  add_vars = TRUE
)

#------------------ tests ------------------

test_that("slopes_sum is unchanged (total)", {
  target <- read_csv(here::here("data/gam-derivatives.csv"))
  new <- derivs_tot$slopes_sum
  expect_equal(data.frame(target), data.frame(new))
})

test_that("slopes_sum is unchanged (dissolved)", {
  target <- read_csv(here::here("data/gam-derivatives-diss.csv"))
  new <- derivs_diss$slopes_sum
  expect_equal(data.frame(target), data.frame(new))
})

#------------------ write ------------------

write_csv(derivs_tot$slopes_sum, here::here("data/gam-derivatives.csv"))
write_csv(derivs_diss$slopes_sum, here::here("data/gam-derivatives-diss.csv"))
write_csv(derivs_tot$limits, here::here("data/series-limits.csv"))
write_csv(derivs_diss$limits, here::here("data/series-limits-diss.csv"))
