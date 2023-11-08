
#------------------ setup ------------------

source(here::here("R/02-fit-gam-non-seasonal.R"))
source(here::here("R/03-fit-gam-seasonal.R"))
library("tidyverse")
library("testthat")

#------------------ functions ------------------

make_preds <- function(data, models) {
  map2(
    .x = data,
    .y = models,
    .f = ~ add_pred_draws_car1(.x, .y) %>% 
      select(.epred) %>%
      summarize_preds(y_var = .x$value, retrans = TRUE)
  ) %>% 
    set_names(names(data)) %>% 
    bind_rows(.id = "type")
}

#------------------ generate predictions (total) ------------------

preds <- make_preds(
  data = list("seasonal" = data_seasonal, "nonseasonal" = data_nonseasonal),
  models = list(model_seasonal, model_nonseasonal)
)

#------------------ generate predictions (dissolved) ------------------

preds_diss <- make_preds(
  data = list("seasonal" = data_seasonal_diss, "nonseasonal" = data_nonseasonal_diss),
  models = list(model_seasonal_diss, model_nonseasonal_diss)
)

#------------------ tests ------------------

# test_that("preds is unchanged", {
#   target <- read_csv(here::here("data/gam-predictions.csv"))
#   expect_equal(data.frame(target), data.frame(preds))
# })
# 
# test_that("preds_diss is unchanged", {
#   target <- read_csv(here::here("data/gam-predictions-diss.csv"))
#   expect_equal(data.frame(target), data.frame(preds_diss))
# })

#------------------ write ------------------

write_csv(preds, "data/gam-predictions.csv")
write_csv(preds_diss, "data/gam-predictions-diss.csv")
  
