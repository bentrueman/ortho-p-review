
#------------------ setup ------------------

source(here::here("R/02-fit-gam-non-seasonal.R"))
source(here::here("R/03-fit-gam-seasonal.R"))
source(here::here("R/07-fit-time-to-event.R"))
library("ggplot2")
library("patchwork")
library("tidybayes")
library("bayesplot")
library("ggdist")
library("pbcusol")
library("wesanderson")
library("ggrepel")
library("glue", include.only = "glue")
library("ggh4x", include.only = "facetted_pos_scales")
library("ggtext", include.only = "element_markdown")
library("devtools", include.only = "session_info")
library("withr", include.only = "with_seed")
source(here::here("R/helpers-figures/functions-figures.R"))

theme_set(
  theme_bw() +
    theme(
      legend.position = "bottom",
      plot.tag = element_text(face = "bold")
    )
)

pal_name <- "Darjeeling1"

palette <- wes_palette(pal_name)

gam_preds <- read_csv(here::here("data/gam-predictions.csv"))
gam_slopes <- read_csv(here::here("data/gam-derivatives.csv"))
phree_preds <- read_csv(here::here("data/preds-pbcusol.csv"))

# dissolved:

gam_preds_diss <- read_csv(here::here("data/gam-predictions-diss.csv"))
gam_slopes_diss <- read_csv(here::here("data/gam-derivatives-diss.csv"))
model_in_diss <- read_csv(here::here("data/time-to-event-input-diss.csv"))

#------------------ predictions ------------------

preds <- list(
  "Posterior" = fit,
  "Prior" = fit_prior,
  "Posterior (flat priors)" = fit_noprior
) %>%
  map_dfr(
    ~ as_draws_df(.x, "b_Intercept") %>% 
      transmute(.prediction = exp(b_Intercept)),
    .id = "type"
  )

data_combined <- combine_data(gam_preds, gam_slopes, g_var = "series")
data_combined_diss <- combine_data(gam_preds_diss, gam_slopes_diss)
seasonal <- conditional_smooths(model_seasonal)[[2]]
seasonal_diss <- conditional_smooths(model_seasonal_diss)[[3]]

#------------------ data for Rmd ------------------

source("R/helpers-figures/tables-rmd.R")

#------------------ figure 1 ------------------

source("R/helpers-figures/figure-1.R")

#------------------ figure 2 ------------------

source("R/helpers-figures/figure-2.R")

#------------------ figures (supplementary) ------------------

source("R/helpers-figures/figures-supplementary.R") # this takes a few minutes

#------------------ session info ------------------

writeLines(capture.output(devtools::session_info()), "session-info.txt")
