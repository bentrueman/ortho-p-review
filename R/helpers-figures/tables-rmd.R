
#------------------ data for Rmd ------------------

preds_rmd <- preds %>%
  ungroup() %>%
  group_by(type) %>%
  median_qi(.prediction)

stable_conc <- gam_preds %>%
  rename(model = type) %>%
  left_join(model_in, by = "series") %>%
  mutate(d = abs(replace_na(data, 1e6) - date_numeric)) %>%
  group_by(model, study = study.x, series) %>%
  slice_min(d) %>%
  summarize(
    date_numeric = unique(data),
    cens_val = unique(censored),
    cens_stable = unique(cens),
    .epred_retrans = median(.epred_retrans)
  ) %>%
  ungroup()

write_csv(preds_rmd, here::here("data/time-to-event-results.csv"))
write_csv(stable_conc, here::here("data/stable-conc.csv"))
