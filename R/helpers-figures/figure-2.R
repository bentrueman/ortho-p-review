
# run from "R/10-make-figures.R"

#------------------ figure 2 ------------------

fig_2_in <- phree_preds %>%
  mutate(
    d = value - prediction,
    database = str_remove(database, "pb_ppb_"),
    database = fct_recode(database, "LEADSOL (1996 values)" = "leadsol", "minteq (v4)" = "minteqv4")
  ) %>% 
  filter(database != "minteq") %>%
  group_by(database) %>% 
  mutate(
    mae = signif(median(abs(d)), 2),
    mae = glue("MAE~'='~{mae}~µg~L^-1")
  ) %>% 
  ungroup() %>% 
  group_by(database, study) %>% 
  summarize(
    xmin = min(value),
    xmax = max(value),
    ymin = min(prediction),
    ymax = max(prediction),
    value = median(value),
    prediction = median(prediction),
    mae = unique(mae)
  )

fig_2 <- fig_2_in %>% 
  ggplot(aes(value, prediction)) + 
  facet_wrap(vars(database), ncol = 1) +
  geom_abline(linetype = 3) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax, col = study), 
    width = 0
  ) +
  geom_errorbarh(
    aes(xmin = xmin, xmax = xmax, col = study), 
    height = 0
  ) + 
  geom_label_repel(
    data = \(x) mutate(x, labs = str_extract(study, "\\d+")),
    aes(label = labs, col = study),
    size = 2.5,
    label.size = 0,
    label.padding = unit(0.35, "lines"),
    alpha = .6,
    seed = 1245,
    nudge_x = if_else(fig_2_in$study == "Study 5", -.05, 0)
  ) +
  geom_point(aes(col = study), shape = 16, alpha = .75, size = 2) + 
  geom_label(
    data = \(x) x %>%
      group_by(database) %>%
      summarize(
        mae = unique(mae)
      ),
    aes(x = 0, y = Inf, label = mae),
    hjust = "inward", vjust = "inward",
    label.size = 0,
    label.padding = unit(0.35, "lines"),
    alpha = .8, parse = TRUE, size = 3
  ) +
  geom_label(
    data = tibble(database = "LEADSOL (1996 values)", x = 5, y = 0, label = "y=x"),
    aes(x = x, y = y, label = label),
    label.size = 0,
    label.padding = unit(0.35, "lines"),
    alpha = .8,
    vjust = "inward"
  ) +
  scale_color_manual(values = wes_palette("Zissou1", type = "continuous", 9)) +
  scale_x_log10() +
  scale_y_log10() +
  # guides(col = guide_legend(nrow = 2, override.aes = list(size = 3))) +
  guides(col = "none") +
  labs(
    x = expression("Observed (µg Pb L"^-1*")"),
    y = expression("Predicted (µg Pb L"^-1*")"),
    col = NULL
  )

ggsave(here::here("figures/figure-2.png"), fig_2, dev = "png", dpi = 600, width = 3.33, height = 4.5)

# alternate:

# fig_2 <- phree_preds %>% 
#   mutate(
#     d = value - prediction,
#     database = str_remove(database, "pb_ppb_"),
#     database = fct_recode(database, "LEADSOL (1996 values)" = "leadsol", "minteq (v4)" = "minteqv4")
#   ) %>% 
#   filter(database != "minteq") %>% 
#   ggplot(aes(value, prediction)) + 
#   facet_wrap(vars(database), ncol = 1) +
#   geom_abline(linetype = 3) +
#   geom_point(aes(col = study), shape = 16, alpha = .75, size = 1.5) + 
#   geom_label(
#     data = \(x) x %>% 
#       group_by(database) %>% 
#       summarize(
#         x = Inf,
#         y = Inf,
#         mae = signif(median(abs(d)), 2),
#         mae = glue("MAE~'='~{mae}~µg~L^-1")
#       ),
#     aes(x = x, y = y, label = mae),
#     hjust = "inward", vjust = "inward",
#     label.size = 0,
#     label.padding = unit(0.35, "lines"),
#     alpha = .8, parse = TRUE
#   ) +
#   geom_label(
#     data = tibble(database = "LEADSOL (1996 values)", x = 5, y = 10, label = "y=x"),
#     aes(x = x, y = y, label = label),
#     label.size = 0,
#     label.padding = unit(0.35, "lines"),
#     alpha = .8
#   ) +
#   scale_color_manual(values = palette) +
#   scale_x_log10() +
#   scale_y_log10() +
#   guides(col = guide_legend(nrow = 2, override.aes = list(size = 3))) +
#   labs(
#     x = expression("Observed (µg Pb L"^-1*")"),
#     y = expression("Predicted (µg Pb L"^-1*")"),
#     col = NULL
#   )
