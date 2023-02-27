
# run from "R/10-make-figures.R"

#------------------ figure 1 ------------------

seed_jitter <- 8215

p0 <- model_in %>%
  mutate(study = str_extract(study, "\\d+")) %>% 
  filter(!is.na(data)) %>% 
  ggplot(aes(data, reorder(study, as.numeric(study)), col = study)) +
  geom_point(
    data = . %>%
      mutate(shp = if_else(cens == "right", NA_character_, "none")),
    aes(shape = shp),
    size = 3, 
    alpha = .5,
    position = position_jitter(height = .2, width = 0, seed = seed_jitter)
  ) +
  geom_linerange(
    data = . %>%
      filter(cens == "right"),
    aes(xmin = data, xmax = Inf),
    linewidth = .2,
    position = position_jitter(height = .2, width = 0, seed = seed_jitter)
  ) +
  guides(shape = "none", col = "none") +
  scale_shape_manual(values = c(16, 0)) +
  scale_x_continuous(breaks = 365 * 0:3, labels = \(x) signif(x / 365, 1)) +
  scale_color_manual(values = wes_palette(pal_name, 8, "continuous")) +
  coord_cartesian(xlim = 365 * c(0, 4)) +
  theme(
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    # panel.grid = element_blank()
  ) +
  labs(x = "Stabilization time (yr)", y = "Study", col = NULL)

p1 <- as_draws_df(fit, "r_study|b_Intercept", regex = TRUE) %>%
  as_tibble() %>%
  pivot_longer(starts_with("r")) %>% 
  mutate(
    estimate = b_Intercept + value,
    study = str_extract(name, "\\d+")
  ) %>%
  ggplot(aes(exp(estimate), reorder(study, as.numeric(study)), fill = study, col = study)) +
  stat_halfeye(slab_alpha = .5, show.legend = FALSE) +
  scale_x_continuous(breaks = 365 * c(0:5), labels = \(x) x / 365) +
  scale_y_discrete(labels = \(breaks) str_extract(breaks, "\\d+")) +
  scale_color_manual(values = wes_palette(pal_name, 8, "continuous")) +
  scale_fill_manual(values = wes_palette(pal_name, 8, "continuous")) +
  coord_cartesian(xlim = c(0, 5 * 365)) +
  theme(
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  labs(
    x = "Mean stabilization\ntime (yr)",
    y = "Study"
  )

p2 <- preds %>%
  filter(type != "Prior") %>%
  mutate(type = str_replace(type, "\\s(?=\\()", "\n")) %>%
  ggplot(aes(x = .prediction, col = type, fill = type)) +
  ggdist::stat_halfeye(
    slab_alpha = .4, 
    position = position_dodge(width = .2),
    point_interval = "median_qi"
  ) +
  scale_x_continuous(breaks = 365 * c(0:10), labels = \(x) x / 365) +
  scale_y_continuous(expand = expansion(add = c(-0.15, 0.25))) +
  scale_color_manual(values = palette[c(1,5)]) +
  scale_fill_manual(values = palette[c(1,5)]) +
  labs(
    x = "Predicted stabilization\ntime (yr)",
    y = "Density",
    col = NULL, fill = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "right"
  ) +
  coord_cartesian(xlim = c(0, 8 * 365))

observed <- survfit(Surv(data, event) ~ 1, data = mutate(model_in, event = cens == "none")) %>% 
  with(tibble(time, surv))

draws <- posterior_predict(fit, draw_ids = withr::with_seed(2236, sample(1:4000, 50))) %>% 
  apply(
    1,
    \(x) with(
      survfit(Surv(data, event) ~ 1, data = tibble(data = x, event  = TRUE)),
      tibble(time, surv)
    )
  ) %>% 
  bind_rows(.id = "id")

p3 <- draws %>% 
  ggplot(aes(time, surv)) + 
  geom_step(aes(group = id, col = "y<sub>rep</sub>"), linewidth = .2) + 
  geom_step(data = observed, aes(col = "y")) +
  scale_x_continuous(breaks = 365 * seq(0, 10, by = 2.5), labels = \(x) x / 365) +
  scale_color_manual(values = c("black", "grey")) +
  labs(
    x = "Time since P increase (yr)",
    y = "Fraction\nstabilizing",
    col = NULL
  ) +
  theme(
    legend.position = "right",
    legend.text.align = 0,
    legend.text = element_markdown()
  ) +
  coord_cartesian(xlim = c(0, 10 * 365))

pcomb <- wrap_plots(p0, p1) +
  plot_layout(guides = "collect")

fig_1 <- wrap_plots(p2, p3, pcomb, design = "AB\nCC", heights = c(1, 1.5)) +
  plot_annotation(tag_levels = "a")

ggsave("figures/figure-1.png", fig_1, dev = "png", dpi = 600, width = 6.5, height = 4.5)
