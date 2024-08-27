
# run from "R/10-make-figures.R"

#------------------ figure s1 ------------------

striplabs <- as_labeller(c("bfun" = "Unweighted basis functions", "weighted" = "Weighted basis functions"))

hdatdemo <- data_combined %>%
  filter(series == "Study 3.i")

newdat <- hdatdemo %>%
  with(tibble(
    date_numeric = seq(min(date_numeric), max(date_numeric), by = 7)
  ))

out <- bind_rows(
  "n = 10" = fit_gam(10),
  "n = 40" = fit_gam(40),
  "n = 40\n(unpenalized)" = fit_gam(40, fx = TRUE),
  .id = "k"
)

colors <- out %>% 
  distinct(k, coef) %>% 
  group_by(k) %>% 
  mutate(basis_no = seq(1, 39, length.out = n_distinct(coef))) %>% 
  ungroup()

p0 <- out %>%
  left_join(colors, by = c("k", "coef")) %>% 
  ggplot(aes(date_numeric)) +
  facet_grid(cols = vars(facet), rows = vars(k), labeller = labeller(.cols = striplabs)) +
  geom_line(aes(y = basis, col = basis_no, group = basis_no), show.legend = FALSE) +
  geom_line(
    data = function(x) {
      x %>%
        filter(facet == "weighted") %>%
        group_by(date_numeric, facet, k) %>%
        summarize(value = sum(basis))
    },
    aes(y = value)
  ) +
  geom_point(
    data = hdatdemo %>%
      mutate(
        value = log(value) - unique(round(out$intercept, 6)),
        facet = "weighted"
      ),
    aes(y = value),
    alpha = .5, shape = 16
  ) +
  scale_x_continuous(breaks = 365 * c(-2:1), labels = \(x) x / 365) +
  scale_color_gradientn(colors = palette) +
  labs(
    x = "Time since P increase (yr)",
    y = expression("log([Pb] (µg L"^-1 * "))")
  )

striplabs2 <- as_labeller(c(
  "data" = "log<sub>10</sub>([Pb] (µg L<sup>-1</sup>\\)\\)",
  "slope" = "First derivative",
  "smooth" = "Long-term trend"
))

stable_here <- model_in %>%
  filter(series == "Study 5.b")

p1 <- data_combined %>%
  filter(series == "Study 5.b") %>%
  mutate(across(c(value, starts_with("fit")), log10)) %>%
  ggplot(aes(date_numeric)) +
  facet_wrap(
    vars(type = fct_relevel(type, "slope", after = Inf)),
    ncol = 1, scales = "free_y", labeller = striplabs2
  ) +
  # data:
  geom_line(aes(y = value)) +
  # model"
  geom_ribbon(aes(ymin = fit_lower, ymax = fit_upper), fill = "#00BFC4", alpha = .3, col = NA) +
  geom_line(aes(y = fit), col = palette[1]) +
  # multi-year trend:
  geom_ribbon(aes(ymin = term_lower, ymax = term_upper), alpha = .3) +
  geom_line(aes(y = term)) +
  highlight_smooth("smooth", col = palette[5]) +
  # derivatives:
  geom_ribbon(aes(ymin = deriv_lower, ymax = deriv_upper), alpha = .3) +
  geom_line(aes(y = deriv)) +
  highlight_smooth("slope", col = palette[5]) +
  geom_label(
    data = stable_here %>%
      mutate(
        label = "Stabilization time",
        type = "data"
      ),
    aes(x = data, y = Inf, label = label),
    vjust = "inward", hjust = "right",
    size = 2.5, label.r = unit(0, "cm"), label.size = 0,
    alpha = .7, col = "grey35"
  ) +
  geom_vline(
    data = stable_here,
    aes(xintercept = data),
    col = "grey"
  ) +
  scale_x_continuous(breaks = 365 * c(-1:3), labels = \(x) x / 365) +
  labs(
    x = "Time since P increase (yr)",
    y = NULL
  ) +
  theme(strip.text = element_markdown())

p2 <- seasonal %>%
  filter(study == "Study 5") %>%
  ggplot(aes(date_yday * 365)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = .3) +
  geom_line(aes(y = estimate__)) +
  labs(
    x = "Ordinal day",
    y = "Partial effect"
  )

p3 <- preds %>%
  filter(type == "Prior") %>%
  ggplot(aes(.prediction / 365)) +
  stat_halfeye() +
  scale_x_log10(breaks = c(1e-2, 1, 1e2), labels = c(0.01, 1, 100)) +
  scale_y_continuous(expand = expansion(add = c(.1, 0))) +
  labs(
    x = "Stabilization time (yr)",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(1e-3, 1e3))

fig_s1 <- wrap_plots(p0, p1, p2, p3, design = "AA\nAA\nAA\nAA\nBC\nBD") +
  plot_annotation(tag_levels = "a")

ggsave("figures/figure-s1.png", fig_s1, dev = "png", dpi = 600, width = 6.5, height = 6.75)

#------------------ figure s2 ------------------

fig_s2 <- list(
  "Total" = seasonal,
  "Filtrate" = seasonal_diss
) %>% 
  bind_rows(.id = "type") %>% 
  ggplot(aes(365 * date_yday)) +
  facet_grid(cols = vars(study), rows = vars(type), scales = "free_y") +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = .3) +
  geom_line(aes(y = estimate__)) +
  labs(
    x = "Ordinal day",
    y = "Partial effect"
  )

ggsave("figures/figure-s2.png", fig_s2, dev = "png", dpi = 600, width = 6.5, height = 3.5)

#------------------ figure s3 ------------------

fig_s3 <- plot_gams(data_combined, model_in, "Study 1", log_t = FALSE)
ggsave("figures/figure-s3.png", fig_s3, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s4 ------------------

fig_s4 <- plot_gams(data_combined, model_in, "Study 2")
ggsave("figures/figure-s4.png", fig_s4, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s5 ------------------

fig_s5 <- plot_gams(data_combined, model_in, "Study 3", series %in% letters[1:4], gap_size = 10)
ggsave("figures/figure-s5.png", fig_s5, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s6 ------------------

fig_s6 <- plot_gams(data_combined, model_in, "Study 3", series %in% letters[5:8], gap_size = 10)
ggsave("figures/figure-s6.png", fig_s6, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s7 ------------------

fig_s7 <- plot_gams(data_combined, model_in, "Study 3", series %in% letters[9:12], gap_size = 10)
ggsave("figures/figure-s7.png", fig_s7, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s8 ------------------

fig_s8 <- plot_gams(data_combined, model_in, "Study 4", series %in% letters[1:4], log_t = TRUE)
ggsave("figures/figure-s8.png", fig_s8, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s9 ------------------

fig_s9 <- plot_gams(data_combined, model_in, "Study 4", series %in% letters[5:8], log_t = TRUE)
ggsave("figures/figure-s9.png", fig_s9, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s10 ------------------

fig_s10 <- plot_gams(data_combined, model_in, "Study 5", series %in% letters[1:3])
ggsave("figures/figure-s10.png", fig_s10, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s11 ------------------

fig_s11 <- plot_gams(data_combined, model_in, "Study 5", series %in% letters[4:6])
ggsave("figures/figure-s11.png", fig_s11, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s12 ------------------

fig_s12 <- plot_gams(data_combined, model_in, "Study 5", series %in% letters[7:9])
ggsave("figures/figure-s12.png", fig_s12, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s13 ------------------

fig_s13 <- plot_gams(data_combined, model_in, "Study 6")
ggsave("figures/figure-s13.png", fig_s13, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s14 ------------------

fig_s14 <- plot_gams(data_combined, model_in, "Study 7", series %in% letters[1:3])
ggsave("figures/figure-s14.png", fig_s14, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s15 ------------------

fig_s15 <- plot_gams(data_combined, model_in, "Study 7", series %in% letters[4:6])
ggsave("figures/figure-s15.png", fig_s15, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s16 ------------------

fig_s16 <- plot_gams(data_combined, model_in, "Study 10", series %in% letters[1:3])
ggsave("figures/figure-s16.png", fig_s16, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s17 ------------------

fig_s17 <- plot_gams(data_combined, model_in, "Study 10", series %in% letters[4:6])
ggsave("figures/figure-s17.png", fig_s17, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s18 ------------------

fig_s18 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 1", 
  g_var = "study", log_t = FALSE
)
ggsave("figures/figure-s18.png", fig_s18, dev = "png", dpi = 600, width = 6.5, height = 6)

#------------------ figure s19 ------------------

fig_s19 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 2", 
  g_var = "study", log_t = FALSE
)
ggsave("figures/figure-s19.png", fig_s19, dev = "png", dpi = 600, width = 6.5, height = 6)

#------------------ figure s20 ------------------

fig_s20 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 4", 
  g_var = "study", log_t = TRUE
)
ggsave("figures/figure-s20.png", fig_s20, dev = "png", dpi = 600, width = 6.5, height = 7.5)

#------------------ figure s21 ------------------

fig_s21 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 5", 
  series %in% letters[1:3] | is.na(series), 
  g_var = "study"
)
ggsave("figures/figure-s21.png", fig_s21, dev = "png", dpi = 600, width = 6.5, height = 6)

#------------------ figure s22 ------------------

fig_s22 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 5", 
  series %in% letters[4:6] | is.na(series), g_var = "study", 
  log_t = FALSE
)
ggsave("figures/figure-s22.png", fig_s22, dev = "png", dpi = 600, width = 6.5, height = 6)

#------------------ figure s23 ------------------

fig_s23 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 6", 
  g_var = "study", log_t = FALSE
)
ggsave("figures/figure-s23.png", fig_s23, dev = "png", dpi = 600, width = 6.5, height = 7.5)

#------------------ figure s24 ------------------

fig_s24 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 7", 
  g_var = "study", log_t = TRUE
)
ggsave("figures/figure-s24.png", fig_s24, dev = "png", dpi = 600, width = 6.5, height = 6)

#------------------ figure s25 ------------------

fig_s25 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 8", 
  g_var = "study", log_t = FALSE
)
ggsave("figures/figure-s25.png", fig_s25, dev = "png", dpi = 600, width = 6.5, height = 6)

#------------------ figure s26 ------------------

fig_s26 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 9", 
  g_var = "study", log_t = FALSE
)
ggsave("figures/figure-s26.png", fig_s26, dev = "png", dpi = 600, width = 6.5, height = 4.5)

#------------------ figure s27 ------------------

fig_s27 <- plot_gams(
  data_combined_diss, model_in_diss, "Study 10", 
  g_var = "study", log_t = TRUE, gap_size = 10
) + 
  geom_point(
    data = \(x) x %>% 
      filter(date_numeric > 400, series %in% c("b", "d", "e")),
    aes(y = value),
    alpha = .7,
    shape = 16
  )

ggsave("figures/figure-s27.png", fig_s27, dev = "png", dpi = 600, width = 6.5, height = 9)

#------------------ figure s28 ------------------

ppc <- tibble(
  name = c("Non-seasonal", "Seasonal", "Non-seasonal (filtrate)", "Seasonal (filtrate)"),
  data = list(data_nonseasonal, data_seasonal, data_nonseasonal_diss, data_seasonal_diss),
  model = list(model_nonseasonal, model_seasonal, model_nonseasonal_diss, model_seasonal_diss),
) %>% 
  mutate(ppc = map2(data, model, ~ ppc_km_nada(.x, .y, seed = 1259)))

fig_s28 <- ppc %>%
  select(name, ppc) %>% 
  unnest(ppc) %>% 
  ggplot(aes(obs, prob, col = type, group = .draw)) +
  facet_wrap(vars(name)) +
  geom_line() +
  scale_color_manual(values = c(palette[1], "grey70")) +
  coord_cartesian(xlim = c(-8, 8)) +
  labs(x = "Observation", y = "Probability", col = NULL)

ggsave("figures/figure-s28.png", fig_s28, dev = "png", dpi = 600, width = 6.5, height = 4)

#------------------ figure s29 ------------------

fig_s29 <- phree_preds %>%
  make_dx2() %>%  
  pivot_longer(cols = c(ph, dic, p_ppm, value)) %>%  
  mutate(type = "data", facets = NA) %>% 
  rowid_to_column() %>% 
  group_by(rowid) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    data = map(
      data,
      ~ add_rows(.x, size = 10, date_numeric = .x$date_numeric, name = .x$name, study = .x$study)
    )
  ) %>% 
  unnest(data) %>%
  mutate(
    study = str_replace(study, "\\s", "\n"),
    series = str_remove(series, "Study \\d+\\."),
    name = fct_relevel(name, "value", after = Inf),
    name = fct_recode(
      name, 
      "DIC (mg C L<sup>-1</sup>)" = "dic",
      "Orthophosphate<br>(mg P L<sup>-1</sup>)" = "p_ppm",
      "Filtrate lead<br>(µg L<sup>-1</sup>)" = "value",
      "pH" = "ph"
    )
  ) %>% 
  ggplot(aes(date_numeric, value, col = series)) + 
  ggh4x::facet_grid2(
    cols = vars(name), 
    rows = vars(study = fct_relevel(study, "Study\n10", after = Inf)),
    scales = "free",
    independent = "all"
  ) + 
  scale_color_manual(values = wes_palette(pal_name, 9, "continuous")) +
  geom_line(linewidth = .2) + 
  geom_point(
    data = \(x) filter(x, study == "Study\n4", date_numeric > 0),
    size = .3
  ) +
  theme(
    strip.text = element_markdown(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  ) +
  labs(
    x = "Time since P increase (days)",
    y = NULL,
    col = "Pipe"
  )

ggsave(here::here("figures/figure-s29.png"), fig_s29, dev = "png", dpi = 600, width = 6.5, height = 10)
