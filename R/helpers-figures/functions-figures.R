
#------------------ functions ------------------

# highlight sections where derivative is non-zero:

highlight_smooth <- function(x, col = "#F21A00") {
  arg <- paste(".median", x, sep = "_")
  geom_line(
    data = gam_slopes %>%
      filter(series == "Study 5.b") %>%
      arrange(date_numeric) %>%
      mutate(
        "{arg}" := if_else(
          sign(.lower_slope) == sign(.upper_slope),
          .data[[arg]],
          NA_real_
        ),
        type = x
      ),
    aes(y = .data[[arg]]),
    col = col
  )
}

# demo GAM for fig 1:

fit_gam <- function(n, ...) {
  model_demo <- mgcv::gam(
    log(value) ~
      s(date_numeric, bs = "cr", k = n, ...),
    data = hdatdemo,
    method = "REML"
  )
  
  coefs <- coef(model_demo) %>%
    as_tibble(rownames = "coef") %>%
    rename(cval = value)
  
  predict(model_demo, type = "lpmatrix", newdata = newdat) %>%
    as_tibble() %>%
    bind_cols(newdat) %>%
    pivot_longer(starts_with("s("), values_to = "bfun", names_to = "coef") %>%
    left_join(coefs) %>%
    mutate(
      weighted = bfun * cval,
      intercept = as.numeric(coefs[1, 2]) * `(Intercept)`
    ) %>%
    pivot_longer(c(bfun, weighted), names_to = "facet", values_to = "basis")
}

# for supplementary figs:

plot_gams <- function(data_combined, model_in, this_study, ..., mcolor = "#5BBCD6", lcol = "black", 
                      plot_smooth = TRUE, log_t = TRUE, plot_stable_conc = FALSE, ljust = "right", 
                      gap_size = 5, g_var = "series", lwidth = .5) {
  # build dataset:
  
  these_facets <- if (plot_smooth) {
    c("data", "slope", "smooth")
  } else {
    c("data", "slope")
  }
  
  data <- data_combined %>%
    mutate(series = str_extract(series, "(?<=\\.)[a-z]$")) %>% 
    filter(
      study == this_study,
      type %in% these_facets,
      ...
    )
  
  if (log_t) {
    # log-transform fitted values for plotting:
    data <- data %>%
      mutate(
        across(starts_with("fit"), log10),
        value = log10(value)
      )
  }
  
  if(g_var == "series") model_in <- model_in %>% 
    mutate(series = str_extract(series, "(?<=\\.)[a-z]$")) %>% 
    filter(...)
  
  stable_here <- model_in %>%
    filter(
      study == this_study,
      cens == "none"
    )
  
  stable_line <- if (nrow(stable_here) > 0) {
    geom_vline(
      data = stable_here,
      aes(xintercept = data),
      col = "grey"
    )
  } else {
    NULL
  }
  
  annotate_stable <- if (nrow(stable_here) > 0) {
    geom_label(
      data = stable_here %>%
        mutate(
          label = "Stabilization time",
          type = "data"
        ),
      aes(x = data, y = Inf, label = label),
      vjust = "inward", hjust = ljust,
      size = 2.5, label.r = unit(0, "cm"), label.size = 0,
      alpha = .7, col = "grey35"
    )
  } else {
    NULL
  }
  
  # this is a temporary check on stable_conc:
  
  if(plot_stable_conc) {
    stable_conc <- stable_conc %>%
      mutate(
        series = str_extract(series, "(?<=\\.)[a-z]$"),
        type = "data"
      ) %>% 
      filter(study == this_study, ...)
  }
  
  check_stable_conc <- function(log_t, plot_stable_conc) {
    
    if (log_t) {
      stable_conc <- mutate(stable_conc, .epred_retrans = log10(.epred_retrans))
    }
    if (plot_stable_conc) {
      geom_point(
        data = stable_conc,
        aes(y = .epred_retrans)
      )
    } else NULL
    
  }
  
  # plot labels
  
  fstriplabs <- as_labeller(c(
    "data" = "[Pb] (µg L<sup>-1</sup>\\)",
    "slope" = "First derivative",
    "smooth" = "Long-term trend"
  ))
  
  # if nseries == 1, no need for column strip text:
  
  nseries <- data %>%
    distinct(series) %>%
    nrow()
  
  facet <- if (nseries > 1 & g_var != "study") {
    facet_grid(
      cols = vars(series),
      rows = vars(type = fct_relevel(type, "slope", after = Inf)),
      scales = "free",
      labeller = labeller(.rows = fstriplabs)
    )
  } else {
    facet_grid(
      rows = vars(type = fct_relevel(type, "slope", after = Inf)),
      scales = "free",
      labeller = labeller(.rows = fstriplabs)
    )
  }
  
  term_line <- if (plot_smooth) {
    geom_line(aes(y = term), col = mcolor)
  } else {
    NULL
  }
  
  term_ribbon <- if (plot_smooth) {
    geom_ribbon(aes(ymin = term_lower, ymax = term_upper), alpha = .3, fill = mcolor, col = NA)
  } else {
    NULL
  }
  
  adjust_scales <- if (log_t) {
    ggh4x::facetted_pos_scales(
      y = list(
        type == "data" ~ scale_y_continuous(
          breaks = log10(c(.1, 1, 10, 100, 1000, 10000)),
          labels = \(x) 10^x
        )
      )
    )
  } else {
    NULL
  }

  # plot model fit? not if there are multiple time series in one plot:
  
  model_ribbon <- if(g_var != "study") {
    geom_ribbon(aes(ymin = fit_lower, ymax = fit_upper, group = series), alpha = .3, fill = mcolor, col = NA)
  } else NULL
  
  model_line <- if(g_var != "study") {
    geom_line(aes(y = fit, group = series), col = mcolor)
  } else NULL
  
  # build plot:
  
  data %>%
    rowid_to_column() %>% 
    group_by(rowid) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(
      data = map(data, ~ add_rows(.x, size = gap_size))
    ) %>% 
    unnest(data) %>% 
    ggplot(aes(date_numeric)) +
    facet +
    # data:
    geom_path(
      data = \(x) x %>% 
        mutate(value = if_else(censored == "none", value, NA_real_)),
      aes(y = value, group = series), col = lcol, linewidth = lwidth
    ) +
    geom_segment(
      data = \(x) x %>%
        mutate(y = -Inf, type = "data") %>% 
        filter(censored == "left") %>%
        add_row(date_numeric = NA, value = NA, type = "data", series = unique(x$series)[1]),
      aes(
        x = date_numeric, 
        xend = date_numeric, 
        y = y,
        yend = value
      ),
      col = "grey", size = .4
    ) +
    # fit:
    model_ribbon + 
    model_line + 
    # derivatives:
    geom_hline(data = tibble(type = "slope", yi = 0), aes(yintercept = yi), linetype = 3) +
    geom_ribbon(aes(ymin = deriv_lower, ymax = deriv_upper), alpha = .3, fill = mcolor, col = NA) +
    geom_line(aes(y = deriv), col = mcolor) +
    # smooth:
    term_ribbon +
    term_line +
    stable_line +
    annotate_stable +
    check_stable_conc(log_t, plot_stable_conc) +
    adjust_scales +
    labs(
      x = "Time since P increase (days)",
      y = NULL
    ) +
    theme(
      strip.text.y = element_markdown(),
      axis.text.x = element_text(angle = 35, hjust = 1)
    )
}

add_rows <- function(x, size = 2, ...) {
  if (is.na(x$d_x)) {
    x
  } else {
    if (x$d_x > size & x$type == "data") {
      add_row(
        x, 
        type = x$type, 
        series = x$series, 
        .before = -Inf, 
        ...
      )
    } else {
      x
    }
  }
}

combine_data <- function(gam_preds, gam_slopes, g_var = NULL) {
  
  if(!is.null(g_var)) {
    if(g_var == "series") {
      gam_slopes <- mutate(gam_slopes, study = str_extract(series, ".+?(?=\\.)"))
    }
  }
  
  list(
    "data" = gam_preds %>%
      select(
        mtype = type, study, series, date_numeric, value, fit = .epred_retrans,
        fit_lower = .lower_retrans, fit_upper = .upper_retrans, d_x, censored
      ),
    "slope" = gam_slopes %>%
      select(
        mtype = type, study, all_of(g_var), date_numeric, deriv = .median_slope,
        deriv_lower = .lower_slope, deriv_upper = .upper_slope
      ),
    "smooth" = gam_slopes %>%
      select(
        mtype = type, study, all_of(g_var), date_numeric, term = .median_smooth,
        term_lower = .lower_smooth, term_upper = .upper_smooth
      )
  ) %>%
    bind_rows(.id = "type")
}

make_dx2 <- function(x) {
  x %>% 
    arrange(study, series, date_numeric) %>% 
    group_by(series) %>%
    mutate(
      d_x = date_numeric - dplyr::lag(date_numeric),
      d_x = d_x / 7, # a common time difference
      d_x = replace_na(d_x, 0) # NA is due to dplyr::lag()
    ) %>%
    ungroup()
}
