
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
                      log_t = TRUE, ljust = "right", 
                      gap_size = 5, g_var = "series", lwidth = .5) {
  # build dataset:
  
  data <- data_combined %>%
    mutate(
      series = str_extract(series, "(?<=\\.)[a-z]$"),
      facets = if_else(type == "data", paste(type, series, sep = "_"), type) # for dissolved lead facets
    ) %>%
    filter(
      study == this_study,
      ...
    )
  
  # log-transform fitted values for plotting:
  
  if (log_t) {
    data <- data %>%
      mutate(
        across(starts_with("fit"), log10),
        value = log10(value)
      )
  }
  
  if (g_var == "series") {
    model_in <- model_in %>%
      mutate(series = str_extract(series, "(?<=\\.)[a-z]$")) %>%
      filter(...)
  }
  
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
  
  top_row <- data$facets[1]
  
  annotate_stable <- if (nrow(stable_here) > 0) {
    geom_label(
      data = stable_here %>%
        mutate(
          facets = top_row,
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
  
  # plot labels
  
  fstriplabs_names <- data %>%
    filter(type == "data") %>%
    pull(facets) %>%
    unique()
  
  series_labs <- rep("[Pb] (µg L<sup>-1</sup>\\)", length(fstriplabs_names))
  
  series_labs <- paste(series_labs, str_extract(fstriplabs_names, "(?<=_)[a-z]$"), sep = " - ")
  
  names(series_labs) <- fstriplabs_names
  
  fstriplabs <- as_labeller(c(
    series_labs,
    "data" = "[Pb] (µg L<sup>-1</sup>\\)",
    "slope" = "First derivative",
    "smooth" = "Long-term trend"
  ))
  
  nseries <- data %>%
    distinct(series) %>%
    nrow()
  
  facet <- if (g_var == "series") {
    facet_grid(
      cols = vars(series),
      rows = vars(type = fct_relevel(type, "slope", after = Inf)),
      scales = "free",
      labeller = labeller(.rows = fstriplabs)
    )
  } else if (g_var == "study") {
    facet_grid(
      rows = vars(facets = fct_relevel(facets, "slope", after = Inf)),
      scales = "free",
      labeller = labeller(.rows = fstriplabs)
    )
  } else {
    stop("g_var must be one of 'study' or 'series'.") # study is dissolved Pb model, series is total Pb model
  }
  
  adjust_scales <- if (log_t) {
    
    if (g_var == "study") {
      ggh4x::facetted_pos_scales(
        y = list(
          str_detect(facets, "data") ~ scale_y_continuous(
            breaks = log10(c(.1, 1, 10, 100, 1000, 10000)),
            labels = \(x) 10^x
          )
        )
      )
    } else 
      if (g_var == "series") {
        ggh4x::facetted_pos_scales(
          y = list(
            type == "data" ~ scale_y_continuous(
              breaks = log10(c(.1, 1, 10, 100, 1000, 10000)),
              labels = \(x) 10^x
            )
          )
        )
      }
    
  } else {
    NULL
  }
  
  angled_labs <- if (g_var == "series") {
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
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
        filter(censored == "left", type == "data") %>%
        add_row(
          date_numeric = NA, value = NA,
          type = unique(x$type)[1],
          facets = unique(x$facets)[1],
          series = unique(x$series)[1]
        ) %>%
        mutate(y = -Inf),
      aes(
        x = date_numeric,
        xend = date_numeric,
        y = y,
        yend = value
      ),
      col = "grey", linewidth = .4
    ) +
    # fit:
    # geom_ribbon(aes(ymin = fit_lower, ymax = fit_upper, group = series), alpha = .3, fill = mcolor, col = NA) +
    # geom_polygon() and geom_path() ensure that long gaps in the time series don't get predictions:
    geom_polygon(
      data = \(x) x %>% 
        filter(type == "data") %>% 
        # sections represent gaps in the time series:
        mutate(section = cumsum(is.na(date_numeric))) %>% 
        group_by(facets, section) %>% 
        nest() %>% 
        mutate(
          poly = map(
            data, 
            ~ with(.x, tibble(
              type = unique(type)[1], 
              series = unique(series)[1], 
              date_numeric = c(date_numeric, rev(date_numeric)), 
              ypoly = c(fit_lower, rev(fit_upper))
            ))
          )
        ) %>% 
        unnest(poly),
      aes(x = date_numeric, y = ypoly), 
      alpha = .3, fill = mcolor
    ) +
    geom_path(aes(y = fit, group = series), col = mcolor) +
    # derivatives:
    geom_hline(
      data = \(x) x %>% 
        filter(type == "slope") %>% 
        mutate(yi = 0), 
      aes(yintercept = yi), 
      linetype = 3
    ) +
    geom_ribbon(aes(ymin = deriv_lower, ymax = deriv_upper), alpha = .3, fill = mcolor, col = NA) +
    geom_line(aes(y = deriv), col = mcolor) +
    # smooth:
    geom_ribbon(aes(ymin = term_lower, ymax = term_upper), alpha = .3, fill = mcolor, col = NA) +
    geom_line(aes(y = term), col = mcolor) +
    stable_line +
    annotate_stable +
    adjust_scales +
    labs(
      x = "Time since P increase (days)",
      y = NULL
    ) +
    theme(strip.text.y = element_markdown()) + 
    angled_labs
}

add_rows <- function(x, size = 2, ...) {
  if (is.na(x$d_x)) {
    x
  } else {
    if (x$d_x > size & x$type == "data") {
      add_row(
        x,
        type = x$type,
        facets = x$facets,
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
