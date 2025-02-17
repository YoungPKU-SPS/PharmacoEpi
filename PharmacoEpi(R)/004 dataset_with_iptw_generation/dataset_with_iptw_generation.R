dataset_with_iptw_generation <- function(data = dta_time_varying,
                                         x_adjusted_t0 = x_adjusted_var_t0,
                                         trunc_prop = 0.001) {
  ## numerator of IPTW
  dta_baseline <- data %>% filter(i == 1)
  numerator.fit <- nrow(dta_baseline[dta_baseline$index_exposure == 1, ]) / nrow(dta_baseline)
  regression_formula_IPTW_de <- as.formula(paste0("index_exposure ~ ",
    paste(x_adjusted_t0, collapse = " + "),
    sep = " "
  ))
  ## denominator of IPTW (Propensity score)
  ### logit regression and fit the ps
  denominator.fit <- glm(regression_formula_IPTW_de,
    data = dta_baseline,
    family = binomial(link = "logit")
  )
  dta_baseline$ps <- fitted(denominator.fit)

  ## IPTW
  dta_baseline$iptw <- ifelse(dta_baseline$index_exposure == 1,
    numerator.fit / fitted(denominator.fit),
    (1 - numerator.fit) / (1 - fitted(denominator.fit))
  )

  data <- data[, c(colnames(data)[!colnames(data) %in% c("iptw", "ps")])] %>%
    left_join(dta_baseline[, c("lopnr", "iptw", "ps")], by = "lopnr")

  ## ipw_trunc at 0.001 and 0.999
  trunc.l <- quantile(dta_baseline$iptw, trunc_prop)
  trunc.u <- quantile(dta_baseline$iptw, (1 - trunc_prop))

  data$iptw_trunc <-
    ipw_truncation(
      data$iptw,
      trunc.l = trunc.l,
      trunc.u = trunc.u
    )
  data$ipw <- data$iptw
  data$ipw_trunc <- data$iptw_trunc
  return(data)
}
