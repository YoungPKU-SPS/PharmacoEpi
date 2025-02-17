dataset_with_ipcw_generation <- function(
    data = dta_time_varying,
    x_adjusted = x_adjusted_var,
    x_adjusted_t0 = x_adjusted_var_t0,
    trunc_prop = 0.001) {
  ## IPTW
  dta_baseline <- data %>% filter(i == 1)
  numerator.fit <- nrow(dta_baseline[dta_baseline$exposure == 1, ]) / nrow(dta_baseline)
  regression_formula_IPTW_de <-
    as.formula(paste0("exposure ~ ",
      paste(x_adjusted_t0, collapse = " + "),
      sep = " "
    ))
  denominator.fit <- glm(regression_formula_IPTW_de,
    data = dta_baseline,
    family = binomial(link = "logit")
  )
  dta_baseline$ps <- fitted(denominator.fit)
  dta_baseline$iptw <- ifelse(dta_baseline$exposure == 1,
    numerator.fit / fitted(denominator.fit),
    (1 - numerator.fit) / (1 - fitted(denominator.fit))
  )
  data <- data[, c(colnames(data)[!colnames(data) %in% c("iptw", "ps")])] %>%
    left_join(dta_baseline[, c("lopnr", "iptw", "ps")], by = "lopnr")

  ## IPCW
  regression_formula_IPCW_de <-
    as.formula(paste0("censoring_medication_modification ~ exposure + i + i^2 + ",
      paste(x_adjusted, collapse = " + "),
      sep = " "
    ))
  regression_formula_IPCW_nu <-
    as.formula(paste0("censoring_medication_modification ~ exposure + i + i^2 + ",
      paste(x_adjusted_t0, collapse = " + "),
      sep = " "
    ))
  denominator.fit <- glm(regression_formula_IPCW_de,
    data = data,
    family = binomial(link = "logit")
  )
  numerator.fit <- glm(regression_formula_IPCW_nu,
    data = data,
    family = binomial(link = "logit")
  )
  data$ipcw_per_month <-
    ifelse(data$censoring_medication_modification == 1,
      0, (1 - fitted(numerator.fit)) / (1 - fitted(denominator.fit))
    )
  data$ipcw <- unlist(tapply(data$ipcw_per_month, data$lopnr, cumprod))

  ## ipw = iptw * ipcw
  data$ipw <- data$iptw * data$ipcw

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
