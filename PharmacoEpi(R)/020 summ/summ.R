summ <- function(data = dta_time_varying, x = "ipw_trunc") {
  var <- data[[x]]
  var_sum <- data.frame(
    var = x,
    mean = round(mean(var), 4),
    sd = round(sd(var), 4),
    median = round(quantile(var, 0.5), 4),
    percentile_25 = round(quantile(var, 0.25), 4),
    percentile_75 = round(quantile(var, 0.75), 4),
    percentile_5 = round(quantile(var, 0.05), 4),
    percentile_95 = round(quantile(var, 0.95), 4),
    minimum = round(min(var), 4),
    maxmium = round(max(var), 4)
  )
  var_sum$`Mean (SD)` <- paste0(var_sum$mean, " (", var_sum$sd, ")")
  var_sum$`Median (IQR)` <- paste0(var_sum$median, " (", var_sum$percentile_25, ", ", var_sum$percentile_75, ")")
  var_sum <- var_sum[, c("var", "Mean (SD)", "Median (IQR)", "percentile_5", "percentile_95", "minimum", "maxmium")]
  rownames(var_sum) <- NULL
  return(var_sum)
}
