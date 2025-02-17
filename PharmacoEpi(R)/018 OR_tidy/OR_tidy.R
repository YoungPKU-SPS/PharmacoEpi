OR.result.tidy <- function(result) {
  res <- result$coefficients[2, c(1, 2)]
  OR <- exp(res[1])
  lci <- exp(res[1] - qnorm(0.975) * res[2])
  uci <- exp(res[1] + qnorm(0.975) * res[2])
  res <- data.frame(OR, lci, uci)
  colnames(res) <- c("OR", "lci", "uci")
  res <- res %>%
    data.frame() %>%
    mutate_at(vars(OR, lci, uci), funs(round(., 3)))
  res <- res %>%
    mutate(OR = paste0(res[, 1], " (", res[, 2], "-", res[, 3], ")")) %>%
    cbind(exposure = c("Intervention")) %>%
    dplyr::select(c(4, 1))
  res <- rbind(data.frame(exposure = "Control", OR = "Ref"), res)
  res$exposure <- as.character(res$exposure)
  return(res)
}
