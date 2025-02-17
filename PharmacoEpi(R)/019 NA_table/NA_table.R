NA_table <- function(data = dta) {
  p_count <- sapply(data, function(y) length(unique(data[is.na(y), ]$lopnr)))
  p_count <- p_count[p_count > 0]
  na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
  na_count <- na_count[na_count > 0]
  na_count <- data.frame(na_count)
  na_count$covariate <- rownames(na_count)
  na_count <- cbind(p_count, na_count) %>%
    mutate(na_percentage = round(na_count / nrow(data), 4) * 100) %>%
    dplyr::select(covariate, p_count, na_count, na_percentage)
  return(na_count)
}
