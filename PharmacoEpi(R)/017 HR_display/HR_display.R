## HR display function
HR.display <- function(unadjusted, adjusted, group_name) {
  res <- rbind(t(unadjusted$conf.int[c(1, 3, 4)]), t(adjusted$conf.int[1, c(1, 3, 4)]))
  res <- round(res, 3)
  res <- data.frame(res)
  res <- res %>% mutate(HR = paste0(res[, 1], " (", res[, 2], "-", res[, 3], ")"))
  res <- t(c(res$HR[1], res$HR[2]))
  colnames(res) <- c("crude HR", "adjusted HR")
  res <- res %>%
    data.frame() %>%
    mutate(group = group_name)
  res <- res[, c(3, 1, 2)]
  return(res)
}
