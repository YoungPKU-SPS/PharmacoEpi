bootstrap_sample_generation <- function(
    seed = 1, ## see of random
    data = dta_time_varying) {
  set.seed(seed)
  lopnr_random_sample <- sample(unique(data$lopnr),
    size = length(unique(data$lopnr)),
    replace = TRUE
  )
  dataset_random_sample <-
    data.frame(
      id = 1:length(lopnr_random_sample),
      lopnr = lopnr_random_sample
    ) %>%
    inner_join(
      data,
      by = "lopnr",
      multiple = "all",
      unmatched = "drop"
    ) %>%
    mutate(
      lopnr = id
    ) %>%
    dplyr::select(., -id)
  return(dataset_random_sample)
}
