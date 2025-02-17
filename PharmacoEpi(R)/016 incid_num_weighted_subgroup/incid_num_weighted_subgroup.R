incid_num_weighted_subgroup <- function(data = dta_baseline,
                                        weights = "ipw_trunc",
                                        event_outcome = "outcome",
                                        digit = 1) {
  data$weights <- data[[weights]]
  incidence_table <- data %>%
    arrange(lopnr, index_date) %>%
    group_by(index_exposure, lopnr) %>%
    summarise(
      person_years = sum(as.numeric(difftime(tstop, tstart)) / 365.25 * weights),
      outcome = sum((event == event_outcome) * weights),
    ) %>%
    ungroup() %>%
    group_by(index_exposure) %>%
    summarise(
      n = length(lopnr),
      weighted_event = round(sum(outcome), 0),
      weighted_person_years = round(sum(person_years), 0),
      weighted_rate_point = round(weighted_event / weighted_person_years * 1000, digit)
    ) %>%
    dplyr::select(c("index_exposure", "n", "weighted_event", "weighted_rate_point"))

  incidence_table_0 <- incidence_table[incidence_table$index_exposure == 0, ]
  colnames(incidence_table_0) <- paste0(colnames(incidence_table_0), "_0")
  incidence_table_1 <- incidence_table[incidence_table$index_exposure == 1, ]
  colnames(incidence_table_1) <- paste0(colnames(incidence_table_1), "_1")
  incidence_table <- cbind(
    incidence_table_0,
    incidence_table_1
  )
  incidence_table$N <- incidence_table$n_0 + incidence_table$n_1
  incidence_table <- incidence_table %>%
    dplyr::select(c(
      "N",
      "n_0", "weighted_event_0", "weighted_rate_point_0",
      "n_1", "weighted_event_1", "weighted_rate_point_1"
    ))
  return(incidence_table)
}
