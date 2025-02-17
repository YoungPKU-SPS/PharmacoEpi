incid_num_weighted <- function(data = dta_time_varying,
                               weights = "ipw_trunc",
                               exposure = "all",
                               event_outcome = "hhf",
                               digit = 1) {
  data$weights <- data[[weights]]

  if (exposure == "all") {
    data$exposure <- "all"
  }

  dta <- data %>%
    arrange(lopnr, index_date) %>%
    group_by(lopnr) %>%
    summarise(
      tstop = as.Date("1970-01-01") + sum(weights * difftime(tstop, tstart)),
      tstart = as.Date("1970-01-01"),
      outcome = sum((event == event_outcome) * weights),
      exposure = first(exposure)
    ) %>%
    ungroup() %>%
    group_by(exposure) %>%
    summarise(
      n = length(lopnr),
      median.followup = round(median(time_length(difftime(tstop, tstart), "month")), digit),
      q25.followup = round(quantile(time_length(difftime(tstop, tstart), "month"), 0.25), digit),
      q75.followup = round(quantile(time_length(difftime(tstop, tstart), "month"), 0.75), digit),
      weighted_followup = paste0(median.followup, " [", q25.followup, "-", q75.followup, "]"),
      weighted_event = round(sum(outcome), 0),
      weighted_person_years = round(sum(time_length(difftime(tstop, tstart), "year")), 0),
      weighted_rate_point = round(weighted_event / weighted_person_years * 1000, digit),
      ## Daly LE. Confidence Limits Made Easy: Interval Estimation Using a Substitution Method. American journal of epidemiology. 1998;147(8):783-90. !!!alarm, weights were not considered in this version
      weighted_rate_lci = round(exactPoiCI(weighted_event)[1] / weighted_person_years * 1000, digit),
      weighted_rate_uci = round(exactPoiCI(weighted_event)[2] / weighted_person_years * 1000, digit),
      weighted_rate = paste0(weighted_rate_point, " [", weighted_rate_lci, "-", weighted_rate_uci, "]")
    ) %>%
    dplyr::select(c(
      "exposure", "n",
      "weighted_followup", "weighted_event",
      "weighted_person_years", "weighted_rate"
    ))
  return(dta)
}
