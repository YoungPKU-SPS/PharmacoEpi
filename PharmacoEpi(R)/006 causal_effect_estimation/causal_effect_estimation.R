causal_effect_estimation <- function(
    data = dta_time_varying,
    formula = "Surv(i-1, i, event) ~ index_exposure",
    max_month = 60,
    time = 1:120,
    intervention = "GLP-1",
    control = "DPP-4",
    risk_intervention_name = "risk_GLP1",
    risk_control_name = "risk_DPP4",
    weights = "iptw_trunc",
    if_boot = FALSE) {
  data$ipw <- data[[weights]]
  event.adjusted <-
    coxph(
      as.formula(formula),
      id = lopnr,
      weights = ipw,
      data = data
    )
  coef <- summary(event.adjusted)$coef

  ## non-parametric
  fitted <- survfit(as.formula(formula),
    id = lopnr,
    weights = ipw,
    data = data,
    conf.type = "plain"
  )

  if (if_boot == FALSE) {
    if (max(summary(fitted)$time) >= max_month) {
      unique_time <- sort(unique(c(0, summary(fitted)$time, max_month)))
      intervention_time <- unique(c(0, summary(fitted)$time[summary(fitted)$strata == "index_exposure=1"], max_month))
      control_time <- unique(c(0, summary(fitted)$time[summary(fitted)$strata == "index_exposure=0"], max_month))
      time <- unique_time
    } else if (max(summary(fitted)$time) < max_month) {
      unique_time <- sort(unique(c(0, summary(fitted)$time)))
      intervention_time <- unique(c(0, summary(fitted)$time[summary(fitted)$strata == "index_exposure=1"]))
      control_time <- unique(c(0, summary(fitted)$time[summary(fitted)$strata == "index_exposure=0"]))
      time <- unique_time
    }
  }


  time_length <- length(time)

  pstate <-
    as.data.frame(
      matrix(summary(fitted, times = time)$pstate,
        ncol = 3,
        byrow = FALSE,
        dimnames = list(
          summary(fitted, times = time)$time,
          c(summary(fitted, times = time)$states)
        )
      )
    ) %>%
    cbind(data.frame(
      time = summary(fitted, times = time)$time,
      index_exposure = summary(fitted, times = time)$strata
    )) %>%
    mutate(
      index_exposure = ifelse(
        index_exposure == "index_exposure=0", 0, 1
      )
    ) %>%
    dplyr::select("time", "index_exposure", "outcome") %>%
    arrange(index_exposure, time) %>%
    filter(time <= max_month) %>%
    left_join(data.frame(time = c(time, time)),
      by = join_by("time" <= "time"),
      suffix = c("_origin", "")
    ) %>%
    mutate(time_delta = time - time_origin) %>%
    group_by(time) %>%
    filter(time_delta == min(time_delta)) %>%
    ungroup() %>%
    distinct() %>%
    dplyr::select("time", "index_exposure", "outcome")

  rownames(pstate) <- NULL

  ## Intervention
  event_risk_summary_intervention <-
    pstate %>%
    mutate(
      i = time,
      p_cumevent = outcome
    ) %>%
    filter(index_exposure == 1) %>%
    mutate(X = intervention)

  ## Control
  event_risk_summary_control <-
    pstate %>%
    mutate(
      i = time,
      p_cumevent = outcome
    ) %>%
    filter(index_exposure == 0) %>%
    mutate(X = control)


  event_risk_summary <-
    rbind(
      event_risk_summary_intervention,
      event_risk_summary_control
    )


  risk_intervention <- event_risk_summary_intervention %>%
    filter(i <= max_month) %>%
    dplyr::select(p_cumevent)
  risk_control <- event_risk_summary_control %>%
    filter(i <= max_month) %>%
    dplyr::select(p_cumevent)
  RD <- (risk_intervention - risk_control)

  risk_res <-
    round(
      cbind(
        time[time <= max_month],
        risk_intervention * 100,
        risk_control * 100,
        RD * 100,
        c(exp(summary(event.adjusted)$coef[1, 1]), rep(NA, (nrow(risk_intervention) - 1)))
      ),
      2
    )


  ## parametric model
  # fitted <- summary(survfit(event.adjusted,
  #                           data.frame(index_exposure = c(0, 1)),
  #                           conf.type = "plain"), time = time)
  # pstate <-
  #     as.data.frame(
  #         matrix(fitted$pstate,
  #              ncol = 3,
  #              byrow = FALSE,
  #              dimnames = list(c(time, time), c(fitted$states)))
  #     ) %>%
  #     mutate(
  #         time = c(time, time),
  #         index_exposure = c(rep(0, (max_month+1)), rep(1, (max_month+1)))
  #     ) %>%
  #     dplyr::select('time', 'index_exposure', 'outcome')
  # rownames(pstate) <- NULL

  # ## Intervention
  # event_risk_summary_intervention <-
  #     data.frame(
  #         cbind(exposure = rep(1, length(time)),
  #               i = time,
  #               p_cumevent = pstate$outcome[pstate$index_exposure == 1])
  #     ) %>%
  #     mutate(X = intervention)

  # ## Control
  # event_risk_summary_control <-
  #     data.frame(
  #         cbind(exposure = rep(0, length(time)),
  #               i = time,
  #               p_cumevent = pstate$outcome[pstate$index_exposure == 0])
  #     ) %>%
  #     mutate(X = control)

  # event_risk_summary <-
  #     rbind(event_risk_summary_intervention,
  #           event_risk_summary_control)

  # risk_intervention <- event_risk_summary_intervention %>%
  #     filter(i %in% seq(0, max_month, 1)) %>%
  #     dplyr::select(p_cumevent)
  # risk_control <- event_risk_summary_control %>%
  #     filter(i %in% seq(0, max_month, 1)) %>%
  #     dplyr::select(p_cumevent)
  # RD <- (risk_intervention - risk_control)

  # risk_res <-
  #     round(cbind(seq(0, max_month, 1),
  #                 risk_intervention * 100,
  #                 risk_control * 100,
  #                 RD * 100,
  #                 c(exp(summary(event.adjusted)$coef[1,1]), rep(NA, max_month))
  #                ),
  #           2)

  colnames(risk_res) <-
    c("Month", risk_intervention_name, risk_control_name, "RD", "HR")


  if (if_boot == FALSE) {
    time_ponits <- list(
      unique_time = unique_time,
      intervention_time = intervention_time,
      control_time = control_time
    )
    risk_res <- list(risk_res, event_risk_summary, coef, time_ponits)
  } else {
    risk_res <- list(risk_res, event_risk_summary, coef)
  }
  return(risk_res)
}
