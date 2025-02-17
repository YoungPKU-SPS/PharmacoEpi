subgroup_causal_effect_estimation <- function(
    data = dta_time_varying,
    analysis = "ITT",
    subgroup_formula = "Surv(i - 1, i, event) ~ index_exposure",
    prespecified_subgroup = prespecified_subgroup,
    prespecified_subgroup_name = prespecified_subgroup_name,
    x_adjusted_var = x_adjusted_var,
    x_adjusted_var_t0 = x_adjusted_var_t0,
    weights = "ipw_trunc",
    trunc_prop = 0.001) {
  subgroup_results <- lapply(
    seq_along(prespecified_subgroup),
    function(x) {
      subgroup <- prespecified_subgroup[x]
      subgroup_name <- prespecified_subgroup_name[x]
      message(subgroup_name)

      data$subgroup <- as.factor(data[, subgroup])
      subgroup_levels <- levels(data$subgroup)
      message(subgroup_levels)

      # incidence and HR
      message("incidence and HR")
      res_sub <- foreach(
        k = subgroup_levels,
        .combine = rbind
      ) %dopar% {
        dta_sub <- data[data$subgroup == k, ]
        dta_sub <- dta_sub[, colnames(dta_sub)[!colnames(dta_sub) %in%
          c("ps", "iptw", "ipcw", "iptw_trunc", "ipw", "ipw_trunc")]]
        # incidence rate
        dta_sub$null_weight <- 1
        incidence_table <- incid_num_weighted_subgroup(
          data = dta_sub,
          weights = "null_weight",
          event_outcome = "outcome",
          digit = 1
        )

        # HR
        ## IPW
        if (analysis == "ITT") {
          dta_sub <- dataset_with_iptw_generation(
            data = dta_sub,
            x_adjusted_t0 = x_adjusted_var_t0[!x_adjusted_var_t0 %in% subgroup],
            trunc_prop = trunc_prop
          )
          dta_sub$ipw <- dta_sub[[weights]]
          regression_formula <- as.formula(subgroup_formula)
          outcome.adjusted_sub <-
            coxph(regression_formula,
              id = lopnr,
              data = dta_sub,
              weight = ipw
            )

          HR_sub <- t(summary(outcome.adjusted_sub)$coef[1, 1])
          HR_sub <- data.frame(HR_sub)
          colnames(HR_sub) <- c("HR")
          return(cbind(incidence_table, HR_sub))
        }
        if (analysis == "PP") {
          dta_sub <- dataset_with_ipw_generation(
            data = dta_sub,
            x_adjusted_var = x_adjusted_var,
            x_adjusted_t0 = x_adjusted_var_t0[!x_adjusted_var_t0 %in% subgroup],
            trunc_prop = trunc_prop
          )
          dta_sub$ipw <- dta_sub[[weights]]
          dta_sub$outcome <- (dta_sub$event == "outcome")
          regression_formula <- as.formula(subgroup_formula)
          outcome.adjusted_sub <-
            parglm(regression_formula,
              weights = dta_time_varying$ipw_trunc,
              data = dta_time_varying,
              family = binomial(link = "logit"),
              control = parglm.control(nthreads = 6)
            )
          HR_sub <- t(summary(outcome.adjusted_sub$coef)[1, 1])
          HR_sub <- data.frame(HR_sub)
          colnames(HR_sub) <- c("HR")
          return(cbind(incidence_table, HR_sub))
        }
      }
      message(length(res_sub))
      res_sub <- rbind(
        c(subgroup_name, rep(NA, (length(res_sub) - 1))),
        res_sub
      )
      return(res_sub)
    }
  )
  return(subgroup_results)
}
