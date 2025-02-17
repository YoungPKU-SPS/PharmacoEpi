bootstrap_estimation_with_subgroup <- function(
    data = dta_time_varying,
    analysis = "ITT",
    if_boot = TRUE,
    x_adjusted_random_sample = x_adjusted_var,
    x_adjusted_random_sample_t0 = x_adjusted_var_t0,
    k = 1,
    no_of_sample = 5,
    weights = "iptw_trunc",
    trunc_prop = 0.001,
    formula = "Surv(i-1, i, event) ~ index_exposure",
    max_month = 60,
    intervention = "GLP-1",
    control = "DPP-4",
    risk_intervention_name = "risk_GLP1",
    risk_control_name = "risk_DPP4",
    prespecified_subgroup = prespecified_subgroup,
    prespecified_subgroup_name = prespecified_subgroup_name,
    subgroup_formula = "Surv(i - 1, i, event) ~ index_exposure") {
  CI_res_sub <- mclapply(
    (1 + (k - 1) * no_of_sample):(k * no_of_sample),
    function(j) {
      # random sample
      if (if_boot) {
        dataset_random_sample <-
          bootstrap_sample_generation(
            seed = j,
            data = data
          )
      } else {
        dataset_random_sample <- data
      }

      # ipw sample
      if (analysis == "ITT") {
        dataset_random_sample <-
          dataset_with_iptw_generation(
            data = dataset_random_sample,
            x_adjusted_t0 = x_adjusted_random_sample_t0,
            trunc_prop = trunc_prop
          )
      }
      if (analysis == "PP") {
        dataset_random_sample <-
          dataset_with_ipw_generation(
            data = dataset_random_sample,
            x_adjusted = x_adjusted_var,
            x_adjusted_t0 = x_adjusted_random_sample_t0,
            trunc_prop = trunc_prop
          )
      }


      # causal estimation
      risk_res <-
        causal_effect_estimation(
          data = dataset_random_sample,
          formula = formula,
          max_month = max_month,
          intervention = intervention,
          control = control,
          risk_intervention_name = risk_intervention_name,
          risk_control_name = risk_control_name,
          weights = weights
        )

      # causal subgroup estimation
      subgroup_results <-
        subgroup_causal_effect_estimation(
          data = dataset_random_sample,
          analysis = analysis,
          subgroup_formula = subgroup_formula,
          prespecified_subgroup = prespecified_subgroup,
          prespecified_subgroup_name = prespecified_subgroup_name,
          x_adjusted_var = x_adjusted_random_sample,
          x_adjusted_var_t0 = x_adjusted_random_sample_t0,
          weights = weights,
          trunc_prop = trunc_prop
        )

      return(list(
        risk_res = risk_res,
        subgroup_results = subgroup_results
      ))
    },
    mc.cores = no_of_sample
  )

  # risk_res <-
  #     foreach (i = 1 : no_of_sample,
  #              .combine = c) %do% {
  #         CI_res_sub[[i]][[1]]
  #     }

  # subgroup_results <-
  #     foreach (i = 1 : no_of_sample,
  #              .combine = c) %do% {
  #         CI_res_sub[[i]][[2]]
  #     }

  # return(list(risk_res = risk_res,
  #             subgroup_results = subgroup_results))
  return(CI_res_sub)
}
