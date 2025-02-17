bootstrap_estimation_nosub <- function(
    data = dta_time_varying,
    analysis = "ITT",
    if_boot = TRUE,
    x_adjusted_random_sample = x_adjusted_var,
    x_adjusted_random_sample_t0 = x_adjusted_var_t0,
    no_of_sample = 5,
    weights = "iptw_trunc",
    formula = "Surv(i-1, i, event) ~ index_exposure",
    max_month = 60,
    intervention = "GLP-1",
    control = "DPP-4",
    risk_intervention_name = "risk_GLP1",
    risk_control_name = "risk_DPP4",
    trunc_prop = 0.001,
    k = 1,
    ...) {
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
      dataset_random_sample <-
        dataset_with_iptw_generation(
          data = dataset_random_sample,
          x_adjusted_t0 = x_adjusted_random_sample_t0,
          trunc_prop = trunc_prop
        )

      # causal estimation
      risk_res <-
        causal_effect_estimation(
          data = dataset_random_sample,
          formula = "Surv(i-1, i, event) ~ index_exposure",
          max_month = 60,
          intervention = intervention,
          control = control,
          risk_intervention_name = risk_intervention_name,
          risk_control_name = risk_control_name,
          weights = weights
        )


      return(risk_res)
    },
    mc.cores = no_of_sample
  )

  return(CI_res_sub)
}
