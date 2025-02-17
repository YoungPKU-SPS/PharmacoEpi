# Imputation ---> Boot ---> Causal estimation ---> Subgroup estimation
results_process_with_subgroup <- function(
    ## original data
    data = dta,
    ## parameters for imputation
    dta_imps = dta_imps,
    if_imps = FALSE, # whether to conduct imputation process
    outome = "outcome_HHF1",
    vars = vars, # all variables in the final datasets,
    imputed_size = 1000, # Cut the data into n subsets with 1000 patients
    imputed_vars = c("cov_albuminuria_cat", "cov_hba1c", "cov_UACR", "cov_eGFR"),
    imputed_methods = "cart",
    imputed_not_include_vars = c("lopnr", "i", "index_date", "tstop", "exposure"),
    imputed_cores_2_use = 450,
    imputed_m = 5,
    ## parameters for data tidy
    xvars_t0 = xvars_t0,
    outcome_filter = "outcome_HHF1",
    ## parameters for analysis
    analysis_m = 1,
    analysis = "ITT",
    if_boot = TRUE,
    x_adjusted_var = x_adjusted_var,
    x_adjusted_var_t0 = x_adjusted_var_t0,
    ### parameters for analysis boot
    boot_k = 2,
    boot_no_of_sample = 2,
    ### parameters for analysis ipw
    weights = "iptw_trunc",
    trunc_prop = 0.001,
    ### parameters for CIF
    formula = "Surv(i-1, i, event) ~ index_exposure",
    max_month = 60,
    time = 1:60,
    intervention = "GLP1",
    control = "DPP4",
    risk_intervention_name = "risk_GLP1",
    risk_control_name = "risk_DPP4",
    subgroup_formula = "Surv(i-1, i, event) ~ index_exposure",
    prespecified_subgroup = prespecified_subgroup,
    prespecified_subgroup_name = prespecified_subgroup_name) {
  if (if_imps) {
    # imputation process
    message("imputation process")
    dta_imps <- imputation(
      data = data,
      outcome = outcome,
      vars = vars, # all variables in the final datasets,
      size = imputed_size, # Cut the data into n subsets with 1000 patients
      imputed_vars = imputed_vars,
      imputed_methods = imputed_methods,
      not_include_vars = imputed_not_include_vars,
      cores_2_use = imputed_cores_2_use,
      m = imputed_m
    )
    message("imputation process save file")
    imps_filename <- paste0("dta_imps_", intervention, control, "_", outcome, "_2010to2021.R")
    save(dta_imps, file = imps_filename)
  }

  results <- lapply(1:analysis_m, function(imputation_no) {
    # data tidy
    message("data tidy process for ", imputation_no)
    data_tidy <- data_tidy_process(
      dta_imps = dta_imps[[imputation_no]],
      dta = dta,
      xvars_t0 = xvars_t0,
      outcome_filter = outcome,
      outcome_analysis = outcome_filter
    )
    data_tidy[[1]]$null_weight <- 1

    if (analysis == "ITT") {
      data_tidy_analysis <- data_tidy[[2]]
    } else {
      data_tidy_analysis <- data_tidy[[1]]
    }
    # bootstrap process
    message("bootstrap process")

    result_m <- foreach(
      i = 1:boot_k,
      .combine = c
    ) %dopar% {
      result_k <- bootstrap_estimation(
        data = data_tidy_analysis,
        analysis = analysis,
        if_boot = if_boot,
        x_adjusted_random_sample = x_adjusted_var,
        x_adjusted_random_sample_t0 = x_adjusted_var_t0,
        k = i,
        no_of_sample = boot_no_of_sample,
        weights = weights,
        trunc_prop = trunc_prop,
        formula = formula,
        max_month = max_month,
        time = time,
        intervention = intervention,
        control = control,
        risk_intervention_name = risk_intervention_name,
        risk_control_name = risk_control_name,
        subgroup_formula = subgroup_formula,
        prespecified_subgroup = prespecified_subgroup,
        prespecified_subgroup_name = prespecified_subgroup_name
      )
      return(result_k)
    }
    return(result_m)
  })

  ## file save
  if (if_boot) {
    message("save the result")
    results_filename <- paste0(paste(analysis, intervention, control, outcome, "res_main", sep = "_"), ".R")
    save(results, file = results_filename)
  }
  return(results)
}
