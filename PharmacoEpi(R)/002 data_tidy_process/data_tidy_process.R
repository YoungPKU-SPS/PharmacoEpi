data_tidy_process <- function(
    dta_imps = dta_imps,
    dta = dta,
    xvars_t0 = xvars_t0,
    outcome_filter = "outcome",
    outcome_analysis = "outcome") {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  dta_imps_final <-
    dta_imps %>%
    left_join(
      dta[, c(
        "lopnr", "index_date", "tstart", "tstop", "exposure", "index_exposure",
        xvars_t0[!(xvars_t0 %in% colnames(dta_imps))]
      )],
      by = c("lopnr", "index_date", "tstart", "tstop", "exposure")
    )

  dta_imps_final <-
    dta_imps_final %>%
    group_by(lopnr) %>%
    arrange(lopnr, tstart) %>%
    mutate(
      cov_eGFR_t0 = first(cov_eGFR, order_by = tstart),
      cov_eGFR_LM_t0 = first(cov_eGFR_LM, order_by = tstart),
      cov_eGFR_EKFC_t0 = first(cov_eGFR_EKFC, order_by = tstart),
      cov_hba1c_t0 = first(cov_hba1c, order_by = tstart),
      cov_UACR_t0 = first(cov_UACR, order_by = tstart),
      cov_HHF_subgroup_t0 = first(cov_HHF_subgroup, order_by = tstart),
      cov_calendar_year_t0 = first(calendar_year, order_by = tstart)
    ) %>%
    ungroup()

  dta_imps_final <-
    dta_imps_final %>%
    arrange(lopnr, tstart) %>%
    mutate(
      exposure = ifelse(exposure == "GLP-1", 1, 0),
      index_exposure = ifelse(index_exposure == "GLP-1", 1, 0),
      age_2 = age * age,
      age_cat = case_when(
        age < 50 ~ "<50",
        age >= 50 & age < 60 ~ "50-59",
        age >= 60 & age < 70 ~ "60-69",
        age >= 70 & age < 80 ~ "70-79",
        age >= 80 ~ ">=80"
      ),
      age_cat = factor(age_cat, levels = c("<50", "50-59", "60-69", "70-79", ">=80")),
      cov_number_of_medications_cat_t0 = case_when(
        cov_number_of_medications_t0 <= 5 ~ "0-5",
        cov_number_of_medications_t0 > 5 & cov_number_of_medications_t0 <= 10 ~ "6-10",
        cov_number_of_medications_t0 > 10 & cov_number_of_medications_t0 <= 15 ~ "11-15",
        cov_number_of_medications_t0 > 10 ~ ">15"
      ),
      cov_number_of_medications_cat_t0 = factor(cov_number_of_medications_cat_t0,
        levels = c("0-5", "6-10", "11-15", ">15")
      ),
      cov_number_of_medications_cat = case_when(
        cov_number_of_medications <= 5 ~ "0-5",
        cov_number_of_medications > 5 & cov_number_of_medications <= 10 ~ "6-10",
        cov_number_of_medications > 10 & cov_number_of_medications <= 15 ~ "11-15",
        cov_number_of_medications > 10 ~ ">15"
      ),
      cov_number_of_medications_cat = factor(cov_number_of_medications_cat,
        levels = c("0-5", "6-10", "11-15", ">15")
      ),
      cov_eGFR_cat_t0 = case_when(
        cov_eGFR_t0 >= 90 ~ "G1",
        cov_eGFR_t0 >= 60 & cov_eGFR_t0 < 90 ~ "G2",
        cov_eGFR_t0 >= 45 & cov_eGFR_t0 < 60 ~ "G3a",
        cov_eGFR_t0 >= 30 & cov_eGFR_t0 < 45 ~ "G3b",
        cov_eGFR_t0 < 30 ~ "G4-5"
      ),
      cov_eGFR_cat_t0 = factor(cov_eGFR_cat_t0, levels = c("G1", "G2", "G3a", "G3b", "G4-5")),
      cov_eGFR_cat = case_when(
        cov_eGFR >= 90 ~ "G1",
        cov_eGFR >= 60 & cov_eGFR < 90 ~ "G2",
        cov_eGFR >= 45 & cov_eGFR < 60 ~ "G3a",
        cov_eGFR >= 30 & cov_eGFR < 45 ~ "G3b",
        cov_eGFR < 30 ~ "G4-5"
      ),
      cov_eGFR_cat = factor(cov_eGFR_cat, levels = c("G1", "G2", "G3a", "G3b", "G4-5")),
      cov_hba1c_perc_t0 = cov_hba1c_t0 / 10.929 + 2.15,
      cov_hba1c_perc = cov_hba1c / 10.929 + 2.15,
      cov_albuminuria_cat = case_when(
        cov_UACR < 30 ~ "A1",
        cov_UACR >= 30 & cov_UACR < 300 ~ "A2",
        cov_UACR >= 300 ~ "A3"
      ),
      cov_albuminuria_cat = factor(cov_albuminuria_cat, levels = c("A1", "A2", "A3")),
      cov_albuminuria_cat_t0 = case_when(
        cov_UACR_t0 < 30 ~ "A1",
        cov_UACR_t0 >= 30 & cov_UACR_t0 < 300 ~ "A2",
        cov_UACR_t0 >= 300 ~ "A3"
      ),
      cov_albuminuria_cat_t0 = factor(cov_albuminuria_cat_t0, levels = c("A1", "A2", "A3")),
      cov_diuretics_t0 = ifelse((cov_loop_diuretic_t0 == 0 &
        cov_thiazide_t0 == 0 &
        cov_potassium_sparing_diuretics_t0 == 0),
      0, 1
      ),
      cov_diuretics = ifelse((cov_loop_diuretic == 0 &
        cov_thiazide == 0 &
        cov_potassium_sparing_diuretics == 0),
      0, 1
      ),
      cov_ASCVD_t0 = ifelse(cov_acute_coronary_syndrome_t0 == 0 &
        cov_other_ischemic_heart_disease_t0 == 0 &
        cov_stroke_t0 == 0 &
        cov_other_cerebrovascular_disease_t0 == 0 &
        cov_valve_diseases_t0 == 0 &
        cov_peripheral_vascular_disease_t0 == 0, 0, 1)
    )

  # outcome variable
  dta_imps_final$outcome_filter <- dta_imps_final[[outcome_filter]]
  dta_imps_final$outcome <- dta_imps_final[[outcome_analysis]]

  dta_imps_final <-
    dta_imps_final %>%
    arrange(lopnr, tstart) %>%
    mutate(
      # death as the competing risk (Priority: Outcome, Death, Censor)
      outcome_all_cause_death = ifelse(
        outcome_filter == 1, 0, outcome_all_cause_death
      ),
      event = ifelse(outcome == 1, 1,
        ifelse(outcome_all_cause_death == 1, 2, 0)
      ),
      event = factor(event, 0:2, labels = c("censor", "outcome", "death"))
    ) %>%
    group_by(lopnr) %>%
    mutate(
      death_filter = cumsum(outcome_all_cause_death),
      outcome_filter = cumsum(outcome_filter),
      death_filter = cumsum(death_filter),
      outcome_filter = cumsum(outcome_filter)
    ) %>%
    filter(death_filter <= 1 &
      outcome_filter <= 1) %>%
    ungroup()

  # Subgroup variables
  dta_time_varying <- data.frame(dta_imps_final)
  ## (≥70 vs. <70 years)
  dta_time_varying$agesub <- ifelse(dta_time_varying$age >= 70, 1, 0)
  ## (≥60 vs. <60 ml/min/1.73m2)
  dta_time_varying$eGFRsub <- ifelse(dta_time_varying$cov_eGFR_t0 >= 60, 1, 0)

  ## Calendar year
  dta_time_varying$cov_calendar_year_cat <-
    ifelse(dta_time_varying$cov_calendar_year_t0 %in% c(
      "2010", "2011", "2012", "2013",
      "2014", "2015", "2016", "2017"
    ), 0, 1)

  # Baseline data
  dta_baseline <-
    dta_time_varying %>%
    group_by(lopnr) %>%
    mutate(end_date = max(tstop)) %>%
    filter(end_date == tstop) %>%
    ungroup()

  dta_baseline$exposure <- dta_baseline$index_exposure
  dta_baseline$tstart <- dta_baseline$index_date
  dta_baseline$tstop <- dta_baseline$end_date
  dta_baseline$outcome <- (dta_baseline$event == "outcome")

  return(list(dta_time_varying, dta_baseline))
}
