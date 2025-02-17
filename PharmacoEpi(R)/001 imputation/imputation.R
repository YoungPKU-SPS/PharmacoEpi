imputation <- function(
    data = data,
    outcome = "outcome_HHF1",
    vars = vars, # all variables in the final datasets,
    size = 1000, # Cut the data into n subsets with 1000 patients
    imputed_vars = c("cov_albuminuria_cat", "cov_hba1c", "cov_UACR", "cov_eGFR"),
    imputed_methods = rep("cart", 5),
    imputed_not_include_vars = c("lopnr", "i", "index_date", "tstop", "exposure"),
    cores_2_use = 450,
    m = 5) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  if (!requireNamespace("mice", quietly = TRUE)) {
    install.packages("mice")
  }
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    install.packages("doParallel")
  }
  if (!requireNamespace("parallell", quietly = TRUE)) {
    install.packages("parallell")
  }

  data$outcome <- data[[outcome]]
  data$nelsonaalen <- mice::elsonaalen(data, i, outcome)
  data <- data %>% dplyr::select(vars)
  ## Cut the data into n subsets with 1000 patients
  set.seed(123)
  vector_divide <- function(vector, size = size) {
    sample <- list()
    i <- 1
    while (length(vector) >= size) {
      sample[[i]] <- sample(x = vector, size = size, FALSE)
      vector <- setdiff(vector, sample[[i]])
      sample[[i + 1]] <- vector
      i <- i + 1
    }
    return(sample)
  }
  sample <- vector_divide(
    vector = unique(data$lopnr),
    size = size
  )

  datalist <- list()
  for (i in seq_along(sample)) {
    datalist[[i]] <- data[which(data$lopnr %in% sample[[i]]), ]
  }

  ## Set the imputation parameters for each subsets
  ini <- mice(data, maxit = 0)
  for (i in seq_len(dim(ini$predictorMatrix)[1])) {
    if (!any(ini$predictorMatrix[, c(i)] == 1)) {
      print(colnames(ini$predictorMatrix)[i])
    }
  }
  for (i in imputed_vars) {
    ini$method[i] <- imputed_methods[i]
  }
  ini$method[!colnames(ini$method) %in% imputed_vars] <- ""
  method <- ini$method
  ini$predictorMatrix[, c(imputed_not_include_vars)] <- 0
  predictorMatrix <- ini$predictorMatrix

  ## Multi imputation
  length <- length(datalist)
  datalist_imps <- parallel::mclapply(
    1:length,
    function(i) {
      data_temp_i_imps_list <- parallel::mclapply(
        1:m,
        function(no) {
          data_temp_i_imps <- mice::mice(
            datalist[[i]],
            m = 1,
            method = method,
            predictorMatrix = predictorMatrix,
            seed = no
          )
          data_temp_i_imps <- mice::complete(data_temp_i_imps, 1)
          return(data_temp_i_imps)
        },
        mc.cores = min(floor(cores_2_use / length), 100)
      )
      datalist_imps <- list()
      for (k in 1:m) {
        datalist_imps <- list(data_temp_i_imps_list[[k]])
      }
      return(datalist_imps)
    },
    mc.cores = length
  )
  datalist_imps <- lapply(1:m, function(i) {
    data_imps_list <- list()
    for (j in 1:length) {
      data_imps_list[[j]] <- datalist_imps[[j]][[i]]
    }
    data_imps <- do.call(rbind, data_imps_list)
    return(data_imps)
  })

  dta_imps <- datalist_imps
  return(dta_imps)
}
