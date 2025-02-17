######################### boot.pval for boot distribution and boot percentile ci #########
boot.pval <- function(boot_distribution,
                      type = "perc",
                      theta_null = 0,
                      pval_precision = NULL,
                      ...) {
  if (is.null(pval_precision)) {
    pval_precision <- 1 / length(boot_distribution)
  }

  # Create a sequence of alphas:
  alpha_seq <- seq(1e-16, 1 - 1e-16, pval_precision)

  # Compute the 1-alpha confidence intervals, and extract
  # their bounds:
  ci <- suppressWarnings(
    lapply(seq_along(alpha_seq), function(no) {
      data.frame(
        quantile(boot_distribution, alpha_seq[no]),
        quantile(boot_distribution, 1 - alpha_seq[no])
      )
    })
  )

  bounds <- do.call(rbind, ci)

  # Find the smallest alpha such that theta_null is not contained in the 1-alpha
  # confidence interval:
  alpha <- alpha_seq[which.min(theta_null >= bounds[, 1] & theta_null <= bounds[, 2])]

  # Return the p-value:
  return(alpha)
}
