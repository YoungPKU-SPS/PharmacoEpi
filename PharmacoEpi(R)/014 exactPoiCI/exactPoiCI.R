### please check whether using poissons regression to calcualte rate is correct
exactPoiCI <- function(X, conf.level = 0.95) {
  alpha <- 1 - conf.level
  upper <- 0.5 * qchisq((1 - (alpha / 2)), (2 * X))
  lower <- 0.5 * qchisq(alpha / 2, (2 * X + 2))
  return(c(lower, upper))
}
