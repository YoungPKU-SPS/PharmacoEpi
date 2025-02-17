ipw_truncation <- function(
    ipw.weights,
    trunc.u = 10,
    trunc.l = 0) {
  weights.trunc.temp <-
    ifelse(ipw.weights >= trunc.u,
      trunc.u,
      ipw.weights
    )
  weights.trunc <-
    ifelse(weights.trunc.temp <= trunc.l,
      trunc.l,
      weights.trunc.temp
    )
  return(weights.trunc)
}
