#' mab (mean age at birth) function
#'
#' @param a0 intercept (to be optimized), one value
#' @param age age (from age_min - 1 until age_max), vector
#' @param y_no_a0 regression result without intercept, vector
#' @param mab_proj projected mab (objective), one value
#'
#' @return diff (absolute difference between mab and mab_proj), one value
#' @export
#' @autoglobal
#'
#' @examples
mab_fun <- function(a0, age, y_no_a0, mab_proj) {
  # y according to regression
  y_reg <- a0 + y_no_a0

  # cumulated fertility rate
  tx_sta_reg <- exp(-exp(y_reg))

  # fertility rate per age
  fer_age <- c(0, diff(tx_sta_reg))

  # mean age at birth
  # on purpose also calculate over (age_min - 1) since
  # contribution at that age is zero
  mab <- weighted.mean(age, fer_age)

  # difference to optimize (i.e. minimize absolute difference)
  diff <- abs(mab - mab_proj)

  # output
  return(diff)
}
