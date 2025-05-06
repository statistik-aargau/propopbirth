#' optimize the mab function
#'
#' @param mab_proj objective of the optimization
#' @param a0 intercept (to be optimized), one value
#' @param age age (from age_min - 1 until age_max), vector
#' @param y_no_a0 regression result without intercept, vector
#' @param maxit maximum iterations (of optim function)
#' @param abstol absolute tolerance (of optim function)
#'
#' @return tibble with optimized parameter and objective function
#' @export
#'
#' @examples
  opt_fun <- function(mab_proj, a0, age, y_no_a0, maxit, abstol){

      res <- optim(par = a0,
                    mab_fun,
                      age = age,
                       y_no_a0 = y_no_a0,
                       mab_proj = mab_proj,
                    method = "BFGS",
                    control = list(maxit = maxit,
                                   abstol = abstol))

      out <- tibble(par = res$par,
             value = res$value)

      return(out)


  }
