#' Check: tibble with columns spatial_unit and nat?
#' @param x tibble
#' @return TRUE if tibble with these columns, else FALSE
#' @export
#' @examples
is_tibble_with_cols <- function(x) {
  inherits(x, "tbl_df") && all(c("spatial_unit", "nat") %in% names(x))
}


