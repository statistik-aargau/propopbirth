#' temporal model: constant
#'
#' @param points_dat # tibble with point information (x0, y0) by spatial_unit, nat
#' @param year_begin # begin of prediction
#' @param year_end # end of prediction
#'
#' @return #tibble with prediction data
#' @export
#' @autoglobal
#'
#' @examples
temporal_constant <- function(points_dat, year_begin, year_end) {
  
  # years: numeric  
    year_begin <- as.numeric(year_begin)  
    year_end <- as.numeric(year_end)    
  
  # prediction
  pred <- tidyr::expand_grid(
    year = year_begin:year_end,
    spatial_unit = unique(points_dat$spatial_unit),
    nat = unique(points_dat$nat)
  ) |>
    dplyr::left_join(points_dat, by = c("spatial_unit", "nat")) |>
    dplyr::mutate(
      y = y0,
      category = "constant"
    ) |>
    dplyr::select(spatial_unit, nat, year, y, category)

  # output
  return(pred)
}
