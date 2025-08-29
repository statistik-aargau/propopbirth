#' constant model
#'
#' @param points_dat # tibble with model results of previous period by spatial_unit, nat
#' @param year_begin # begin of prediction
#' @param year_end # end of prediction
#'
#' @return #tibble with prediction data
#' @export
#' @autoglobal
#'
#' @examples
constant_model <- function(in_dat, year_begin, year_end) {
  # years: numeric
  year_begin <- as.numeric(year_begin)
  year_end <- as.numeric(year_end)

  # last year of previous model
  last_year <- in_dat |>
    filter(year == max(year)) |>
    select(spatial_unit, nat, y)

  # prediction
  pred <- tidyr::expand_grid(
    year = year_begin:year_end,
    spatial_unit = unique(in_dat$spatial_unit),
    nat = unique(in_dat$nat)
  ) |>
    dplyr::left_join(last_year, by = c("spatial_unit", "nat")) |>
    dplyr::mutate(
      category = "constant"
    ) |>
    dplyr::select(spatial_unit, nat, year, y, category)

  # output
  return(pred)
}
