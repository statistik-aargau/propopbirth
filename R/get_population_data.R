#' FSO population (females at 'fertile' age)
#'
#' @param number_fso # number of FSO table (STAT-TAB)
#' @param year_first # first year
#' @param year_last # last year
#' @param age_fert_min # minimum age (of 'fertile age')
#' @param age_fert_max # maximum age (of 'fertile age')
#' @param spatial_code # official FSO codes, vector
#' @param spatial_unit # spatial unit names (free choice), vector
#' @param with_nationality # TRUE (with nationality), FALSE (without nationality)
#'
#' @return female population at 'fertile age' at the end of the year,
#' tibble (spatial_unit, year, age, pop), with or without nat (nationality)
#' @export
#' @autoglobal
#'
#' @examples
# get_population_data(number_fso = "px-x-0102010000_101",
#         year_first = 2020,
#         year_last = 2023,
#         age_fert_min = 15,
#         age_fert_max = 49,
#         spatial_code = c("0261", "4566", "0198"),
#         spatial_unit = c("Stadt Zürich", "Frauenfeld", "Uster"),
#         with_nationality = TRUE)
get_population_data <- function(number_fso,
                                year_first, year_last,
                                age_fert_min, age_fert_max,
                                spatial_code, spatial_unit,
                                with_nationality = TRUE) {
  # metadata ----------------------------------------------------------------

  fso_metadata <- BFS::bfs_get_metadata(
    number_bfs = number_fso,
    language = "de"
  )


  # years -------------------------------------------------------------------

  query_year <- fso_metadata |>
    dplyr::filter(grepl("Jahr", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::mutate(year_num = as.numeric(values)) |>
    dplyr::filter(year_num >= year_first, year_num <= year_last) |>
    dplyr::pull(values)



  # spatial units -----------------------------------------------------------

  # lookup: codes and names
  spatial_lookup <- tibble(
    spatial_code = spatial_code,
    spatial_unit = spatial_unit
  )

  # preparation
  spatial_prep <- fso_metadata |>
    dplyr::filter(grepl("Kanton", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts))

  # selected spatial units
  spatial_selected <- spatial_prep |>
    dplyr::filter(trimws(values) %in% spatial_code) |>
    dplyr::left_join(spatial_lookup, by = c("values" = "spatial_code"))

  # spatial units: query
  query_spatial <- spatial_selected |>
    dplyr::pull(values)


  # age ---------------------------------------------------------------------

  query_age <- fso_metadata |>
    dplyr::filter(grepl("Alter", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::mutate(age_num = as.numeric(values)) |>
    dplyr::filter(age_num >= age_fert_min, age_num <= age_fert_max) |>
    dplyr::pull(values)


  # sex ---------------------------------------------------------------------

  query_sex <- fso_metadata |>
    dplyr::filter(grepl("Geschlecht", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::filter(values == "2") |>
    dplyr::pull(values)


  # nationality -------------------------------------------------------------

  # nationality lookup
  nat_lookup <- tibble(
    nat_code = c("1", "2", "-99999"),
    nat_text = c("ch", "int", "all"),
    nat_fso_text = c(
      "Schweiz", "Ausland",
      "Staatsangehörigkeit (Kategorie) - Total"
    )
  )

  # with nationality? filter and text
  if (with_nationality) {
    nat_filter <- nat_lookup$nat_code[1:2]
    nat_text <- nat_lookup$nat_text[1:2]
  } else {
    nat_filter <- nat_lookup$nat_code[3]
    nat_text <- nat_lookup$nat_text[3]
  }

  # nationality query
  query_nat <- fso_metadata |>
    dplyr::filter(grepl("Staatsangehörigkeit", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::filter(values %in% nat_filter) |>
    dplyr::pull(values)


  # population --------------------------------------------------------------

  query_pop <- fso_metadata |>
    dplyr::filter(grepl("Bevölkerungstyp", code)) |>
    dplyr::select(code, values, valueTexts) |>
    tidyr::unnest_longer(c(values, valueTexts)) |>
    dplyr::filter(valueTexts == "Ständige Wohnbevölkerung") |>
    dplyr::pull(values)



  # query -------------------------------------------------------------------

  # query parameters
  query_para <- structure(
    list(
      query_year,
      query_spatial,
      query_pop,
      query_nat,
      query_sex,
      query_age
    ),
    names = c(
      "Jahr",
      "Kanton (-) / Bezirk (>>) / Gemeinde (......)",
      "Bevölkerungstyp",
      "Staatsangehörigkeit (Kategorie)",
      "Geschlecht",
      "Alter"
    )
  )

  # query
  fso_data_import <- BFS::bfs_get_data(
    number_bfs = number_fso,
    language = "de",
    query = query_para
  )


  # output ------------------------------------------------------------------

  # fso population
  fso_pop <- fso_data_import |>
    dplyr::left_join(spatial_selected, by = c("Kanton (-) / Bezirk (>>) / Gemeinde (......)" = "valueTexts")) |>
    dplyr::left_join(nat_lookup, by = c("Staatsangehörigkeit (Kategorie)" = "nat_fso_text")) |>
    dplyr::mutate(
      year = as.numeric(Jahr),
      age = as.numeric(substr(Alter, 1, 2))
    ) |>
    rename(
      pop = `Ständige und nichtständige Wohnbevölkerung`,
      nat = nat_text
    ) |>
    select(spatial_unit, year, nat, age, pop) |>
    arrange(spatial_unit, year, nat, age)


  # output
  return(fso_pop)
}
