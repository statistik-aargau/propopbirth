#' input data
#'
#' @param pop # female population at 'fertile age', 
#' tibble (spatial_unit, year, age, pop), with or without nat (nationality)
#' @param bir # births
#' tibble (spatial_unit, year, age, pop), with or without nat (nationality)
#' @param year_first # first year
#' @param year_last # last year
#' @param age_fert_min # minimum age (of 'fertile age')
#' @param age_fert_max # maximum age (of 'fertile age')
#' @param fer_last_years # how many years are used to calculate age-specific fertility rates?
#'
#' @return list with: 
#' tfr (total fertility rate), 
#' mab (mean age of the mother at birth), 
#' fer (fertility rate of last year(s))
#' @export
#' @autoglobal
#'
#' @examples
# get_input_data(pop = pop,
#                bir = bir,
#         year_first = 2011,
#         year_last = 2023,
#         age_fert_min = 15,
#         age_fert_max = 49,
#         fer_last_years = 1)

get_input_data <- function(pop, bir,
                    year_first, year_last,                           
                    age_fert_min, age_fert_max,
                    fer_last_years) {
  

# mean annual population --------------------------------------------------

  # unique values
    uni_spatial <- unique(pop$spatial_unit) 
    uni_nat <- unique(pop$nat)
  
  
  # population at the end of the year, all possible variable combinations
    pop_end_year <- tidyr::expand_grid(spatial_unit = uni_spatial,
                       year = (year_first - 1):year_last,
                       nat = uni_nat,
                       age = (age_fert_min - 1):age_fert_max) |> 
    dplyr::left_join(pop, by = c("spatial_unit", "year", "nat", "age")) |> 
    tidyr::replace_na(list(pop = 0)) 
  
  # population next year
    pop_next <- pop_end_year |>
      dplyr::rename(pop_previous = pop) |>
      dplyr::mutate(
        year_next = year + 1,
        age_next = age + 1
      ) |>
      dplyr::select(spatial_unit, year_next, age_next, nat, pop_previous)      
      
  # mean annual population
  pop_mean <- pop_end_year |> 
    dplyr::filter(year >= year_first, age >= age_fert_min) |> 
    dplyr::left_join(pop_next, by = c("spatial_unit", "year" = "year_next", "nat", "age" = "age_next")) |>  
    tidyr::replace_na(list(pop_previous = 0)) |> 
    dplyr::mutate(pop = (pop_previous + pop) / 2) |> 
    dplyr::select(spatial_unit, year, nat, age, pop)
  
  

# fertility rate, all years -----------------------------------------------

  fer_y <- pop_mean |> 
      dplyr::left_join(bir, by = c("spatial_unit", "year", "nat", "age")) |>  
      tidyr::replace_na(list(bir = 0)) |> 
      dplyr::mutate(fer = if_else(pop == 0, NA_real_, bir / pop))  
  
    

# TFR (total fertility rate) ----------------------------------------------

  tfr <- fer_y |>
    dplyr::group_by(spatial_unit, nat, year) |>
    dplyr::summarize(tfr = sum(fer, na.rm = TRUE), .groups = "drop") |> 
    dplyr::arrange(spatial_unit, nat, year)

  

# MAB (mean age of the mother at birth) -----------------------------------
  
  mab <- fer_y |> 
    dplyr::group_by(spatial_unit, nat, year) |> 
    dplyr::filter(!is.na(fer)) |> 
    dplyr::summarize(mab = weighted.mean(age, fer, na.rm = TRUE), .groups = "drop") |> 
    dplyr::arrange(spatial_unit, nat, year)
 
  
  
# fertility rate, last year(s) --------------------------------------------

  # if fer_last_years == 1, only the last year
  # if fer_last_years == 3, average over the last three years
  
    fer <- fer_y |> 
      dplyr::select(-fer) |> 
      dplyr::filter(year >= (max(year)  - fer_last_years + 1)) |> 
      dplyr::group_by(spatial_unit, nat, age) |> 
      dplyr::summarize(pop = sum(pop, na.rm = TRUE),
                bir = sum(bir, na.rm = TRUE), .groups = "drop") |> 
      dplyr::mutate(fer = if_else(pop == 0, NA_real_, bir / pop)) |> 
      dplyr::select(spatial_unit, nat, age, fer) |> 
      dplyr::arrange(spatial_unit, nat, age)      
  
            
# Output ------------------------------------------------------------------
    
# fer_y (fertility per year) is in the output to make plots
# (at the very end of the fertility rate forecast)
  fer_y_out <- fer_y |> 
    rename(birth_rate = fer) |> 
    select(spatial_unit, nat, year, age, birth_rate)
  
  return(list(tfr = tfr, mab = mab, fer = fer, fer_y = fer_y_out))  
  
}
  
  




  