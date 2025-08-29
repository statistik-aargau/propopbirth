#' FSO data for births
#'
#' @description Data was retrieved from the FSO.
#'

# Load data
fso_birth <- data.table::fread("data-raw/fso_birth.csv") |> 
  as_tibble()

# Add data frames to package
usethis::use_data(fso_birth, overwrite = TRUE)
