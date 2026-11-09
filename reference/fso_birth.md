# FSO data for births

Data was retrieved from the FSO.

## Usage

``` r
fso_birth
```

## Format

A data frame with 437054 rows and 5 variables:

- year:

  year within the range 2010-2023

- spatial_unit:

  Swiss municipalities by name

- nat:

  Nationality; either "ch" = Swiss or "int" = international

- age:

  Age in full years

- n_birth:

  number of births

## Examples

``` r
dplyr::glimpse(fso_birth)
#> Rows: 4,012
#> Columns: 5
#> $ year         <int> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 200…
#> $ spatial_unit <chr> "Aarau", "Aarau", "Aarau", "Aarau", "Aarau", "Aarau", "Aa…
#> $ nat          <chr> "ch", "ch", "ch", "ch", "ch", "ch", "ch", "ch", "ch", "ch…
#> $ age          <int> 21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 3…
#> $ n_birth      <dbl> NA, NA, NA, NA, 6, 5, 8, NA, 11, 12, 10, 4, 5, 4, 5, 4, 4…
```
