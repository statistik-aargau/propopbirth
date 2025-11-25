# snapshot test for temporal_points()

    Code
      dput(temporal_points(input_past = input_data, input_trend = trend_data,
        year_start = 2027, year_end = 2055, trend_prop = 0.8, z0_prop = 0.7, z1_prop = 0))
    Output
      structure(list(spatial_unit = c("Aarau", "Aarau"), nat = c("ch", 
      "int"), x0 = c(2027, 2027), y0 = c(1.04367077231807, 1.74285303641717
      ), z0 = c(-0.00443341059853193, -0.0171697595329484), x1 = c(2055, 
      2055), y1 = c(0.895487631234005, 1.22114673570507), z1 = c(0, 
      0)), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"
      ))

