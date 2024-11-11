Extract_plate_metadata <- function(Q2_input_metadata, day_col, plate_col, temp_col, location_col) {
  # Group by user-defined DAY and PLATE columns
  df_grouped <- Q2_input_metadata %>%
    group_by(!!sym(day_col), !!sym(plate_col))

  # Split the grouped data into a list and extract the first row
  df_list <- df_grouped %>%
    group_split() %>%
    set_names(
      df_grouped %>%
        group_keys() %>%
        mutate(name = paste0("Day_", !!sym(day_col), "_Plate_", !!sym(plate_col))) %>%
        pull(name)
    ) %>%
    map(~ .x %>% slice(1) %>% select(!!sym(temp_col), !!sym(plate_col), !!sym(day_col), !!sym(location_col)))

  return(df_list)
}
