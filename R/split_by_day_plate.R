#' split_by_day_plate
#'
#' split_by_day_plate
#'
#' @param Q2_input_data,day_col,plate_col
#' @return df_list
#' @export

#Split data by user-defined DAY and PLATE_NR column names
split_by_day_plate <- function(Q2_input_data, day_col, plate_col) {
  # Dynamically use the column names provided by the user
  df_grouped <- Q2_input_data %>%
    group_by(!!sym(day_col), !!sym(plate_col))

  # Split the grouped data into a list
  df_list <- df_grouped %>%
    group_split() %>%
    set_names(
      df_grouped %>%
        group_keys() %>%
        mutate(name = paste0("Day_", !!sym(day_col), "_Plate_", !!sym(plate_col))) %>%
        pull(name)
    )

  return(df_list)
}
