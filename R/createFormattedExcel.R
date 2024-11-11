#' createFormattedExcel
#'
#' Q2 Excel Metadata creation
#'
#' @param metadata_values,file_name,sample_design_data,raw_data
#' @return Exported Excel sheet with raw/output data and metadata
#' @export

### Q2 Excel Metadata creation
createFormattedExcel <- function(
    metadata_values = default_metadata_values,
    file_name = "Formatted_Metadata_and_Phenotypes.xlsx",
    sample_design_data = NULL,  # Data for "Sample_design" sheet
    raw_data = NULL  # Data for "Raw_data" sheet
) {
  # Create a new workbook
  workbook <- createWorkbook()

  # Add the "Metadata" sheet
  addWorksheet(workbook, "Metadata")

  # Convert the metadata_values list into a data frame
  df1 <- data.frame(
    LABEL = metadata_values$LABEL,
    DEFINITION = metadata_values$DEFINITION,
    VALUE = metadata_values$VALUE,
    stringsAsFactors = FALSE
  )

  # Write data to the "Metadata" sheet
  writeData(workbook, "Metadata", df1)

  # Apply header style: Excel-like blue background with white text
  header_style <- createStyle(
    fontColour = "white", fgFill = "#4472C4", textDecoration = "bold",
    halign = "center", valign = "top", border = "TopBottomLeftRight", borderColour = "#B7B7B7"
  )
  addStyle(workbook, "Metadata", style = header_style, rows = 1, cols = 1:ncol(df1), gridExpand = TRUE)

  # Apply row styles: Alternate between white and light blue, with Excel-like colors and top alignment
  light_blue_fill <- createStyle(
    fgFill = "#D9E1F2", border = "TopBottomLeftRight", borderColour = "#B7B7B7",
    wrapText = TRUE, valign = "top"
  )
  white_fill <- createStyle(
    fgFill = "white", border = "TopBottomLeftRight", borderColour = "#B7B7B7",
    wrapText = TRUE, valign = "top"
  )

  # Loop for applying alternating row styles in the "Metadata" sheet
  for (i in 2:(nrow(df1) + 1)) {
    if (i %% 2 == 0) {
      addStyle(workbook, "Metadata", style = light_blue_fill, rows = i, cols = 1:ncol(df1), gridExpand = TRUE)
    } else {
      addStyle(workbook, "Metadata", style = white_fill, rows = i, cols = 1:ncol(df1), gridExpand = TRUE)
    }
  }

  # Adjust column widths for the "Metadata" sheet
  setColWidths(workbook, "Metadata", cols = 1, widths = 20)  # Set width for LABEL column
  setColWidths(workbook, "Metadata", cols = 2, widths = 50)  # Set width for DEFINITION column
  setColWidths(workbook, "Metadata", cols = 3, widths = 30)  # Set width for VALUE column

  # Generate the "Phenotypes" sheet based on column names from Sample_design and Raw_data
  sample_design_cols <- if (!is.null(sample_design_data)) colnames(sample_design_data) else character(0)
  raw_data_cols <- if (!is.null(raw_data)) colnames(raw_data) else character(0)

  # Combine the column names and create a phenotypes data frame
  combined_cols <- unique(c(sample_design_cols, raw_data_cols))

  # Ensure accurate matching by trimming whitespace
  combined_cols_trimmed <- trimws(combined_cols)
  short_names_trimmed <- trimws(default_phenotypes_df$ShortName)

  # Match combined column names with the defaults using "ShortName"
  matched_df <- default_phenotypes_df[short_names_trimmed %in% combined_cols_trimmed, ]

  # Add any unmatched columns with default placeholders
  unmatched_cols <- setdiff(combined_cols_trimmed, short_names_trimmed)
  if (length(unmatched_cols) > 0) {
    unmatched_df <- data.frame(
      Name = unmatched_cols,
      ShortName = unmatched_cols,  # Placeholder ShortNames
      Description = rep("Description to be added", length(unmatched_cols)),  # Placeholder descriptions
      DataType = rep("text", length(unmatched_cols)),  # Placeholder data types
      UnitName = rep(NA, length(unmatched_cols)),  # Placeholder unit names
      UnitAbbreviation = rep(NA, length(unmatched_cols)),  # Placeholder unit abbreviations
      stringsAsFactors = FALSE
    )
    phenotypes_df <- rbind(matched_df, unmatched_df)
  } else {
    phenotypes_df <- matched_df
  }

  # Filter out unwanted rows from the "Phenotypes" sheet
  rows_to_remove <- c(
    "B1", "C1", "D1", "E1", "F1", "F2", "E2", "D2", "C2", "B2",
    "A2", "A3", "B3", "C3", "D3", "E3", "F3", "F4", "E4", "D4",
    "C4", "B4", "A4", "A5", "B5", "C5", "D5", "E5", "F5", "F6",
    "E6", "D6", "C6", "B6", "A6", "A7", "B7", "C7", "D7", "E7",
    "F7", "F8", "E8", "D8", "C8", "B8", "A8"
  )
  phenotypes_df <- phenotypes_df[!(phenotypes_df$Name %in% rows_to_remove | phenotypes_df$ShortName %in% rows_to_remove), ]

  # Add the "Phenotypes" sheet as the second sheet
  addWorksheet(workbook, "Phenotypes")

  # Write data to the "Phenotypes" sheet
  writeData(workbook, "Phenotypes", phenotypes_df)

  # Apply header style to the "Phenotypes" sheet
  addStyle(workbook, "Phenotypes", style = header_style, rows = 1, cols = 1:ncol(phenotypes_df), gridExpand = TRUE)

  # Apply alternating row styles in the "Phenotypes" sheet
  for (i in 2:(nrow(phenotypes_df) + 1)) {
    if (i %% 2 == 0) {
      addStyle(workbook, "Phenotypes", style = light_blue_fill, rows = i, cols = 1:ncol(phenotypes_df), gridExpand = TRUE)
    } else {
      addStyle(workbook, "Phenotypes", style = white_fill, rows = i, cols = 1:ncol(phenotypes_df), gridExpand = TRUE)
    }
  }

  # Adjust column widths for the "Phenotypes" sheet
  setColWidths(workbook, "Phenotypes", cols = 1:ncol(phenotypes_df), widths = "auto")

  # Add the "Sample_design" and "Raw_data" sheets after the "Phenotypes" sheet
  addWorksheet(workbook, "Sample_design")
  addWorksheet(workbook, "Raw_data")

  # Write sample_design_data and raw_data to their respective sheets if provided
  if (!is.null(sample_design_data)) {
    writeData(workbook, "Sample_design", sample_design_data)
  }
  if (!is.null(raw_data)) {
    writeData(workbook, "Raw_data", raw_data)
  }

  # Save the workbook
  saveWorkbook(workbook, file = file_name, overwrite = TRUE)
}

# Example: Using the function
#createFormattedExcel()
