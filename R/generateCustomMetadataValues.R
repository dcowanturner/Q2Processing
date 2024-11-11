#' generateCustomMetadataValues
#'
#' Function to create default metadata values with optional user settings
#'
#' @param LeafArea Test test test
#' @return default metadata values with optional user settings
#' @export
generateCustomMetadataValues <- function(
    Data_Title = "e.g Loc_TOS1_23_Q2Respiration_Rawdata",
    Summary_Info = "Griffith field trial TOS1 2023",
    TubeSize = "2 mL",
    LeafArea = "1.9 cm2",
    Light_Dark = "Dark",
    Experiment_Description = NULL,
    Collected = "Collected from 5-7 September 1782. Raw data from field experiment.",
    Performed = "Performed by XXX",
    Contact = "Contact XXX",
    ProjectID = "e.g ANU-1234-0123",
    Metadata_Excel_file_description = "File contains a Phenotypes sheet that describes each column of data, Sample_Design sheet that gives information on samples and their field and instrument positions, and Raw_Data sheet that contains the fluorophore output from the Q2 instrument.",
    Rights = "",
    ProjectTitle = "",
    ProjectDescription = "",
    FileDescription = "",
    FileVersion = ""
) {
  # If Experiment_Description is not provided, generate it dynamically
  if (is.null(Experiment_Description)) {
    Experiment_Description <- paste0(
      "Raw fluorophore data from ", Summary_Info, " with ", LeafArea,
      " leaf tissue per ", TubeSize, " sample tube placed in Q2 fluorophore instrument and measured in the ",
      Light_Dark, ". ", Metadata_Excel_file_description
    )
  }

  # Dynamic metadata values
  custom_Metadata_VALUE <- c(
    Data_Title,
    Experiment_Description,
    Rights,
    Collected,
    Performed,
    "Excel file (.xlsx)",
    "",
    "Q2 flurophore Instrument",
    "",
    "Q2, dark respiration, O2 consumption, Q2-dark, sample description.",
    Contact,
    ProjectTitle,
    ProjectDescription,
    ProjectID,
    "",
    FileDescription,
    FileVersion,
    ""
  )

  # Create the custom metadata values list
  custom_metadata_values <- list(
    LABEL = default_metadata_values$LABEL,
    DEFINITION = default_metadata_values$DEFINITION,
    VALUE = custom_Metadata_VALUE
  )

  return(custom_metadata_values)
}

