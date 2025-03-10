#' Create Combined Phenotypes Metadata
#'
#' This function creates default metadata for Q2 data and combines the default phenotypes data frame
#' with an output phenotypes data frame.
#'
#' @return A combined phenotypes data frame with unique entries.
#' @export
create_combined_phenotypes_df <- function() {

  # Define default metadata values
  default_metadata_values <- list(
    LABEL = c(
      "Title", "Description", "Rights", "Date of creation", "Publisher", "Format", "Language",
      "Source", "Type", "Subject", "Contact", "Investigation Title",
      "Investigation Description", "Investigation unique ID",
      "Associated data file link", "Associated data file description",
      "Associated data file version", "Information"
    ),
    DEFINITION = c(
      "A name given to the resource. Typically, a Title will be a name by which the resource is formally known.",
      "An account of the resource. Description may include but is not limited to: an abstract, table of contents, graphical depiction, or a free-text account of the content.",
      "Information about rights held in and over the resource.",
      "A point or period of time associated with an event in the life cycle of the resource.",
      "An entity responsible for making the resource available.",
      "The file format, physical medium, or dimensions of the resource.",
      "A language of the resource. Recommended best practice is to use a controlled vocabulary such as RFC 4646.",
      "A related resource from which the described resource is derived.",
      "The nature or genre of the resource. Recommended best practice is to use a controlled vocabulary such as DCMI Type.",
      "The topic of the resource. Typically, the subject will be represented using keywords or key phrases.",
      "The individual that can be contacted about this data package.",
      "Human-readable string summarising the investigation.",
      "Human-readable text describing the investigation aim and methods.",
      "Identifier comprising the unique name of the investigation.",
      "Link to the data file (or digital object) in an open and accessible database.",
      "Description of the format of the data file. May include file size, format, and data structure information.",
      "The version of the dataset (the actual data).",
      "The PHENOTYPES sheet should contain a definition for all phenotypic traits"
    ),
    VALUE = c(
      "e.g Loc_TOS1_23_Q2Respiration_Rawdata",
      "Raw fluorophore data from Example field trial experiment.", "Blank",
      "Collected from 5-7 September 1782. Raw data from field experiment.",
      "Performed by XXX", "Excel file (.xlsx)", "Blank",
      "Q2 flurophore Instrument", "Blank",
      "Q2, dark respiration, O2 consumption, Q2-dark, sample description.",
      "Contact inPerformed bylank", "Blank", "e.g ANU-1234-0123", "Blank", "Blank", "Blank",
      "The PHENOTYPES sheet should contain a definition for all phenotypic traits"
    )
  )

  # Define the default phenotypes data frame ######
  default_phenotypes_df <- data.frame(
    Name = c(
      "Name", "ENTRY", "Location", "Location code", "Location Name",
      "Plot", "Experiment", "Field Replicate", "PlotRange", "PlotRow",
      "BARCODE", "Sample", "Measurement temperture", "Day", "Plate",
      "Plate row", "Plate column", "Leaf Area", "Leaf Dry Mass",
      "Plate number of Q2", "SCANTIME", "Hours", "temperature", "A1",
      "Temperature", "DAY", "PLATE", "SCANTIME"  # Added new entries here
    ),
    ShortName = c(
      "Name", "ENTRY", "LOC", "LOC.Code", "Loc.Name",
      "Plot", "Experiment", "FRep", "PlotRange", "PlotRow",
      "BARCODE", "Sample", "MeasurementTemp", "Day", "Plate",
      "PRow", "PCol", "LeafArea", "LeafDryMass", "PLATE_NR",
      "SCANETIME", "HRS", "TEMPERATURE", "A1",
      "Temperature", "DAY", "PLATE", "SCANTIME"  # Added new entries here
    ),
    Description = c(
      "Name of the genotype", "Entry number", "Assigned field site location numeric",
      "Field site location code", "Field site location name", "Field plot number",
      "Name of the experiment from the trial coordinators", "Field replicate number with 2 plots per genotype",
      "Plot range", "Plot row", "Barcode assigned to genotype",
      "Sample number of an individual plot", "The set temperature at which the sample was designated to be measured on the Q2",
      "Day of the field trial", "Assigned Q2 sample plate number",
      "Assigned Q2 sample plate row", "Assigned Q2 sample plate column",
      "Area of leaf sampled", "Dry mass of leaf sampled",
      "The Plate position in Q2 coordinates", "The date and time of the scan of each plate",
      "The time since the Q2 run began in hours",
      "The set temperature of the sample tubes in Q2 matching the sample design MeasurementTemp",
      "A1-A8 are the tube coordinates in Q2 plate corresponding to PRow 1-6 (A-F) and PCol 1-8 in sample design",
      "Q2Temperature", "Q2 run day", "Q2 plate", "Q2 Scan time"  # Added new descriptions here
    ),
    DataType = c(
      "text", "numeric", "numeric", "text", "text", "numeric",
      "text", "numeric", "numeric", "numeric", "text", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "date", "numeric",
      "numeric", "text", "numeric", "numeric", "numeric", "date"  # Added new data types here
    ),
    UnitName = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      "Degree Celsius", NA, NA, NA, NA, "square centimetres",
      "grams", NA, NA, "hours", "Degree Celsius", NA,
      "Degree Celsius", NA, NA, NA  # Added new unit names here
    ),
    UnitAbbreviation = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      "°C", NA, NA, NA, NA, "cm²", "g", NA, NA, "hrs", "°C", NA,
      "°C", NA, NA, NA  # Added new unit abbreviations here
    ),
    stringsAsFactors = FALSE
  )

  #######
  OutputData_phenotypes_df <- data.frame(
    Name = c("Name", "ENTRY", "Loc", "Location Code", "Location Name",
             "Plot", "Experiment", "Field Replicate", "Plot Range", "Plot Row",
             "BARCODE", "Sample", "Day", "Plate", "Plate Row", "Plate Column",
             "Leaf Area", "Leaf Dry Mass", "Q2Temperature", "Well", "Elevation",
             "Pressure_kPa", "Scantime", "slopeRaw", "R2_SlopeRaw", "slopeZero",
             "Q2 Standard Nitrogen", "slopeBlank", "StandardBlank", "Q2Range",
             "slopeRawAdj", "slopeRawAdjRan", "slopeRawAdjSloRan", "SlopeMolar",
             "SlopeMolarAdjSlo", "slopeMolarAdjRan", "slopeMolarAdjSloRan",
             "RespirationAreaRaw", "RespirationAreaAdjSlo", "RespirationAreaAdjRan",
             "RespirationAreaAdjSloRan", "RespirationMassRaw", "RespirationMassAdjSlo",
             "RespirationMassAdjRan", "RespirationMassAdjSloRan"),
    `ShortName` = c("Name", "ENTRY", "Loc", "Loc.Code", "Loc.Name",
                    "Plot", "Experiment", "FRep", "PlotRange", "PlotRow",
                    "BARCODE", "Sample", "Day", "Plate", "PRow", "PCol",
                    "LeafArea", "LeafDryMass", "Q2Temperature", "Well", "Elevation",
                    "Pressure_kPa", "Scantime", "slopeRaw", "R2_SlopeRaw", "slopeZero",
                    "Q2StandardN", "slopeBlank", "StandardBlank", "Q2Range",
                    "slopeRawAdj", "slopeRawAdjRan", "slopeRawAdjSloRan", "SlopeMolar",
                    "SlopeMolarAdjSlo", "slopeMolarAdjRan", "slopeMolarAdjSloRan",
                    "RespirationAreaRaw", "RespirationAreaAdjSlo", "RespirationAreaAdjRan",
                    "RespirationAreaAdjSloRan", "RespirationMassRaw", "RespirationMassAdjSlo",
                    "RespirationMassAdjRan", "RespirationMassAdjSloRan"),
    Description = c(
      "Name of the genotype", "Entry number", "Assigned field site location numeric",
      "Field site location code", "Field site location name", "Field plot number",
      "Name of the experiment from the trial coordinators", "Field replicate number with 2 plots per genotype",
      "Plot range", "Plot row", "Barcode assigned to genotype",
      "Sample number of an individual plot", "Day of the field trial",
      "Assigned Q2 sample plate number", "Assigned Q2 sample plate row", "Assigned Q2 sample plate column",
      "The area of leaf measured", "The dry mass of the leaf measured",
      "Measuring temperature of the Q2 instrument", "Sampling plate well position",
      "Elevation of the field site", "Mean sea level pressure of the field site each day of measurement",
      "Date and time each experimental run began", "Raw O2 uptake slope",
      "Goodness of fit of the raw O2 uptake slope", "O2 slope of a nitrogen flushed sample tube",
      "The Nitrogen standard value", "O2 slope of an air filled sample tube with no sample",
      "The air standard value", "The range between the air and nitrogen standard",
      "Raw O2 uptake slope adjusted for slopes from the standards",
      "Raw O2 uptake slope adjusted for the range of the standards",
      "Raw O2 uptake slope adjusted for the slope and range of the standards",
      "Raw slope converted to a molar concentration",
      "Raw slope adjusted for standard slopes and converted to a molar concentration",
      "Raw slopes adjusted for range converted to a molar concentration",
      "Raw slopes adjusted for slopes and range of standards converted to molar concentration",
      "Slope Molar converted to respiration per unit of leaf area",
      "Slope Molar standard slope adjusted converted to respiration per unit of leaf area",
      "Slope Molar standard range adjusted converted to respiration per unit of leaf area",
      "Slope Molar standard slope and range adjusted converted to respiration per unit of leaf area",
      "Slope Molar converted to respiration per unit of dry mass",
      "Slope Molar standard slope adjusted converted to respiration per unit of dry mass",
      "Slope Molar standard range adjusted converted to respiration per unit of dry mass",
      "Slope Molar standard slope and range adjusted converted to respiration per unit of dry mass"
    ),
    DataType = c(
      "text", "numeric", "numeric", "text", "text", "numeric",
      "text", "numeric", "numeric", "numeric", "text", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "date", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric"
    ),
    UnitName = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      "square centimetres", "grams", "Degree Celsius", NA, "metres",
      "kilo Pascals", NA, "O2 as a fraction of air consumed per hour",
      "unitless", "O2 as a fraction of air consumed per hour",
      "O2 as a fraction of O2 in air", "O2 as a fraction of air consumed per hour",
      "O2 as a fraction of O2 in air", "O2 as a fraction of O2 in air",
      "O2 as a fraction of air consumed per hour",
      "O2 as a fraction of air consumed per hour",
      "O2 as a fraction of air consumed per hour",
      "µmol of O2 consumed per hour",
      "µmol of O2 consumed per hour", "µmol of O2 consumed per hour",
      "µmol of O2 consumed per hour", "µmol of O2 consumed per m2 of leaf per second",
      "µmol of O2 consumed per m2 of leaf per second",
      "µmol of O2 consumed per m2 of leaf per second",
      "µmol of O2 consumed per m2 of leaf per second",
      "µmol of O2 consumed per m2 of leaf per second",
      "µmol of O2 consumed per m2 of leaf per second",
      "µmol of O2 consumed per m2 of leaf per second",
      "µmol of O2 consumed per m2 of leaf per second"
    ),
    UnitAbbreviation = c(
      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      "cm²", "g", "°C", NA, "m", "kPa", NA, "O2 hour-1 fraction air",
      "unitless", "O2 hour-1 fraction air", "ratio",
      "O2 hour-1 fraction air", "ratio", "ratio",
      "O2 hour-1 fraction air", "O2 hour-1 fraction air",
      "O2 hour-1 fraction air", "µmol O2 hour-1",
      "µmol O2 hour-1", "µmol O2 hour-1", "µmol O2 hour-1",
      "µmol O2 m-2 s-1", "µmol O2 m-2 s-1", "µmol O2 m-2 s-1",
      "µmol O2 m-2 s-1", "µmol O2 m-2 s-1", "µmol O2 m-2 s-1",
      "µmol O2 m-2 s-1", "µmol O2 m-2 s-1"
    ),
    stringsAsFactors = FALSE
  )


  # Combine the original and new phenotypes data frames
  combined_phenotypes_df <- rbind(default_phenotypes_df, OutputData_phenotypes_df)

  # Remove any duplicate rows based on the "Name" column to ensure uniqueness
  combined_phenotypes_df <- combined_phenotypes_df[!duplicated(combined_phenotypes_df$Name), ]


  return(list(
    combined_phenotypes_df = combined_phenotypes_df,
    default_metadata_values = default_metadata_values
  ))
}

# # Example usage
# combined_df <- create_combined_phenotypes_df()

# # Accessing each component from the list
# combined_phenotypes <- combined_df$combined_phenotypes_df
# metadata_values <- combined_df$default_metadata_values



