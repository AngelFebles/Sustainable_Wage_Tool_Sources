library(dplyr)
library(httr)
library(rvest)
library(RSelenium)

# Without this is takes forever to filter table
library(data.table)


get_place_id <- function(county) {
  # Reads the file containing all the county codes
  df <- read.delim("DataFiles/oe.area", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  # Index 2 is the id column
  result <- df |>
    filter(area_name == county) |>
    pull(2) |>
    as.character()

  return(result)
}

download_job_salary_data <- function() {
  # url <- "https://download.bls.gov/pub/time.series/oe/oe.data.1.AllData"
  url <- "https://download.bls.gov/pub/time.series/oe/"
  destfile <- "DataFiles/RawOutputFiles"


  # Start Selenium with a visible Chrome window
  rdriver <- rsDriver(port = sample(7600)[1], browser = c("firefox"), chromever = NULL)
  remDr <- rdriver$client
  remDr$navigate(url)

  # # Wait for the page to load
  Sys.sleep(5)
  print("Downloading...")
  webElem <- remDr$findElement(using = "xpath", "//a[@href='/pub/time.series/oe/oe.data.1.AllData']")
  webElem$clickElement()

  Sys.sleep(5)

  # Close the browser and stop the server
  remDr$close()
  rdriver$server$stop()
  print("Download complete!")


  default_dir <- file.path("", "Users", Sys.info()[["user"]], "Downloads")
  file.rename(file.path(default_dir, "oe.data.1.AllData"), file.path(destfile, "oe.data.1.AllData"))



  df <- read.delim("DataFiles/RawOutputFiles/oe.data.1.AllData", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  head(df)
}

get_job_data_by_place <- function(place) {
  place_id <- get_place_id(place)

  df <- read.delim("DataFiles/RawOutputFiles/oe.data.1.AllData", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  # Decreases the time it takes to filter the table
  setDT(df)

  county_header <- paste0("OEUM00", place_id)

  print("Filtering the data...")

  filtered_rows <- df[grepl(county_header, df[[1]]), ]


  # Remove unneeded columns
  filtered_rows[, c("year", "period", "footnote_codes") := NULL]

  # Reshape for  1 col to 17 cols
  df_reshaped <- data.table(matrix(filtered_rows$value, ncol = 17, byrow = TRUE))

  # To get the job ids
  df_reshaped2 <- data.table(matrix(filtered_rows$series_id, ncol = 17, byrow = TRUE))
  id_col <- df_reshaped2[[1]]

  # Ids have padding at the beginning and end. This removes it
  id_col <- substr(id_col, 18, nchar(id_col))
  id_col <- substr(id_col, 1, nchar(id_col) - 7)


  # df_reshaped[, "Job ID" := id_col]


  final_df <- cbind(id_col, df_reshaped)


  # Set column names
  setnames(final_df, c(
    "Job ID",
    "Employment",
    "Employment percent relative standard error",
    "Hourly mean wage",
    "Annual mean wage",
    "Wage percent relative standard error",
    "Hourly 10th percentile wage",
    "Hourly 25th percentile wage",
    "Hourly median wage",
    "Hourly 75th percentile wage",
    "Hourly 90th percentile wage",
    "Annual 10th percentile wage",
    "Annual 25th percentile wage",
    "Annual median wage",
    "Annual 75th percentile wage",
    "Annual 90th percentile wage",
    "Employment per 1,000 jobs",
    "Location Quotient"
  ))

  print(final_df)


  openxlsx::write.xlsx(final_df, "DataFiles/OutputFiles/salary_data.xlsx", asTable = TRUE)

  # return(df_reshaped)
}

county <- "Racine, WI"

# download_job_salary_data()

get_job_data_by_place(county)
