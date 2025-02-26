library(dplyr)
library(httr)
library(rvest)
library(RSelenium)

# Without this is takes forever to filter table
library(data.table)


get_place_id <- function(county) {
  # Reads the file containing all the county codes
  df <- read.delim("DataFiles/oe.area", sep = "\t", header = TRUE, colClasses = "character")

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

get_job_data_by_place <- function(place_id) {
  # place_id <- get_place_id(place)

  df <- read.delim("DataFiles/RawOutputFiles/oe.data.1.AllData", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  # Decreases the time it takes to filter the table
  setDT(df)


  county_header <- place_id

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
    "job_id",
    "employment",
    "employment_percent_relative_standard_error",
    "hourly_mean_wage",
    "annual_mean_wage",
    "wage_percent_relative_standard_error",
    "hourly_10th_percentile_wage",
    "hourly_25th_percentile_wage",
    "hourly_median_wage",
    "hourly_75th_percentile_wage",
    "hourly_90th_percentile_wage",
    "annual_10th_percentile_wage",
    "annual_25th_percentile_wage",
    "annual_median_wage",
    "annual_75th_percentile_wage",
    "annual_90th_percentile_wage",
    "employment_per_1000_jobs",
    "location_quotient"
  ))

  # print(final_df)

  return(final_df)
}

get_national_job_data <- function(place_id) {
  # place_id <- get_place_id(place)

  df <- read.delim("DataFiles/RawOutputFiles/oe.data.1.AllData", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  # Decreases the time it takes to filter the table
  setDT(df)

  county_header <- place_id

  print("Filtering the data...")

  filtered_rows <- df[grepl(county_header, df[[1]]), ]


  # Remove unneeded columns
  filtered_rows[, c("year", "period", "footnote_codes") := NULL]

  # Reshape for  1 col to 17 cols
  df_reshaped <- data.table(matrix(filtered_rows$value, ncol = 15, byrow = TRUE))

  # To get the job ids
  df_reshaped2 <- data.table(matrix(filtered_rows$series_id, ncol = 15, byrow = TRUE))
  id_col <- df_reshaped2[[1]]

  # Ids have padding at the beginning and end. This removes it
  # id_col <- trimws(id_col)
  # id_col <- substr(id_col, 1, nchar(id_col) - 2)
  # id_col <- substr(id_col, 17)

  id_col <- trimws(id_col)
  id_col <- substr(id_col, 18, nchar(id_col) - 2)

  # id_col2[1:1107]


  id_col2 <- id_col[1:nrow(df_reshaped)]
  id_col2 <- id_col[1:1105]
  df_reshaped <- df_reshaped[1:1105, ]

  # df_reshaped[, "Job ID" := id_col]


  final_df <- cbind(id_col2, df_reshaped)


  # Set column names
  setnames(final_df, c(
    "job_id",
    "employment",
    "employment_percent_relative_standard_error",
    "hourly_mean_wage",
    "annual_mean_wage",
    "wage_percent_relative_standard_error",
    "hourly_10th_percentile_wage",
    "hourly_25th_percentile_wage",
    "hourly_median_wage",
    "hourly_75th_percentile_wage",
    "hourly_90th_percentile_wage",
    "annual_10th_percentile_wage",
    "annual_25th_percentile_wage",
    "annual_median_wage",
    "annual_75th_percentile_wage",
    "annual_90th_percentile_wage"
  ))

  # print(final_df)

  return(final_df)
}

compare_county_and_national <- function(county_df, national_df) {
  county_df_selected <- county_df |> select(job_id, hourly_mean_wage, annual_mean_wage)
  national_df_selected <- national_df |> select(job_id, hourly_mean_wage, annual_mean_wage)

  result <- merge(county_df_selected, national_df_selected, by = "job_id", all = FALSE)

  # print(result)

  setnames(result, c(
    "job_id",
    "county_hourly_mean_wage",
    "county_annual_mean_wage",
    "national_hourly_mean_wage",
    "national_annual_mean_wage"
  ))

  # Get job titles from the job ids




  return(result)
}

get_job_titles <- function(job_ids) {
  # Reads the file containing all the county codes
  df <- read.delim("DataFiles/oe.occupation", sep = "\t", header = TRUE, colClasses = "character")
  colnames(df)[1] <- "job_id"
  job_titles <- df |>
    filter(series_id %in% job_ids) |>
    select(series_id, job_id)
  return(job_titles)
}


# 0039540
county <- "Racine, WI"
place_id <- get_place_id(county)

# get_place_id(county)
# download_job_salary_data()

print("Getting job data of county...")
county_df <- get_job_data_by_place(paste0("OEUM", place_id))

openxlsx::write.xlsx(county_df, "DataFiles/OutputFiles/salary_data_county.xlsx", asTable = TRUE)

print("Getting national job data ...")

national_df <- get_national_job_data("OEUN0000000")
openxlsx::write.xlsx(national_df, "DataFiles/OutputFiles/salary_data_national.xlsx", asTable = TRUE)


print("Merging the data...")
county_and_national <- compare_county_and_national(county_df, national_df)
openxlsx::write.xlsx(county_and_national, "DataFiles/OutputFiles/county_and_national.xlsx", asTable = TRUE)
