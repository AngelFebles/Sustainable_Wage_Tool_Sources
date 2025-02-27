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
    pull(2)

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


get_job_data <- function(county_id) {
  PATH <- "DataFiles/RawOutputFiles/oe.data.1.AllData"


  print("Getting job data...")
  raw_oe <- PATH |>
    readr::read_tsv(
      col_types = list(
        year = "i",
        value = "d",
        footnote_codes = "-",
        .default = "c"
      ),
      na = "-"
    ) |>
    tidyr::separate_wider_position(
      cols = "series_id",
      widths = c(
        survey = 2,
        seasonal = 1,
        area_type = 1,
        state_code = 2,
        area_code = 5,
        industry_code = 6,
        occupation_code = 6,
        datatype_code = 2
      )
    )

  raw_oe <- raw_oe |>
    dplyr::filter(!datatype_code %in% c(16, 17))

  ## Joinning with looking table (filtering and lookup at the same time!!!!)

  PLACES <- tibble::tribble(
    ~Place, ~state_code, ~area_code,
    "US", "00", "00000",
    "WI", "55", "00000",
    "County", "00", county_id
  )

  racine_oe <- raw_oe |>
    dplyr::inner_join(
      PLACES,
      by = c("state_code", "area_code")
    )

  VARIABLES <- tibble::tribble(
    ~datatype_code, ~datatype_name,
    "01", "Employment",
    "02", "Employment percent relative standard error",
    "03", "Hourly mean wage",
    "04", "Annual mean wage",
    "05", "Wage percent relative standard error",
    "06", "Hourly 10th percentile wage",
    "07", "Hourly 25th percentile wage",
    "08", "Hourly median wage",
    "09", "Hourly 75th percentile wage",
    "10", "Hourly 90th percentile wage",
    "11", "Annual 10th percentile wage",
    "12", "Annual 25th percentile wage",
    "13", "Annual median wage",
    "14", "Annual 75th percentile wage",
    "15", "Annual 90th percentile wage"
  )

  # dbl = double!!!


  mean_wages_by_place <- racine_oe |>
    dplyr::inner_join(
      dplyr::filter(
        VARIABLES,
        stringr::str_ends(.data$datatype_name, "mean wage")
      ),
      by = "datatype_code"
    ) |>
    dplyr::select(
      !"datatype_code"
    ) |>
    tidyr::pivot_wider(
      names_from = "datatype_name",
      values_from = "value"
    )

  # print(mean_wages_by_place)

  us_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "US" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean wage`, `Annual mean wage`)

  county_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "County" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean wage`, `Annual mean wage`)

  wisconsin_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "WI" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean wage`, `Annual mean wage`)

  result <- county_wages |>
    dplyr::inner_join(wisconsin_wages, by = "occupation_code", suffix = c("_County", "_WI")) |>
    dplyr::inner_join(us_wages, by = "occupation_code", suffix = c("", "_US"))

  print(result)
  return(result)
}


# TODO: Get the job titles from the job ids
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
# county <- "Racine, WI"
# place_id <- (get_place_id(county))

# get_place_id(county)

download_job_salary_data()

# Racine, WI

place_id <- "39540"
df <- get_job_data(place_id)
openxlsx::write.xlsx(df, "DataFiles/OutputFiles/salary_data_comparison.xlsx", asTable = TRUE)
