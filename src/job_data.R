get_place_id <- function(county) {
  # Reads the file containing all the county codes
  df <- read.delim("DataFiles/oe.area", sep = "\t", header = TRUE, colClasses = "character")

  # Index 2 is the id column
  result <- df |>
    dplyr::filter(area_name == county) |>
    dplyr::pull(2)

  return(result)
}

download_job_salary_data <- function() {
  # url <- "https://download.bls.gov/pub/time.series/oe/oe.data.1.AllData"
  url <- "https://download.bls.gov/pub/time.series/oe/"
  destfile <- "DataFiles/RawOutputFiles"

  # Start Selenium with a visible Chrome window
  rdriver <- RSelenium::rsDriver(port = sample(7600)[1], browser = c("firefox"), chromever = NULL)
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
  print("Getting job data...")
  df <- read.delim("DataFiles/RawOutputFiles/oe.data.1.AllData", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  # head(df)
  # return(df)
}

get_job_data <- function(county_id) {
  PATH <- "DataFiles/RawOutputFiles/oe.data.1.AllData"

  print("Transforming file to dataframe...")
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
  print("Filtering data...")
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
    "03", "Hourly mean",
    "04", "Annual mean",
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

  mean_wages_by_place <- racine_oe |>
    dplyr::inner_join(
      dplyr::filter(
        VARIABLES,
        stringr::str_ends(.data$datatype_name, "mean")
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
  # Opinionated column filtering
  pivoted <- mean_wages_by_place |>
    dplyr::filter(
      .data$industry_code == "000000"
    ) |>
    dplyr::select(
      "occupation_code",
      "Hourly mean",
      "Annual mean",
      "Place"
    ) |>
    tidyr::pivot_wider( # Redistributes the columns
      names_from = "Place",
      values_from = c("Hourly mean", "Annual mean"),
      names_sep = " "
    )

  us_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "US" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean`, `Annual mean`)

  county_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "County" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean`, `Annual mean`)

  wisconsin_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "WI" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean`, `Annual mean`)

  result <- county_wages |>
    dplyr::inner_join(wisconsin_wages, by = "occupation_code", suffix = c(" County", " WI")) |>
    dplyr::inner_join(us_wages, by = "occupation_code", suffix = c("", " US"))

  # print(result)
  return(result)
}

# TODO: Get the job titles from the job ids
get_job_titles <- function(df) {
  print("Getting job titles...")
  # Reads the file containing all the county codes
  result <- read.delim("DataFiles/oe.occupation", sep = "\t", header = TRUE, colClasses = "character")

  # Only take the code and name cols
  result <- result[, 1:2]

  merged_data <- merge(df, result, by.x = "occupation_code", by.y = "occupation_code", all = FALSE)


  return(merged_data)
}

# 0039540
# county <- "Racine, WI"
# place_id <- (get_place_id(county))

# get_place_id(county)


get_education_requirement <- function(df) {
  print("Getting education requirement data...")
  ed_reqs <- readr::read_csv("DataFiles/job_reques.csv", show_col_types = FALSE) |>
    dplyr::mutate(across(2, ~ gsub("-", "", .)))

  # Only job ID and ed req
  ed_reqs <- ed_reqs[2:3] |>
    dplyr::rename(occupation_code = 1, education_requirement = 2)

  df <- df |>
    dplyr::inner_join(ed_reqs, by = "occupation_code")

  return(df)
}


get_job_data_and_ed_req <- function() {
  print("Getting job data and education requirements...")


  # Racine, WI

  place_id <- "39540"
  df <- get_job_data(place_id)

  df_with_ed_req <- get_education_requirement(df)

  # print(df_with_ed_req)

  df_with_titles <- get_job_titles(df_with_ed_req)

  openxlsx::write.xlsx(df_with_titles, "DataFiles/OutputFiles/full_job_data.xlsx", asTable = TRUE)

  print("Job data written to full_job_data.xlsx")
}


# get_job_data_and_ed_req()
