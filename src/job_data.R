get_place_id <- function(county) {
  #' Retrieves the place ID for a given county
  #'
  #' This function reads a file containing county codes and extracts the place ID
  #' for the specified county.
  #'
  #' @param county A string representing the name of the county.
  #' @return A string representing the place ID for the county.

  df <- read.delim("DataFiles/oe.area", sep = "\t", header = TRUE, colClasses = "character")
  result <- df |>
    dplyr::filter(area_name == county) |>
    dplyr::pull(2) |>
    substring(3)
}

get_state_id <- function(state) {
  #' Retrieves the state ID for a given state
  #'
  #' This function reads a file containing state codes and extracts the state ID
  #' for the specified state.
  #'
  #' @param state A string representing the name of the state.
  #' @return A string representing the state ID for the state.

  df <- read.delim("DataFiles/oe.area", sep = "\t", header = TRUE, colClasses = "character")
  result <- df |>
    dplyr::filter(area_name == state) |>
    dplyr::pull(2) |>
    substr(start = 1, stop = 2)
}

download_job_salary_data <- function() {
  #' Downloads job salary data from the BLS website
  #'
  #' This function uses Selenium to navigate to the BLS website and download the
  #' job salary data file. The file is then moved to the specified directory.
  #'
  #' @return None. Downloads the file and moves it to the `DataFiles/RawOutputFiles` directory.

  url <- "https://download.bls.gov/pub/time.series/oe/"
  destfile <- "DataFiles/RawOutputFiles"

  rdriver <- RSelenium::rsDriver(port = sample(7600)[1], browser = c("firefox"), chromever = NULL)
  remDr <- rdriver$client
  remDr$navigate(url)

  Sys.sleep(5)
  print("Downloading...")
  webElem <- remDr$findElement(using = "xpath", "//a[@href='/pub/time.series/oe/oe.data.1.AllData']")
  webElem$clickElement()

  Sys.sleep(5)

  remDr$close()
  rdriver$server$stop()
  print("Download complete!")

  default_dir <- file.path("", "Users", Sys.info()[["user"]], "Downloads")
  file.rename(file.path(default_dir, "oe.data.1.AllData"), file.path(destfile, "oe.data.1.AllData"))
}

get_job_data <- function(county_id, state_id, state_name, state_abreviation, county_name) {
  #' Processes job salary data for a specific county and state
  #'
  #' This function reads the job salary data file, filters it for the specified
  #' county and state, and processes it to calculate mean wages.
  #'
  #' @param county_id A string representing the county ID.
  #' @param state_id A string representing the state ID.
  #' @param state_name A string representing the name of the state.
  #' @param state_abreviation A string representing the state abbreviation.
  #' @param county_name A string representing the name of the county.
  #' @return A tibble containing processed job salary data.

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

  PLACES <- tibble::tribble(
    ~Place, ~state_code, ~area_code,
    "US", "00", "00000",
    state_abreviation, state_id, "00000",
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

  county_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "County" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean`, `Annual mean`)

  state_wages <- mean_wages_by_place |>
    dplyr::filter(Place == state_abreviation & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean`, `Annual mean`)

  us_wages <- mean_wages_by_place |>
    dplyr::filter(Place == "US" & industry_code == "000000") |>
    dplyr::select(occupation_code, `Hourly mean`, `Annual mean`)

  county_wages |>
    dplyr::inner_join(
      state_wages,
      by = "occupation_code", suffix = c(paste0(" ", county_name), paste0(" ", state_name))
    ) |>
    dplyr::inner_join(us_wages, by = "occupation_code", suffix = c(" ", " US")) |>
    invisible()
}

get_job_titles <- function(df) {
  #' Retrieves job titles for the given job data
  #'
  #' This function reads a file containing job titles and merges it with the
  #' provided job data to add job titles.
  #'
  #' @param df A dataframe containing job data.
  #' @return A dataframe with job titles added.

  print("Getting job titles...")
  result <- read.delim("DataFiles/oe.occupation", sep = "\t", header = TRUE, colClasses = "character")
  result <- result[, 1:2]
  merged_data <- merge(df, result, by.x = "occupation_code", by.y = "occupation_code", all = FALSE)
}

get_education_requirement <- function(df) {
  #' Retrieves education requirements for the given job data
  #'
  #' This function reads a file containing education requirements and merges it
  #' with the provided job data to add education requirements.
  #'
  #' @param df A dataframe containing job data.
  #' @return A dataframe with education requirements added.

  print("Getting education requirement data...")
  ed_reqs <- readr::read_csv("DataFiles/job_reques.csv", show_col_types = FALSE) |>
    dplyr::mutate(across(2, ~ gsub("-", "", .)))

  ed_reqs <- ed_reqs[2:3] |>
    dplyr::rename(occupation_code = 1, education_requirement = 2)

  df <- df |>
    dplyr::inner_join(ed_reqs, by = "occupation_code")

  return(df)
}

get_job_data_and_ed_req <- function(county_name, state_abreviation, state_name) {
  #' Retrieves job data and education requirements for a specific county and state
  #'
  #' This function retrieves job data and education requirements for the specified
  #' county and state, and writes the data to an Excel file.
  #'
  #' @param county_name A string representing the name of the county.
  #' @param state_abreviation A string representing the state abbreviation.
  #' @param state_name A string representing the name of the state.
  #' @return None. Writes the data to an Excel file.

  print("Getting job data and education requirements...")

  area_equivalence <- data.table::as.data.table(
    readxl::read_excel("DataFiles/Equivalence_table.xlsx",
      sheet = state_abreviation
    )
  ) |>
    dplyr::filter(
      .data$County == county_name
    ) |>
    dplyr::pull(.data$Area)

  place_id <- (get_place_id(area_equivalence))
  df <- get_job_data(place_id, get_state_id(state_name), state_name, state_abreviation, county_name)
  df_with_ed_req <- get_education_requirement(df)
  df_with_titles <- get_job_titles(df_with_ed_req)

  openxlsx::write.xlsx(df_with_titles, "DataFiles/OutputFiles/full_job_data.xlsx", asTable = TRUE)
  print("Job data written to full_job_data.xlsx")
}

# get_job_data_and_ed_req("Racine County", "WI")
# get_job_data_and_ed_req("Kenosha County", "WI")
# get_job_data_and_ed_req("Lake County", "IL")
