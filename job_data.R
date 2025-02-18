library(dplyr)
library(httr)
library(rvest)

library(RSelenium)

get_county_id <- function(county) {
  # Reads the file containing all the county codes
  df <- read.delim("DataFiles/oe.area", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

  # Index 2 is the id column
  result <- df |>
    filter(area_name == county) |>
    pull(2)

  print(result)
}

download_job_salary_data <- function() {
  url <- "https://download.bls.gov/pub/time.series/oe/oe.data.1.AllData"
  #url <- "https://download.bls.gov/pub/time.series/oe/"
  destfile <- "DataFiles/RawOutputFiles"

  # Create the directory if it doesn't exist
  # dir.create("DataFiles", showWarnings = FALSE, recursive = TRUE)


  # Start Selenium with a visible Chrome window
  rdriver <- rsDriver(port= sample(7600)[1], browser=c("firefox"), chromever = NULL )
  #Sys.sleep(10)
  remDr <- rdriver$client
  remDr$navigate(url)

 default_dir <- file.path("", "Users", Sys.info()[["user"]], "Downloads")

file.rename(file.path(default_dir, "oe.data.1.AllData"), file.path(destfile, "oe.data.1.AllData"))


  # Wait for the page to load (optional)
  Sys.sleep(5)

  # Optionally, print the title of the page to confirm it's loaded
  title <- remDr$getTitle()[[1]]
  print(title)

  Sys.sleep(5)




  print("Download complete!")
}

county <- "Racine, WI"
# get_county_id(county)
download_job_salary_data()
