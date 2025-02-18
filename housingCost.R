library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# The file credentials.R should contain the API_KEY_HOUSING env variable
source(("credentials.R"))


housing_cost_main <- function(county_code) {
  # Scrapes the HUD website to retrieve
  # most recent housing cost data for a specified county.

  # URL with prices for All Bedroom Sizes for the specified county
  url <- paste0("https://www.huduser.gov/hudapi/public/fmr/data/", county_code)

  # Set Authorization header
  headers <- add_headers(
    # Breaks if <-
    Authorization = paste("Bearer", Sys.getenv("API_KEY_HOUSING"))
  )

  # Make the GET request
  print("Getting Housing Cost data....")
  response <- GET(url, headers)

  # Check if the request was successful
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data$basicdata # nolint: line_length_linter.

    # Create a data frame
    df <- data.frame(
      # Output kinda goes bad if you use <- for cols
      Efficiency = data$Efficiency,
      One_Bedroom = data$`One-Bedroom`,
      Two_Bedroom = data$`Two-Bedroom`,
      Three_Bedroom = data$`Three-Bedroom`,
      Four_Bedroom = data$`Four-Bedroom`
    )

    # Transpose df
    df_transposed <- df |>
      pivot_longer(cols = everything(), names_to = "Type", values_to = "Cost")

    print("Done!")


    return(df_transposed)
  } else {
    print(paste("Housing Cost API request failed with status code:", status_code(response))) # nolint: line_length_linter.
  }
}

# Racine is 5510199999
result <- housing_cost_main("5510199999")
print(result)
