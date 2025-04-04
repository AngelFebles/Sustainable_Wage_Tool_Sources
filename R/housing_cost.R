# The file credentials.R should contain the API_KEY_HOUSING env variable
# source(("R/credentials.R"))
source(("DataFiles/credentials.R"))

housing_cost_main <- function(county_code) {
  #' Retrieves housing cost data for a specified county
  #'
  #' This function makes an API request to the HUD website to retrieve the most recent
  #' housing cost data for a specified county using its FIPS code.
  #'
  #' @param county_code A string representing the FIPS code of the county.
  #' @return A tibble containing housing cost data for different housing types (e.g., Efficiency, One-Bedroom).
  #'         Returns `NULL` if the API request fails.
  #' @export

  # URL with prices for All Bedroom Sizes for the specified county
  url <- paste0("https://www.huduser.gov/hudapi/public/fmr/data/", county_code)

  # Set Authorization header
  headers <- httr::add_headers(
    Authorization = paste("Bearer", Sys.getenv("API_KEY_HOUSING"))
  )

  # Make the GET request
  print("Getting Housing Cost data....")
  response <- httr::GET(url, headers)

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data$basicdata

    # If there's a single output
    if (class(data) == "list") {
      means <- data.frame(
        Efficiency = data$Efficiency,
        One_Bedroom = data$`One-Bedroom`,
        Two_Bedroom = data$`Two-Bedroom`,
        Three_Bedroom = data$`Three-Bedroom`,
        Four_Bedroom = data$`Four-Bedroom`
      ) |>
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "housing_type", values_to = "housing_cost")
    }

    # If there are housing costs for all zip codes in the county, calculate the mean
    if (class(data) == "data.frame") {
      means <- colMeans(data[, c(
        "Efficiency",
        "One-Bedroom",
        "Two-Bedroom",
        "Three-Bedroom",
        "Four-Bedroom"
      )], na.rm = TRUE) |>
        as.data.frame() |>
        tibble::rownames_to_column("housing_type")
      colnames(means)[2] <- "housing_cost"
    }

    return(means)
  } else {
    print(paste("Housing Cost API request failed with status code:", httr::status_code(response)))
    return(NULL)
  }
}

get_housing_cost <- function(fips_code) {
  #' Writes housing cost data to an Excel file
  #'
  #' This function retrieves housing cost data for a specified county using its FIPS code
  #' and writes the data to an Excel file.
  #'
  #' @param fips_code A string representing the FIPS code of the county.
  #' @return None. Writes the housing cost data to an Excel file named `housing_cost.xlsx`.
  #' @export

  result <- housing_cost_main(fips_code)

  if (!is.null(result)) {
    openxlsx::write.xlsx(result, "DataFiles/OutputFiles/housing_cost.xlsx", asTable = TRUE, rowNames = TRUE)
    print("Housing Cost data written to housing_cost.xlsx")
  } else {
    print("Failed to retrieve housing cost data.")
  }
}

# Test function, Racine is 5510199999
# get_housing_cost("5510199999")

# Example usage for Brown county
# get_housing_cost("1709799999")
