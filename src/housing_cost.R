# The file credentials.R should contain the API_KEY_HOUSING env variable
source(("src/credentials.R"))

housing_cost_main <- function(county_code) {
  #' Scrapes the HUD website to retrieve
  #' most recent housing cost data for a specified county.

  # URL with prices for All Bedroom Sizes for the specified county
  url <- paste0("https://www.huduser.gov/hudapi/public/fmr/data/", county_code)

  # Set Authorization header
  headers <- httr::add_headers(
    # Breaks if <-
    Authorization = paste("Bearer", Sys.getenv("API_KEY_HOUSING"))
  )

  # Make the GET request
  print("Getting Housing Cost data....")
  response <- httr::GET(url, headers)

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$data$basicdata # nolint: line_length_linter.
    # print(class(data))
    # data <- data.frame(data)

    # If threres a single output
    if (class(data) == "list") {
      means <- data.frame(
        # Output kinda goes bad if you use <- for cols
        Efficiency = data$Efficiency,
        One_Bedroom = data$`One-Bedroom`,
        Two_Bedroom = data$`Two-Bedroom`,
        Three_Bedroom = data$`Three-Bedroom`,
        Four_Bedroom = data$`Four-Bedroom`
      ) |>
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "housing_type", values_to = "housing_cost")
    }

    # Sometimes there's housing costs for all zip codes
    # in the county, when that hapends we need to calculate mean of all
    if (class(data) == "data.frame") {
      # Calculate the means of the columns
      means <- colMeans(data[, c(
        "Efficiency",
        "One-Bedroom",
        "Two-Bedroom",
        "Three-Bedroom",
        "Four-Bedroom"
      )], na.rm = TRUE) |>
        as.data.frame() |>
        tibble::rownames_to_column("housing_type")
      colnames(means)[2] <- c("housing_cost")
    }



    # print(means)
    # Create a data frame
    # df <- data.frame(
    #   # Output kinda goes bad if you use <- for cols
    #   Efficiency = data$Efficiency,
    #   One_Bedroom = data$`One-Bedroom`,
    #   Two_Bedroom = data$`Two-Bedroom`,
    #   Three_Bedroom = data$`Three-Bedroom`,
    #   Four_Bedroom = data$`Four-Bedroom`
    # )

    # Transpose df
    # df_transposed <- means |>
    #   tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Type", values_to = "Cost")

    # print("Done!")

    return(means)
  } else {
    print(paste("Housing Cost API request failed with status code:", httr::status_code(response))) # nolint: line_length_linter.
  }
}

get_housing_cost <- function(fips_code) {
  # result <- housing_cost_main("5510199999")
  # result <- housing_cost_main("5500999999")
  # print(fips_code)
  result <- housing_cost_main(fips_code)
  # print(result)
  # Rename columns
  openxlsx::write.xlsx(result, "DataFiles/OutputFiles/housing_cost.xlsx", asTable = TRUE, rowNames = TRUE)

  print("House Cost data written to housing_cost.xlsx")
  # print(result)
}

# Test function, Racine is 5510199999
# get_housing_cost("5510199999")


# get_housing_cost("1709799999")

# number <- 123 |> sprintf("%05d")
# print(formatted_number)
