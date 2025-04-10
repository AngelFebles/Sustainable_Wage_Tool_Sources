main <- function(state, county) {
    #' main.R
    #'
    #' Main script to generate self-sufficiency data and related outputs
    #'
    #' This script orchestrates the execution of various modules to generate self-sufficiency
    #' data for a specified state and county. It performs the following tasks:
    #' - Retrieves job data and education requirements.
    #' - Processes food plans and housing costs.
    #' - Fetches and processes the Self Sufficiency Standard data.
    #' - Combines all data to calculate final self-sufficiency costs.
    #'
    #' @note This script does not support Alaska or Hawaii.
    #' @param state A string representing the name of the state (e.g., "Wisconsin").s
    #' @param county A string representing the name of the county (e.g., "Racine").
    #' @return None. Outputs are written to Excel files in the `DataFiles/OutputFiles` directory.
    #' @export


    # # Can't do Alaska or Hawaii
    # state <- "Wisconsin"

    # # Don't add ' County' at the end
    # # Ex: county <- "Brown", county <- "Racine"
    # county <- "Racine"

    # Generate the FIPS code for the county
    fips_code <- readxl::read_xls("DataFiles/US_FIPS_Codes.xls") |>
        dplyr::filter(State == state, `County Name` == county)

    # The code for the county needs to be 0 padded for 3 digits
    fips_code["FIPS County"] <- sprintf("%03d", dplyr::pull(fips_code, `FIPS County`))

    fips_code <- dplyr::select(fips_code, `FIPS State`, `FIPS County`) |>
        paste0(collapse = "") |> # We join the codes for State and County
        paste0("99999") # Appending 99999 at the end is required by the source

    # Retrieve the state abbreviation
    state_abreviation <- readxl::read_xls("DataFiles/US_State_Abbreviations.xls") |>
        dplyr::filter(State == state) |>
        dplyr::pull(Abreviation)

    # Generate the compound county name
    compound_county_name <- paste0(county, ", ", state_abreviation)

    # Step 1: Retrieve job data and education requirements
    # source("R/job_data.R")
    get_job_data_and_ed_req(paste(county, "County"), state_abreviation, state)

    # Step 2: Process food plans
    # source("R/food_plans.R")
    get_food_plans()

    # Step 3: Retrieve housing cost data
    # source("R/housing_cost.R")
    get_housing_cost(fips_code)

    # Step 4: Fetch and process the Self Sufficiency Standard data
    # source("R/raw_self_suff_standard.R")
    get_raw_self_suff_standard(paste(county, "County"), state)

    # Step 5: Combine all data to calculate final self-sufficiency costs
    # source("R/self_suff_standard.R")
    get_final_self_suff_standard()

    # Optional: Retrieve the top 5 jobs based on target salary
    # source("R/binary_search.R")
    # get_5_jobs(county)
}

# main("Wisconsin", "Racine")
