# Can't do Alaska or Hawaii
state <- "Minnesota"

# Don't add ' County' at the end
# Ex: county <- "Brown", county <- "Racine"
county <- "Stearns"

fips_code <- readxl::read_xls("DataFiles/US_FIPS_Codes.xls") |>
    dplyr::filter(State == state, `County Name` == county)

# The code for the county needs to be 0 padded for 3 digits
fips_code["FIPS County"] <- sprintf("%03d", dplyr::pull(fips_code, `FIPS County`))

fips_code <- dplyr::select(fips_code, `FIPS State`, `FIPS County`) |>
    paste0(collapse = "") |> # We join the codes for State and County
    paste0("99999") # Appending 99999 at the end is required by the source




# print(fips_code)

state_abreviation <- readxl::read_xls("DataFiles/US_State_Abbreviations.xls") |>
    dplyr::filter(State == state) |>
    dplyr::pull(Abreviation)

# print(state_abreviation)

compound_county_name <- paste0(county, ", ", state_abreviation)

# print(compound_county_name)


source("src/job_data.R")
get_job_data_and_ed_req(paste(county, "County"), state_abreviation, state) # compound_county_name

source("src/food_plans.R")
get_food_plans()

source("src/housing_cost.R")
get_housing_cost(fips_code) # fips_code
# get_job_data_and_ed_req(compound_county_name) # compound_county_name


source("src/raw_self_suff_standard.R")
get_raw_self_suff_standard(paste(county, "County"), state) # county name + " county"

source("src/self_suff_standard.R")
get_final_self_suff_standard()

# source("src/binary_search.R")
# get_5_jobs(county)
