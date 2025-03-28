# To print all columns

get_jobs <- function(target, county) {
    options(dplyr.width = Inf)

    county_hourly_string <- paste0("Hourly mean ", county, " County")
    county_yearly_string <- paste0("Annual mean ", county, " County")

    full_job_data <- readxl::read_excel("DataFiles/OutputFiles/full_job_data.xlsx")

    full_job_data <- full_job_data[, c("occupation_name", county_hourly_string, county_yearly_string, "education_requirement")] # nolint
    # Remove rows where 'Hourly mean County' is NA
    full_job_data <- full_job_data[!is.na(full_job_data[[county_hourly_string]]), ]

    # Sort the data by 'Hourly mean County'
    x <- full_job_data[!is.na(full_job_data[[county_yearly_string]]), ]
    x <- x[order(x[[county_yearly_string]]), ]



    # A little more realistic (index 4316 [One room, moderate food] goes to 28k)
    # target <- 30000


    # print(head(job_df))

    # All jobs greater than target
    # indexes1 <- seq(index, length(x$`Annual mean County`))

    # Only first 5 jobs
    index <- findInterval(target, x[[county_yearly_string]]) + 1
    indexes2 <- seq(index, min(index + 4, length(x[[county_yearly_string]])))

    # print("Index of first job")
    # print(index)
    print(x[indexes2, ])


    # target_plus_ten_percent <- target + (target * 0.1)
    # print(target_plus_ten_percent)
}


get_target_by_index <- function(index) {
    job_df <- readxl::read_excel("DataFiles/OutputFiles/self_suff_standard.xlsx")

    # target <- job_df[1]
    target <- job_df$yearly_cost[index]
    print("Target yearly salary:")
    return(target)
}

get_target_table_cols <- function() {
    # Change the parameters in this funciton to filter a specific job requirement
    # The 3 parameters are: Family Type, Housing Type, and Food Plan

    # Family size
    no_of_adults <- 2
    no_of_infants <- 0
    no_of_preschoolers <- 0
    no_of_schoolagers <- 0
    no_of_teenagers <- 0

    family_type <- paste0(
        "a", no_of_adults,
        "i", no_of_infants,
        "p", no_of_preschoolers,
        "s", no_of_schoolagers,
        "t", no_of_teenagers
    )

    # Food Plan
    # Options: Thrifty , Low , Moderate , Liberal
    food_plan <- "Moderate"


    # Housing Type
    # Options: Efficiency , One_Bedroom , Two_Bedroom , Three_Bedroom
    housing_plan <- "Two_Bedroom"

    job_df <- readxl::read_excel("DataFiles/OutputFiles/self_suff_standard.xlsx")

    index <- which(
        job_df$`Family Type` == family_type &
            job_df$housing_type == housing_plan &
            job_df$food_plan == food_plan
    )
    print("Index of job")
    print(index)
    print(job_df$yearly_cost[index])

    target <- job_df$yearly_cost[index]


    return(target)
}


get_5_jobs <- function(county) {
    target <- get_target_table_cols()

    get_jobs(target, county)
}

# county <- "Stearns"
# get_5_jobs(county)
