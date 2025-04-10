get_jobs <- function(target, county) {
    #' Retrieves jobs with salaries greater than or near the target salary
    #'
    #' This function filters job data for a specific county to find jobs with annual
    #' salaries greater than or near the specified target salary. It returns the top 5
    #' jobs closest to the target salary.
    #'
    #' @param target A numeric value representing the target annual salary.
    #' @param county A string representing the name of the county.
    #' @return None. Prints the top 5 jobs with salaries near or above the target.

    options(dplyr.width = Inf)

    county_hourly_string <- paste0("Hourly mean ", county, " County")
    county_yearly_string <- paste0("Annual mean ", county, " County")

    full_job_data <- readxl::read_excel("DataFiles/OutputFiles/full_job_data.xlsx")

    full_job_data <- full_job_data[, c("occupation_name", county_hourly_string, county_yearly_string, "education_requirement")] # nolint
    # Remove rows where 'Hourly mean County' is NA
    full_job_data <- full_job_data[!is.na(full_job_data[[county_hourly_string]]), ]

    # Sort the data by 'Annual mean County'
    x <- full_job_data[!is.na(full_job_data[[county_yearly_string]]), ]
    x <- x[order(x[[county_yearly_string]]), ]

    # Find the index of the first job with a salary greater than or near the target
    index <- findInterval(target, x[[county_yearly_string]]) + 1
    indexes2 <- seq(index, min(index + 4, length(x[[county_yearly_string]])))

    # Print the top 5 jobs
    print(x[indexes2, ])
}

get_target_by_index <- function(index) {
    #' Retrieves the target yearly salary for a specific index
    #'
    #' This function retrieves the yearly cost (target salary) for a specific index
    #' from the self-sufficiency standard data.
    #'
    #' @param index An integer representing the index of the desired row in the data.
    #' @return A numeric value representing the target yearly salary.

    job_df <- readxl::read_excel("DataFiles/OutputFiles/self_suff_standard.xlsx")

    target <- job_df$yearly_cost[index]
    print("Target yearly salary:")
    return(target)
}

get_target_table_cols <- function() {
    #' Retrieves the target yearly salary based on family type, housing, and food plan
    #'
    #' This function filters the self-sufficiency standard data based on the specified
    #' family type, housing type, and food plan to retrieve the target yearly salary.
    #'
    #' @return A numeric value representing the target yearly salary.

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
    # Options: Thrifty, Low, Moderate, Liberal
    food_plan <- "Moderate"

    # Housing Type
    # Options: Efficiency, One_Bedroom, Two_Bedroom, Three_Bedroom
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
    #' Retrieves the top 5 jobs for a specific county based on target salary
    #'
    #' This function retrieves the target yearly salary based on family type, housing,
    #' and food plan, and then finds the top 5 jobs in the specified county with salaries
    #' near or above the target.
    #'
    #' @param county A string representing the name of the county.
    #' @return None. Prints the top 5 jobs with salaries near or above the target.

    target <- get_target_table_cols()

    get_jobs(target, county)
}

# Example usage:
# county <- "Stearns"
# get_5_jobs(county)
