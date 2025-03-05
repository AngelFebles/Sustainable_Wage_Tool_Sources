get_sss <- function(housing_df) {
    # Adding the titles for the food plans
    food_plan <- c(
        "Thrifty",
        "Low",
        "Moderate",
        "Liberal"
    )
    # Cross join everything
    result <- tidyr::crossing(
        raw_sss,
        housing_df,
        food_plan
    )

    return(result)
}

calculate_food_cost <- function(food_df, sss_df) {
    food_plan_cost <- array(0, dim = nrow(sss_df))

    # i <- 1

    for (i in seq_len(nrow(sss_df))) {
        # Check the food plan of the current row
        row_food_plan <- sss_df$food_plan[i]

        # Get the number of people in each age group
        adults <- sss_df$`Adult(s)`[i]
        infants <- sss_df$`Infant(s)`[i]
        preschhoolers <- sss_df$`Preschooler(s)`[i]
        schoolager <- sss_df$`Schoolager(s)`[i]
        teens <- sss_df$`Teenager(s)`[i]

        # Get adult cost
        food_cost <- food_df |>
            dplyr::filter(age_group == "Adult") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_cost * adults

        # Get infant cost
        food_cost <- food_df |>
            dplyr::filter(age_group == "Infant") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * infants

        # Get preschooler cost
        food_cost <- food_df |>
            dplyr::filter(age_group == "Preschooler") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * preschhoolers

        # Get schoolager cost
        food_cost <- food_df |>
            dplyr::filter(age_group == "School Age") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * schoolager

        # Get teenager cost
        food_cost <- food_df |>
            dplyr::filter(age_group == "Teenager") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * teens
    }
    # print(food_plan_cost)

    sss_df$food_plan_cost <- as.double(food_plan_cost)

    return(sss_df)
}

get_monthly_cost <- function(sss_df) {
    # for(i in seq_len(nrow(sss_df))) {
    sss_df$monthly_cost <- rowSums(sss_df[, c(
        "Child Care Costs",
        "Transportation Costs",
        "Health Care Costs",
        "Miscellaneous costs",
        "Broadband & Cell Phone",
        "Other Necessities",
        "Taxes",
        "Earned Income Tax Credit (-)",
        "Child Care Tax Credit (-)",
        "Child Tax Credit (-)",
        "housing_cost",
        "food_plan_cost",
        "food_plan_cost"
    )], na.rm = TRUE)

    sss_df$yearly_cost <- sss_df$monthly_cost * 12

    sss_df <- sss_df[order(sss_df$yearly_cost), ]

    return(sss_df)

    # }
}

food_df <- readxl::read_excel("DataFiles/OutputFiles/food_plans_means.xlsx")
housing_df <- readxl::read_excel("DataFiles/OutputFiles/housing_cost.xlsx")
raw_sss <- readxl::read_excel("DataFiles/OutputFiles/Raw_self_suff_standard.xlsx")



sss_with_food <- calculate_food_cost(food_df, get_sss(housing_df))

# openxlsx::write.xlsx(final_sss, "DataFiles/OutputFiles/self_suff_standard.xlsx", asTable = TRUE)


# sss <- readxl::read_excel("DataFiles/OutputFiles/self_suff_standard.xlsx")
# print(class(sss$food_plan_cost))
sss_with_budget <- get_monthly_cost(sss_with_food)
openxlsx::write.xlsx(sss_with_budget, "DataFiles/OutputFiles/self_suff_standard2.xlsx", asTable = TRUE)
print("Done!")
