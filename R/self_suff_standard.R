get_sss <- function(raw_sss, housing_df, food_df) {
    #' Combines raw self-sufficiency data with housing and food costs
    #'
    #' This function merges the raw self-sufficiency data with housing and food cost data,
    #' calculates the monthly and yearly costs, and returns the final dataframe.
    #'
    #' @param raw_sss A dataframe containing raw self-sufficiency data.
    #' @param housing_df A dataframe containing housing cost data.
    #' @param food_df A dataframe containing food cost data.
    #' @return A dataframe containing the combined self-sufficiency data with calculated costs.
    #' @export

    df_with_housing <- tidyr::crossing(
        raw_sss,
        housing_df
    )

    df_with_all_values <- food_cost_test(df_with_housing, food_df)

    final_df <- get_monthly_cost(df_with_all_values)

    return(final_df)
}

calculate_food_cost <- function(food_df, sss_df) {
    #' Calculates food costs for each family based on age groups and food plans
    #'
    #' This function calculates the total food cost for each family in the self-sufficiency
    #' dataframe based on the number of people in each age group and the selected food plan.
    #'
    #' @param food_df A dataframe containing food cost data for different age groups and plans.
    #' @param sss_df A dataframe containing self-sufficiency data for families.
    #' @return A dataframe with an additional column for the calculated food plan cost.
    #' @export

    food_plan_cost <- array(0, dim = nrow(sss_df))

    for (i in seq_len(nrow(sss_df))) {
        row_food_plan <- sss_df$food_plan[i]

        adults <- sss_df[i, 2]
        infants <- sss_df[i, 3]
        preschhoolers <- sss_df[i, 4]
        schoolager <- sss_df[i, 5]
        teens <- sss_df[i, 6]

        food_cost <- food_df |>
            dplyr::filter(age_group == "Adult") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_cost * adults

        food_cost <- food_df |>
            dplyr::filter(age_group == "Infant") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * infants

        food_cost <- food_df |>
            dplyr::filter(age_group == "Preschooler") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * preschhoolers

        food_cost <- food_df |>
            dplyr::filter(age_group == "School Age") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * schoolager

        food_cost <- food_df |>
            dplyr::filter(age_group == "Teenager") |>
            dplyr::pull(row_food_plan)
        food_plan_cost[i] <- food_plan_cost[i] + food_cost * teens
    }

    sss_df$food_plan_cost <- as.double(food_plan_cost)

    return(sss_df)
}

get_monthly_cost <- function(sss_df) {
    #' Calculates monthly and yearly costs for each family
    #'
    #' This function calculates the total monthly and yearly costs for each family
    #' by summing up various cost components, including housing and food costs.
    #'
    #' @param sss_df A dataframe containing self-sufficiency data with cost components.
    #' @return A dataframe with additional columns for monthly and yearly costs.
    #' @export

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
        "food_plan_cost"
    )], na.rm = TRUE)

    sss_df$yearly_cost <- sss_df$monthly_cost * 12

    sss_df <- sss_df[order(sss_df$yearly_cost), ]

    return(sss_df)
}

food_cost_test <- function(sss_df, food_plans_df) {
    #' Calculates food costs for all families and food plans
    #'
    #' This function calculates the total food cost for all families in the self-sufficiency
    #' dataframe across all food plans by matching age groups and food plan costs.
    #'
    #' @param sss_df A dataframe containing self-sufficiency data for families.
    #' @param food_plans_df A dataframe containing food cost data for different age groups and plans.
    #' @return A dataframe with an additional column for the calculated food plan cost.
    #' @export

    food_plan <- c(
        "Thrifty",
        "Low",
        "Moderate",
        "Liberal"
    )

    sss_df <- tidyr::crossing(
        sss_df,
        food_plan
    )

    long_family_sizes_df <- sss_df |>
        dplyr::mutate(
            family_id = dplyr::row_number()
        ) |>
        tidyr::pivot_longer(
            cols = tidyselect::all_of(names(sss_df)[2:6]),
            names_to = "age_group",
            values_to = "number_of_people"
        )

    long_meal_costs_df <- food_plans_df |>
        dplyr::mutate(
            plan_id = dplyr::row_number()
        ) |>
        tidyr::pivot_longer(
            cols = !tidyselect::all_of(c("age_group", "plan_id")),
            names_to = "food_plan",
            values_to = "cost_of_plan"
        )

    age_group_names <- names(sss_df)[2:6]

    costs_by_family_and_plan <- long_family_sizes_df |>
        dplyr::mutate(age_group = dplyr::case_when(
            age_group == age_group_names[1] ~ "Adult",
            age_group == age_group_names[2] ~ "Infant",
            age_group == age_group_names[3] ~ "Preschooler",
            age_group == age_group_names[4] ~ "School Age",
            age_group == age_group_names[5] ~ "Teenager",
            TRUE ~ age_group
        )) |>
        dplyr::inner_join(
            long_meal_costs_df,
            by = c("food_plan", "age_group")
        ) |>
        dplyr::summarize(
            total_cost = sum(.data$cost_of_plan * .data$number_of_people),
            .by = c("family_id", "food_plan")
        )

    sss_df$food_plan_cost <- as.double(costs_by_family_and_plan$total_cost)

    return(sss_df)
}

get_final_self_suff_standard <- function() {
    #' Generates the final Self Sufficiency Standard data
    #'
    #' This function combines raw self-sufficiency data with housing and food costs,
    #' calculates monthly and yearly costs, and writes the final data to an Excel file.
    #'
    #' @return None. Writes the final self-sufficiency data to `self_suff_standard.xlsx`.
    #' @export

    food_df <- readxl::read_excel("DataFiles/OutputFiles/food_plans_means.xlsx")
    housing_df <- readxl::read_excel("DataFiles/OutputFiles/housing_cost.xlsx")
    raw_sss <- readxl::read_excel("DataFiles/OutputFiles/Raw_self_suff_standard.xlsx")

    print("Getting monthly costs...")

    final_sss_df <- get_sss(raw_sss, housing_df, food_df)
    openxlsx::write.xlsx(final_sss_df, "DataFiles/OutputFiles/self_suff_standard.xlsx", asTable = TRUE)
    print("Monthly costs written to self_suff_standard.xlsx")
    print("Done!")
}

# get_final_self_suff_standard()
