get_sss <- function(raw_sss, housing_df, food_df) {
    # # Adding the titles for the food plans
    # food_plan <- c(
    #     "Thrifty",
    #     "Low",
    #     "Moderate",
    #     "Liberal"
    # )


    # Add housing costs to df
    df_with_housing <- tidyr::crossing(
        raw_sss,
        housing_df
    )

    df_with_all_values <- food_cost_test(df_with_housing, food_df)

    final_df <- get_monthly_cost(df_with_all_values)


    return(final_df)
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


food_cost_test <- function(sss_df, food_plans_df) {
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
            cols = tidyselect::all_of(c("Adult(s)", "Infant(s)", "Preschooler(s)", "Schoolager(s)", "Teenager(s)")),
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
    # print(long_meal_costs_df)

    costs_by_family_and_plan <- long_family_sizes_df |>
        dplyr::mutate(age_group = dplyr::recode(
            age_group,
            "Adult(s)" = "Adult",
            "Infant(s)" = "Infant",
            "Preschooler(s)" = "Preschooler",
            "Schoolager(s)" = "School Age",
            "Teenager(s)" = "Teenager"
        )) |>
        dplyr::inner_join(
            long_meal_costs_df,
            by = c("food_plan", "age_group")
        ) |>
        dplyr::mutate(
            # Calculate family size as the sum of number_of_people across all age groups
            family_size = sum(.data$number_of_people)
        ) |>
        dplyr::summarize(
            total_cost = sum(.data$cost_of_plan * .data$number_of_people),
            .by = c("family_id", "food_plan")
        )

    sss_df$food_plan_cost <- as.double(costs_by_family_and_plan$total_cost)

    # print(colnames(sss_df))
    return(sss_df)

    # print(costs_by_family_and_plan$total_cost)
}

get_final_self_suff_standard <- function() {
    food_df <- readxl::read_excel("DataFiles/OutputFiles/food_plans_means.xlsx")
    housing_df <- readxl::read_excel("DataFiles/OutputFiles/housing_cost.xlsx")
    raw_sss <- readxl::read_excel("DataFiles/OutputFiles/Raw_self_suff_standard.xlsx")

    print("Getting monthly costs...")

    final_sss_df <- get_sss(raw_sss, housing_df, food_df)
    openxlsx::write.xlsx(final_sss_df, "DataFiles/OutputFiles/self_suff_standard.xlsx", asTable = TRUE)
    print("Monthly costs written to self_suff_standard.xlsx")
    print("Done!")
}


get_final_self_suff_standard()
