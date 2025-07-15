sss_data <- "DataFiles/OutputFiles/self_suff_standard.xlsx" |>
    readxl::read_excel()
# dplyr::mutate(
# dplyr::across(
c(
    "Adult(s)",
    "Infant(s)",
    "Preschooler(s)",
    "Schoolager(s)",
    "Teenager(s)",
    "housing_type",
    "food_plan"
)
# factor
# )
# )

outputs <- c(
    "Child Care Costs",
    "Transportation Costs",
    "Health Care Costs",
    "Miscellaneous costs",
    "Broadband & Cell Phone",
    "Other Necessities",
    "Taxes",
    "Earned Income Tax Credit (-)",
    "Child Care Tax Credit (-)",
    "Child Tax Credit (-)"
)


model_formula <- . ~
    `Adult(s)` +
    `Infant(s)` +
    `Preschooler(s)` +
    `Schoolager(s)` +
    `Teenager(s)` +
    # `housing_type` +
    # `food_plan` +
    0

models <- outputs |>
    rlang::set_names() |>
    purrr::map(
        \(.output) paste(paste0("`", .output, "`"), "~ .")
    ) |>
    purrr::map(
        as.formula
    ) |>
    purrr::map(
        \(.lhs) update(.lhs, model_formula)
    ) |>
    purrr::map(
        \(.formula) lm(.formula, sss_data)
    )

model_coefficients <- models |>
    purrr::map(broom::tidy) |>
    purrr::list_rbind(names_to = "output") |>
    dplyr::select("output", "term", "estimate") |>
    tidyr::pivot_wider(names_from = "term", values_from = "estimate")


openxlsx::write.xlsx(model_coefficients, file = "DataFiles/OutputFiles/coefficients.xlsx")

# print(models$Taxes)
# print(model_coefficients$Taxes)

## broom::tidy(models[[1]])


# sss_data |>
#     ggplot2::ggplot(ggplot2::aes(
#         x = .data$`Adult(s)`,
#         y = .data$`Child Care Costs`
#     )) +
#     ggplot2::geom_jitter() +
#     ggplot2::lims(
#         x = c(0, 11),
#         y = c(0, NA)
#     ) +
#     ggplot2::theme_minimal()


### Ben's code. DONT DELETE!!!!!!!!!
# sss_data |>
#     dplyr::mutate(
#         `Little Kids` = .data$`Infant(s)` + .data$`Preschooler(s)` + .data$`Schoolager(s)`,
#         any_teenagers = .data$`Teenager(s)` > 0,
#         many_adults = .data$`Adult(s)` > 2
#     ) |>
#     ggplot2::ggplot(ggplot2::aes(
#         x = .data$`Little Kids`,
#         y = .data$`Child Care Costs`,
#         color = .data$many_adults
#     )) +
#     ggplot2::geom_jitter(
#         alpha = 0.25
#     ) +
#     ggplot2::lims(
#         x = c(0, NA),
#         y = c(0, NA)
#     ) +
#     ggplot2::facet_wrap(
#         ggplot2::vars(.data$any_teenagers)
#     ) +
#     ggplot2::theme_minimal()




### For a 3 child family, see how the number of adults affect the child care cost
# # sss_data |>
# #     dplyr::mutate(
# #         total_kids = .data$`Infant(s)` + .data$`Preschooler(s)` +
# #             .data$`Schoolager(s)` + .data$`Teenager(s)`
# #     ) |>
# #     dplyr::filter(total_kids == 3) |>
# #     ggplot2::ggplot(ggplot2::aes(
# #         x = .data$`Adult(s)`,
# #         y = .data$`Child Care Costs`
# #     )) +
# #     ggplot2::geom_jitter(width = 0.2, alpha = 0.25, color = "steelblue") +
# #     ggplot2::geom_smooth(method = "loess", se = FALSE, color = "darkred") +
# #     ggplot2::labs(
# #         x = "Number of Adults",
# #         y = "Child Care Costs"
# #     ) +
# #     ggplot2::theme_minimal()



### Does having teens reduce Child Care Costs?
### When little kids == 3
# sss_data |>
#     dplyr::mutate(
#         total_kids = .data$`Infant(s)` + .data$`Preschooler(s)` + .data$`Schoolager(s)`,
#         has_teen = .data$`Teenager(s)` > 0
#     ) |>
#     dplyr::filter(total_kids == 3) |>
#     ggplot2::ggplot(ggplot2::aes(
#         x = has_teen,
#         y = .data$`Child Care Costs`
#     )) +
#     ggplot2::geom_boxplot() +
#     ggplot2::labs(
#         x = "Has Teenager(s)",
#         y = "Child Care Costs"
#     ) +
#     ggplot2::theme_minimal()


### 3 childs 2 adults

# sss_data |>
#     dplyr::mutate(
#         total_kids = .data$`Infant(s)` + .data$`Preschooler(s)` + .data$`Schoolager(s)` + .data$`Teenager(s)`
#     ) |>
#     dplyr::filter(total_kids == 3) |>
#     ggplot2::ggplot(ggplot2::aes(
#         x = .data$`Adult(s)`,
#         y = .data$`Child Care Costs`
#     )) +
#     ggplot2::geom_jitter(width = 0.2, alpha = 0.25, color = "steelblue") +
#     ggplot2::geom_smooth(method = "loess", se = FALSE, color = "darkred") +
#     ggplot2::labs(
#         x = "Number of Adults",
#         y = "Child Care Costs"
#     ) +
#     ggplot2::theme_minimal()


#### Plot absolute effect sizes
# Standardize numeric inputs
# sss_scaled <- sss_data |>
#     dplyr::mutate(
#         dplyr::across(
#             c(`Adult(s)`, `Infant(s)`, `Preschooler(s)`, `Schoolager(s)`, `Teenager(s)`),
#             scale
#         )
#     )

# model_scaled <- stats::lm(
#     `Child Care Costs` ~ `Adult(s)` + `Infant(s)` + `Preschooler(s)` +
#         `Schoolager(s)` + `Teenager(s)` +
#         housing_type + food_plan,
#     data = sss_scaled
# )

# broom::tidy(model_scaled) |>
#     dplyr::filter(term != "(Intercept)") |>
#     dplyr::mutate(term = forcats::fct_reorder(term, abs(estimate))) |>
#     ggplot2::ggplot(ggplot2::aes(x = term, y = estimate)) +
#     ggplot2::geom_col(fill = "steelblue") +
#     ggplot2::coord_flip() +
#     ggplot2::labs(
#         title = "Standardized Effect Sizes on Child Care Costs",
#         x = "Predictor",
#         y = "Effect on Child Care Costs (standardized)"
#     ) +
#     ggplot2::theme_minimal()


## Lines for each group
# dplyr::filter(.data$`Adult(s)` == 2) |>

# ### Extract coefficients for each age group from the interaction model
# child_lm <- child_summary |>
#     dplyr::filter(
#         .data$`child_age_group` != "Adult(s)"
#     ) |>
#     lm(
#         formula = `Child Care Costs` ~ count * child_age_group,
#         data = _
#     )

# ## Tidy up the coefficients for easy viewing
# coeff_table <- broom::tidy(child_lm) |>
#     dplyr::select(term, estimate, std.error, statistic, p.value)

# print(coeff_table)




# ### Infants only version

# infant_summary <- sss_data |>
#     dplyr::filter(
#         dplyr::if_all(
#             c("Preschooler(s)", "Schoolager(s)", "Teenager(s)"),
#             \(.) . == 0
#         )
#     ) |>
#     dplyr::summarise(
#         avg_cost = base::mean(.data$`Child Care Tax Credit (-)`, na.rm = TRUE),
#         .by = c("Infant(s)", "Adult(s)")
#     )
# infant_summary |>
#     ggplot2::ggplot(ggplot2::aes(
#         x = .data$`Infant(s)`,
#         y = .data$avg_cost,
#         color = ordered(.data$`Adult(s)`)
#     )) +
#     ggplot2::geom_line(linewidth = 1.2) +
#     ggplot2::geom_point() +
#     ggplot2::labs(
#         x = "No. of Infants",
#         y = "Child care tax credit (-)",
#         color = "No. of Adults"
#     ) +
#     ggplot2::scale_x_continuous(breaks = scales::breaks_width(width = 1)) +
#     ggplot2::facet_wrap(ggplot2::vars(.data$`Adult(s)`)) +
#     ggplot2::theme_minimal()




## Ben's thing
child_long <- sss_data |>
    dplyr::mutate(
        total_kids = .data$`Infant(s)` + .data$`Preschooler(s)` + .data$`Schoolager(s)` + .data$`Teenager(s)`
    ) |>
    tidyr::pivot_longer(
        cols = c(`Infant(s)`, `Preschooler(s)`, `Schoolager(s)`, `Teenager(s)`, `Adult(s)`),
        names_to = "child_age_group",
        values_to = "count"
    ) |>
    dplyr::filter(
        (total_kids == .data$`count` & .data$`child_age_group` != "Adult(s)") |
            (total_kids == 0 & .data$`child_age_group` == "Adult(s)")
    )

child_summary <- child_long |>
    dplyr::summarise(
        dplyr::across(
            c(
                "Child Care Costs",
                "Earned Income Tax Credit (-)",
                "Child Care Tax Credit (-)",
                "Child Tax Credit (-)"
            ),
            \(.) mean(., na.rm = TRUE)
        ),
        .by = c("child_age_group", "count")
    )

plot_child_summary <- function(.child_summary, .y_field) {
    .child_summary |>
        ggplot2::ggplot(ggplot2::aes(
            x = .data$count,
            y = .data[[.y_field]],
            color = .data$child_age_group
        )) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_point() +
        ggplot2::labs(
            x = "No. of people in Age Group",
            y = paste("Average", .y_field),
            color = "Age Group"
        ) +
        ggplot2::scale_x_continuous(breaks = scales::breaks_width(width = 1)) +
        ggplot2::theme_minimal()
}

plot_child_summary(child_summary, "Child Care Costs")

child_summary |>
    dplyr::filter(
        .data$`child_age_group` != "Adult(s)"
    ) |>
    lm(
        formula = `Child Care Costs` ~ count * child_age_group,
        data = _
    ) |>
    summary()




# simmilar to what Ben did above we fit model to age groups
child_lm_models <- child_summary |>
    # dplyr::filter(
    #     child_age_group != "Adult(s)"
    # ) |>
    # dplyr::group_by(
    #     child_age_group
    # ) |>
    tidyr::nest(
        data = -child_age_group
    ) |>
    dplyr::mutate(
        model = purrr::map(
            data, ~ lm(`Child Care Costs` ~ count, data = .x)
        ),
        tidy = purrr::map(
            model, broom::tidy
        )
    )

# extrac and print coefficient for each group
child_lm_coeffs <- child_lm_models |>
    dplyr::select(child_age_group, tidy) |>
    tidyr::unnest(tidy)

print(child_lm_coeffs)
