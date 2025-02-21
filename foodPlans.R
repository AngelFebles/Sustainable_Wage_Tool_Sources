library(rvest)
library(httr)
library(pdftools)
library(tabulapdf)
library(dplyr)
library(purrr)

# Just importing tidyverse raises erros, conflited is used to resolve them
library(conflicted)
library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

print("Getting Food Plans data....")
food_plans_main <- function() {
    url <- "https://www.fns.usda.gov/cnpp/usda-food-plans-cost-food-monthly-reports"
    page <- read_html(url)

    table <- page |>
        html_node("tbody") |>
        html_node("tr")

    # Link to Thrifty Food Plan
    link1 <- table |>
        html_node("a") |>
        html_attr("href") |>
        trimws()
    link_thrifty_plan <- paste0("https://www.fns.usda.gov", link1)

    # Link to Low to Lib Food Plan
    link2 <- html_nodes(table, "a")[2] |>
        html_attr("href") |>
        trimws()
    low_to_lib_plan <- paste0("https://www.fns.usda.gov", link2)

    # Download the PDF files
    download.file(link_thrifty_plan, destfile = "DataFiles/RawOutputFiles/thrifty_plan.pdf", mode = "wb")
    download.file(low_to_lib_plan, destfile = "DataFiles/RawOutputFiles/low_to_lib_plan.pdf", mode = "wb")

    # Read the PDF files
    # thrifty_pdf <- pdf_text("DataFiles/RawOutputFiles/thrifty_plan.pdf")
    # low_to_lib_pdf <- pdf_text("DataFiles/RawOutputFiles/low_to_lib_plan.pdf")

    thirty_plan <- get_thirfty_plan()
    low_to_lib <- get_low_to_lib()

    # fuse_dfs(thirty_plan, low_to_lib)

    # print(thirty_plan)
    # print(low_to_lib)

    return(fuse_dfs(thirty_plan, low_to_lib))
}

get_thirfty_plan <- function() {
    # Extract tables from the PDF file
    file1 <- "DataFiles/RawOutputFiles/thrifty_plan.pdf"
    tab <- extract_tables(file1, pages = 1)

    # Convert to tibble and clean up the data (remove headers)
    df <- as_tibble(tab[[1]]) |>
        mutate(`Age-sex group` = gsub("Individuals3\r", "", `Age-sex group`)) |>
        mutate(`Age-sex group` = gsub("Child:\\r", "", `Age-sex group`)) |>
        mutate(`Age-sex group` = gsub("Male:\\r", "", `Age-sex group`)) |>
        mutate(`Age-sex group` = gsub("Female:\\r", "", `Age-sex group`)) |>
        mutate(`Age-sex group` = gsub("Male and Female, 20-50 years\\r", "", `Age-sex group`)) |>
        mutate(`Age-sex group` = gsub("and Two Children, 6-8 and 9-11 years", "", `Age-sex group`))


    col1 <- str_split(df[[1]], pattern = "\r")
    col1 <- map(col1, ~ Filter(function(x) x != "", .x))

    col2 <- str_split(df[[2]], pattern = "\r")
    col2 <- map(col2, ~ Filter(function(x) x != "", .x))

    col3 <- str_split(df[[3]], pattern = "\r")
    col3 <- map(col3, ~ Filter(function(x) x != "", .x))

    df_clean <- tibble(
        col1 = col1,
        col2 = col2,
        col3 = col3
    ) |>
        unnest(cols = c(col1, col2, col3))

    colnames(df_clean) <- c("age_sex_group", "weekly_cost", "thrifty_monthly_cost")

    # Remove weekly cost and last row
    df_clean$weekly_cost <- NULL
    df_clean <- df_clean[-c(16), ]

    # print(df_clean)
    return(df_clean)
}

get_low_to_lib <- function() {
    # Extract tables from the PDF file
    file1 <- "DataFiles/RawOutputFiles/low_to_lib_plan.pdf"
    tab <- extract_tables(file1, pages = 1)
    # Convert to tibble and clean up the data (remove headers)
    df <- as_tibble(tab[[1]]) |>
        mutate(`...1` = gsub("Age-sex groups", NA, `...1`)) |>
        mutate(`...1` = gsub("Individuals 3", NA, `...1`)) |>
        mutate(`...1` = gsub("Child:", NA, `...1`)) |>
        mutate(`...1` = gsub("Male:", NA, `...1`)) |>
        mutate(`...1` = gsub("Female:", NA, `...1`)) |>
        mutate(`...1` = gsub("Male and Female, 20-50 years", NA, `...1`)) |>
        mutate(`...1` = gsub("and Two Children, 6-8 and 9-11 years", NA, `...1`))

    # Delete the columns for weekly cost
    df <- df[-c(2:4)]

    # Delete empty rows
    df <- df[-c(1:4, 10, 16), ]

    colnames(df) <- c("age_sex_group", "low_monthly_cost", "moderate_monthly_cost", "liberal_monthly_cost")
    # print(head(df))
    return(df)
}

fuse_dfs <- function(df1, df2) {
    df2 <- df2[-c(1)]
    df3 <- bind_cols(df1, df2)

    age_list <- c("Infant", "Preschooler", "Preschooler", "School Age", "School Age", "School Age", "Teenager", "Adult", "Senior", "Senior", "School Age", "Teenager", "Adult", "Senior", "Senior")

    df3 <- df3 |>
        add_column(cohort = get_cohort(df3)$cohort, .after = 1) |>
        add_column(age_group = age_list, .after = 2)

    # print(df3)

    return(df3)
}


get_cohort <- function(df) {
    df$cohort <- "NA"
    for (i in seq_along(df$cohort)) {
        if (i <= 5) {
            df$cohort[i] <- "Child"
        } else if (i > 5 && i <= 10) {
            df$cohort[i] <- "Male"
        } else {
            df$cohort[i] <- "Female"
        }
    }

    return(df)
}


# print(food_plans_main())

df <- food_plans_main()
head(df)


openxlsx::write.xlsx(df, "DataFiles/OutputFiles/food_plans.xlsx", asTable = TRUE)
