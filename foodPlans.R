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


food_plans_main <- function() {
    print("Getting Food Plans data....")

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



    # df_clean <- df |> separate_longer_delim(1, delim = "\r")

    col1 <- str_split(df[[1]], pattern = "\r")
    col1 <- map(col1, ~ Filter(function(x) x != "", .x))

    print(col1)

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

    colnames(df_clean) <- c("age_sex_group", "weekly_cost", "monthlt_cost")


    print(df_clean)
}

food_plans_main()
