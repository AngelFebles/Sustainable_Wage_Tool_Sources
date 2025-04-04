food_plans_main <- function() {
    #' Fetches and processes the Food Plans data
    #'
    #' This function scrapes the USDA Food Plans website, downloads the relevant PDF files,
    #' and processes the data to extract monthly cost information for different food plans.
    #'
    #' @return A fused dataframe containing processed data from the Thrifty and Low to Liberal Food Plans.
    #' #' @export

    page <- rvest::read_html("https://www.fns.usda.gov/cnpp/usda-food-plans-cost-food-monthly-reports") |>
        # table <- page |>
        rvest::html_node("tbody") |>
        rvest::html_node("tr")

    # Link to Thrifty Food Plan
    link_thrifty_plan <- page |>
        rvest::html_node("a") |>
        rvest::html_attr("href") |>
        trimws() |>
        (\(url) ifelse(startsWith(url, "http"), url, paste0("https://www.fns.usda.gov", url)))()


    # Link to Low to Lib Food Plan
    low_to_lib_plan <- rvest::html_nodes(page, "a")[2] |>
        rvest::html_attr("href") |>
        trimws() |>
        (\(url) ifelse(startsWith(url, "http"), url, paste0("https://www.fns.usda.gov", url)))()

    # Download the PDF files
    httr::GET(link_thrifty_plan, httr::write_disk("DataFiles/RawOutputFiles/thrifty_plan.pdf", overwrite = TRUE))
    httr::GET(low_to_lib_plan, httr::write_disk("DataFiles/RawOutputFiles/low_to_lib_plan.pdf", overwrite = TRUE))

    get_fused_dfs(get_thrifty(), get_low_to_lib())

    # return(fused_df)
}

get_thrifty <- function() {
    #' Processes the Thrifty Food Plan PDF
    #'
    #' This function extracts and processes data from the Thrifty Food Plan PDF.
    #'
    #' @return A tibble containing the monthly cost data for the Thrifty Food Plan.
    #' @export


    pdf_lines <- pdftools::pdf_text("DataFiles/RawOutputFiles/thrifty_plan.pdf") |>
        strsplit("\n") |> # Split the text into lines
        lapply(function(page) { # The PDF has a table structure that can be read line by line
            page_data <- strsplit(page, "\\s+")
            max_length <- max(sapply(page_data, length))
            page_data <- lapply(page_data, function(row) {
                length(row) <- max_length
                row
            })
            do.call(rbind, page_data)
        })

    # Convert to a tibble for easier manipulation
    df <- tibble::as_tibble(do.call(rbind, pdf_lines), .name_repair = "unique")

    # Extract data rows
    df <- df[5:22, ]

    # Keep only Col for monthly costs
    df <- df[, 5]

    # Drop NA rows
    df <- df[complete.cases(df), ]
    # Drop columns 1 to 4


    # Rename the first column to "Monthly_Cost"
    colnames(df)[1] <- "thrifty_Monthly_Cost"

    df

    # return(df)
}

get_low_to_lib <- function() {
    #' Processes the Low to Liberal Food Plan PDF
    #'
    #' This function extracts and processes data from the Low to Liberal Food Plan PDF.
    #'
    #' @return A tibble containing the monthly cost data for the Low, Moderate, and Liberal Food Plans.
    #' @export

    file1 <- "DataFiles/RawOutputFiles/low_to_lib_plan.pdf"
    pdf_text <- pdftools::pdf_text(file1)

    # Split the text into lines
    pdf_lines <- strsplit(pdf_text, "\n")

    # The PDF has a table structure that can be read line by line
    data <- lapply(pdf_lines, function(page) {
        page_data <- strsplit(page, "\\s+")
        max_length <- max(sapply(page_data, length))
        page_data <- lapply(page_data, function(row) {
            length(row) <- max_length
            row
        })
        do.call(rbind, page_data)
    })

    # Convert to a tibble for easier manipulation
    df <- tibble::as_tibble(do.call(rbind, data), .name_repair = "unique")

    # Extract data rows
    df <- df[7:25, ]

    # Keep only Col for monthly costs
    df <- df[, c(5, 7, 9)]

    # Drop Headers
    # df <- df[-c(6, 12), ]

    # Drop NA rows
    df <- df[complete.cases(df), ]
    # Drop columns 1 to 4
    # df <- df[, -c(1:4)]

    colnames(df)[1] <- "Low_Monthly_Cost"
    colnames(df)[2] <- "Moderate_Monthly_Cost"
    colnames(df)[3] <- "Liberal_Monthly_Cost"

    return(df)
}

get_fused_dfs <- function(df1, df2) {
    #' Combines data from the Thrifty and Low to Liberal Food Plans
    #'
    #' This function fuses the data from the Thrifty Food Plan and the Low to Liberal Food Plan
    #' into a single dataframe with additional metadata.
    #'
    #' @param df1 A tibble containing Thrifty Food Plan data.
    #' @param df2 A tibble containing Low to Liberal Food Plan data.
    #' @return A tibble containing the combined data.
    #' @export

    age_sex_group <- c(
        "1 year", "2-3 years", "4-5 years", "6-8 years", "9-11 years",
        "12-13 years", "14-18 years", "19-50 years", "51-70 years", "71+ years",
        "12-13 years", "14-18 years", "19-50 years", "51-70 years", "71+ years"
    )

    cohort <- c(
        "Child", "Child", "Child", "Child", "Child",
        "Female", "Female", "Female", "Female", "Female",
        "Male", "Male", "Male", "Male", "Male"
    )

    age_group <- c(
        "Infant", "Preschooler", "Preschooler", "School Age", "School Age",
        "School Age", "Teenager", "Adult", "Senior", "Senior",
        "School Age", "Teenager", "Adult", "Senior", "Senior"
    )

    df_base <- tibble::tibble(age_sex_group, cohort, age_group)

    # print(df_base)

    # Ensure all data frames have the same number of rows
    min_rows <- min(nrow(df_base), nrow(df1), nrow(df2))
    df_base <- df_base[1:min_rows, ]
    df1 <- df1[1:min_rows, ]
    df2 <- df2[1:min_rows, ]

    df <- df_base |>
        dplyr::bind_cols(df1) |>
        dplyr::bind_cols(df2)

    df <- df |>
        dplyr::mutate(
            across(contains("Monthly_Cost"), ~ as.numeric(gsub("\\$", "", .)))
        )

    return(df)
}

get_food_plans <- function() {
    #' Generates summarized Food Plans data
    #'
    #' This function calculates the mean monthly costs for each food plan by age group
    #' and writes the summarized data to an Excel file.
    #'
    #' @return None. Writes the summarized data to an Excel file.
    #' @export

    print("Getting Food Plans data....")
    df <- food_plans_main()
    # head(df)

    df_grouped <- df |>
        dplyr::group_by(age_group) |>
        dplyr::summarise(
            Thrifty = mean(thrifty_Monthly_Cost),
            Low = mean(Low_Monthly_Cost),
            Moderate = mean(Moderate_Monthly_Cost),
            Liberal = mean(Liberal_Monthly_Cost)
        )

    # print(df_grouped)


    openxlsx::write.xlsx(df_grouped, "DataFiles/OutputFiles/food_plans_means.xlsx", asTable = TRUE)

    print("Food Plans data written to food_plans_means.xlsx")
}

# get_food_plans()
