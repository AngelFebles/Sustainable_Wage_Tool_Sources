print("Getting Food Plans data....")
food_plans_main <- function() {
    url <- "https://www.fns.usda.gov/cnpp/usda-food-plans-cost-food-monthly-reports"
    page <- rvest::read_html(url)

    table <- page |>
        rvest::html_node("tbody") |>
        rvest::html_node("tr")

    # Link to Thrifty Food Plan
    link1 <- table |>
        rvest::html_node("a") |>
        rvest::html_attr("href") |>
        trimws()
    link_thrifty_plan <- paste0("https://www.fns.usda.gov", link1)

    # Link to Low to Lib Food Plan
    link2 <- rvest::html_nodes(table, "a")[2] |>
        rvest::html_attr("href") |>
        trimws()
    low_to_lib_plan <- paste0("https://www.fns.usda.gov", link2)

    # Download the PDF files
    httr::GET(link_thrifty_plan, httr::write_disk("DataFiles/RawOutputFiles/thrifty_plan.pdf", overwrite = TRUE))
    httr::GET(low_to_lib_plan, httr::write_disk("DataFiles/RawOutputFiles/low_to_lib_plan.pdf", overwrite = TRUE))

    thirty_plan <- get_thirfty()
    low_to_lib <- get_low_to_lib()
    fused_df <- get_fused_dfs(thirty_plan, low_to_lib)

    return(fused_df)
}

get_thirfty <- function() {
    file1 <- "DataFiles/RawOutputFiles/thrifty_plan.pdf"
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
    df <- df[5:22, ]

    # Keep only Col for monthly costs
    df <- df[, 5]

    # Drop Headers
    # df <- df[-c(6, 12), ]

    # Drop NA rows
    df <- df[complete.cases(df), ]
    # Drop columns 1 to 4
    # df <- df[, -c(1:4)]

    # print(df[5])

    # Rename the first column to "Monthly_Cost"
    colnames(df)[1] <- "Thirfty_Monthly_Cost"

    # print(df)

    return(df)
}

get_low_to_lib <- function() {
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

    df <- df_base |>
        dplyr::bind_cols(df1) |>
        dplyr::bind_cols(df2)

    return(df)
}


# print(food_plans_main())

df <- food_plans_main()
head(df)


openxlsx::write.xlsx(df, "DataFiles/OutputFiles/food_plans.xlsx", asTable = TRUE)
