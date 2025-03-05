sss_main <- function(county_self_sufficiency_standard) {
    # Fetches the Self Sufficiency Standard data from the designated website for Wisconsin.

    # The function scrapes the website to find the most recent Self Sufficiency Standard file link,
    # downloads it if not already present in the './DataFiles/' directory, and reads the file to extract data
    # specific to Racine County. The data is processed using the `read_file` function, which reads the file
    # into a data frame.

    # Returns:
    # @data_frame: A data frame with data specific to the County extracted from the Self Sufficiency Standard file.

    print("Getting Self Sufficiency Standard data....")

    sss_home_page <- "https://selfsufficiencystandard.org/Wisconsin/"
    r_soup <- httr::GET(sss_home_page)

    # raw web data
    soup <- rvest::read_html(httr::content(r_soup, "text"))

    # The table containing all Self Sufficiency Standard files
    table_soup <- soup |> rvest::html_node("div[data-id='2cc978d4']")

    # The first li element contains the most recent Self Sufficiency Standard file
    table_soup <- table_soup |> rvest::html_node("li")
    link_to_sss <- table_soup |>
        rvest::html_node("a") |>
        rvest::html_attr("href")

    # Extract the file name from the URL
    filename <- basename(link_to_sss)

    # Check if we already have that file
    file_path <- file.path("DataFiles/RawOutputFiles", filename)

    # If not, download it
    if (!file.exists(file_path)) {
        print(paste("Downloading", filename, "..."))
        response <- httr::GET(link_to_sss)
        writeBin(httr::content(response, "raw"), file_path)
        print("Download complete!")
    } else {
        print(paste(filename, "already downloaded."))
    }

    return(read_file(file_path, county_self_sufficiency_standard))
}

read_file <- function(file_path, county_self_sufficiency_standard) {
    print("Reading the file...")
    # Read the Excel file using readxl
    df <- readxl::read_excel(file_path, sheet = "By Family") |>
        dplyr::filter(County == county_self_sufficiency_standard) |>
        dplyr::select(-one_of( # We are gonna add our own food/housing costs so their SSS are not needed
            c(
                "Housing Costs",
                "Food Costs",
                "Emergency Savings",
                "Hourly Self-Sufficiency Wage",
                "Monthly Self-Sufficiency Wage",
                "Annual Self-Sufficiency Wage"
            )
        ))

    # Transform the columns 'Adults', 'Infants', 'Preschoolers', 'Schoolagers', 'Teenagers' to integers
    df <- df |>
        dplyr::mutate(
            `Adult(s)` = as.integer(`Adult(s)`),
            `Infant(s)` = as.integer(`Infant(s)`),
            `Preschooler(s)` = as.integer(`Preschooler(s)`),
            `Schoolager(s)` = as.integer(`Schoolager(s)`),
            `Teenager(s)` = as.integer(`Teenager(s)`)
        )

    # Replace NA values with 0
    df[is.na(df)] <- 0

    print("Done!")

    return(df)
}

# Racine County

df <- sss_main("Racine County")
openxlsx::write.xlsx(df, "DataFiles/OutputFiles/Raw_self_suff_standard.xlsx", asTable = TRUE)

# print(df)
