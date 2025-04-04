sss_main <- function(county_self_sufficiency_standard, state_name) {
    #' Fetches the Self Sufficiency Standard data for a specific state
    #'
    #' This function scrapes the Self Sufficiency Standard website to find the most recent
    #' Self Sufficiency Standard file link for the specified state, downloads it if not already
    #' present in the `DataFiles/RawOutputFiles` directory, and reads the file to extract data
    #' specific to the given county.
    #'
    #' @param county_self_sufficiency_standard A string representing the name of the county.
    #' @param state_name A string representing the name of the state.
    #' @return A data frame containing data specific to the county extracted from the Self Sufficiency Standard file.
    #' @export

    print("Getting Self Sufficiency Standard data....")

    sss_home_page <- sprintf("https://selfsufficiencystandard.org/%s/", state_name)
    r_soup <- httr::GET(sss_home_page)

    # Raw web data
    soup <- rvest::read_html(httr::content(r_soup, "text"))

    # The table containing all Self Sufficiency Standard files
    table_soup <- soup |> rvest::html_node("div.elementor-icon-list--layout-traditional")

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
    #' Reads and processes the Self Sufficiency Standard file
    #'
    #' This function reads the Self Sufficiency Standard Excel file, filters it for the specified
    #' county, and processes the data by renaming columns, removing unnecessary columns, and
    #' transforming specific columns to integers.
    #'
    #' @param file_path A string representing the path to the Self Sufficiency Standard file.
    #' @param county_self_sufficiency_standard A string representing the name of the county.
    #' @return A data frame containing processed data specific to the county.
    #' @export

    print("Reading the file...")
    # Read the Excel file using readxl
    df <- readxl::read_excel(file_path, sheet = "By Family") |>
        dplyr::rename_with(~ gsub("\\.", " ", .)) |>
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
            across(2:6, as.integer)
        )

    # Replace NA values with 0
    df[is.na(df)] <- 0

    print("Done!")

    return(df)
}

get_raw_self_suff_standard <- function(county_name, state_name) {
    #' Retrieves and writes the Self Sufficiency Standard data to an Excel file
    #'
    #' This function fetches the Self Sufficiency Standard data for the specified county and state,
    #' processes it, and writes the resulting data to an Excel file.
    #'
    #' @param county_name A string representing the name of the county.
    #' @param state_name A string representing the name of the state.
    #' @return None. Writes the processed data to `Raw_self_suff_standard.xlsx`.
    #' @export

    df <- sss_main(county_name, state_name)
    openxlsx::write.xlsx(df, "DataFiles/OutputFiles/Raw_self_suff_standard.xlsx", asTable = TRUE)
    print("Raw Self Sufficiency Standard data written to Raw_self_suff_standard.xlsx")
}

# Test Function
# get_raw_self_suff_standard("Racine County", "Wisconsin")
# get_raw_self_suff_standard("Lake County", "Illinois")
