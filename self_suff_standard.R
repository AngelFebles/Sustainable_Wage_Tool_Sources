library(rvest)
library(httr)
library(readxl)
library(dplyr)

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
r_soup <- GET(sss_home_page)

# raw web data
soup <- read_html(content(r_soup, "text"))

# The table containing all Self Sufficiency Standard files
table_soup <- soup |> html_node("div[data-id='2cc978d4']")

# The first li element contains the most recent Self Sufficiency Standard file
table_soup <- table_soup |> html_node("li")
link_to_sss <- table_soup |> html_node("a") |> html_attr("href")

# Extract the file name from the URL
filename <- basename(link_to_sss)

# Create the directory if it doesn't exist
dir.create("src/sustainable_wage_tool_data/DataFiles", showWarnings = FALSE, recursive = TRUE)

# Check if we already have that file
file_path <- file.path("src/sustainable_wage_tool_data/DataFiles", filename)

# If not, download it
if (!file.exists(file_path)) {
print(paste("Downloading", filename, "..."))
response <- GET(link_to_sss)
writeBin(content(response, "raw"), file_path)
print("Download complete!")
} else {
print(paste(filename, "already downloaded."))
}

return(read_file(file_path, county_self_sufficiency_standard))
}

read_file <- function(file_path, county_self_sufficiency_standard) {

# Reads an Excel file using readxl and extracts the Self Sufficiency Standard data.

# Parameters:
# file_path (str): The path to the Excel file.


print("Reading the file...")
# Read the Excel file using readxl
df <- read_excel(file_path, sheet = "By Family")

# Get the id of the row that contains the county, Racine by default
county <- county_self_sufficiency_standard

county_row <- df |> filter(str_detect(.[[11]], county))
county_index <- which(df[[11]] %in% county_row[[11]])[1]

# Theres 719 rows for each county, thats where the upperbound comes from
data_frame <- df[county_index:(county_index + 718), ]

# To make the output cleaner, I deleted the index column
data_frame <- data_frame |> select(-1)

print("Done!")

return(data_frame)
}

#Racine County

sss_main("Racine County")
