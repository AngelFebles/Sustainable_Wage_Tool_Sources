# To print all columns
options(dplyr.width = Inf)

full_job_data <- readxl::read_excel("DataFiles/OutputFiles/full_job_data.xlsx")

full_job_data <- full_job_data[, c("occupation_name", "Hourly mean County", "Annual mean County", "education_requirement")]
# Remove rows where 'Hourly mean County' is NA
full_job_data <- full_job_data[!is.na(full_job_data$`Hourly mean County`), ]

# Sort the data by 'Hourly mean County'
x <- full_job_data[!is.na(full_job_data$`Annual mean County`), ]
x <- x[order(x$`Annual mean County`), ]

# 1 Adult, living in a studio. Just the essentials
# target <- 25737.6

# A little more realistic (index 4316 [One room, moderate food] goes to 28k)
target <- 30000


## O(N), Nope
# Find values greater than or equal to the target
# result <- x[x$`Hourly mean County` >= target, ]

# head(x$`Annual mean County`)

## O(log n)
# Only 1 value

# All Vals Greater
# indexes1 <- seq(index, length(x$`Annual mean County`))

# Only 5 values
index <- findInterval(target, x$`Annual mean County`) + 1
indexes2 <- seq(index, min(index + 5, length(x$`Annual mean County`)))

# Display the first few rows of the result
# head(result)

print("Index of first job")
print(index)
print(x[indexes2, ])


# target_plus_ten_percent <- target + (target * 0.1)
# print(target_plus_ten_percent)
