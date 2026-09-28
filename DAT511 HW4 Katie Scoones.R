# DAT 511 - Homework 4
# Name: Katie Scoones

# Part 1: pollutantmean()
# Function to calculate the mean of a specified pollutant across a given set of monitor locations.
# Excluding NA values from the calculation.

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # Creating file names for the requested monitor IDs.
    files <- sprintf("%03d.csv", id)
    # Creating full file paths using set data directory.
    paths <- file.path(directory, files)
    # Reading data from each CSV file.
    data <- lapply(paths, read.csv)
    # Combine data from all monitors into one data frame.
    data <- do.call(rbind, data)
    # Select the requested pollutant from combined data. 
    values <- data [[pollutant]]
    # Calculate the mean and ignore missing values.
    mean(values, na.rm = TRUE)

}

# Part 1 Results
#  pollutantmean("data", "sulfate", 1:10) -- 4.064128
#  pollutantmean("data", "nitrate", 70:72) -- 1.706047
#  pollutantmean("data", "sulfate", 34) -- 1.477143
#  pollutantmean("data", "nitrate") -- 1.702932


# Part 2: complete()
# Function to calculate number of completely observed cases for each specified monitor.

complete <- function(directory, id = 1:332) {
    # Create file names for the requested monitor IDs.
    files <- sprintf("%03d.csv", id)
    # Create full file paths using specified data directory. 
    paths <- file.path(directory, files)
    # Read data from each CSV file.
    data <- lapply(paths, read.csv)
    # Count number of rows with no missing values.
    nobs <- sapply(data, function(x) sum(complete.cases(x)))
    # Create data frame containing monitor IDs and number of complete cases.
    result <- data.frame(id = id, nobs = nobs)
    # Return results.
    result

}

# Part 2 Results
# [1] 228 148 124 165 104 460 232
# [1] 219
# [1] 711 135  74 445 178  73  49   0 687 237



# Part 3 corr()
# Function to calculate correlation between sulfate and nitrate for monitors with more complete cases than specified threshold. 

corr <- function(directory, threshold = 0) {
    # Create file names for all monitor IDs.
    files <- sprintf("%03d.csv", 1:332)
    # Create full file paths using specified data directory. 
    paths <- file.path(directory, files)
    # Read data from each CSV file.
    data <- lapply(paths, read.csv)
    # Count number of completely observed rows for each monitor. 
    nobs <-sapply(data, function(x) sum(complete.cases(x)))
    # Select monitors that have more complete observations than the threshold. 
    data <- data[nobs > threshold]
    # Calculate correlation between sulfate and nitrate for each monitor. 
    correlations <- sapply(data, function(x) {
        cor(x$sulfate, x$nitrate, use = "complete.obs")
    })
    # Return correlations
    correlations

}


# Part 3 Results
# [1]  0.2688  0.1127 -0.0085  0.4586  0.0447
# [1] 243.0000   0.2540   0.0504  -0.1462  -0.1680   0.5969
# [1]  0.0000 -0.0190  0.0419  0.1901


