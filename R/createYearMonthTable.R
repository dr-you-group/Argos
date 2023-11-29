# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of Argos
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#'calculating days in the month 
#'@param date  
#'@import lubridate
#'@export
# Function to calculate days in the month
days_in_month_argos <- function(date) {
    as.integer(format(date %m+% months(1) - days(1), "%d"))
}


#' Create a table of database observation start and end date. 
#'
#' @param startDate   the database's observation start date
#' @param endDate     the database's observation end date
#' @import lubridate
#' @import stringr
#' @export
# Function to calculate days in a month
createYearMonthTable <- function(startDate = lubridate::ymd("20180101"),
                                 endDate = lubridate::ymd("20220401")){
    
    # Initialize empty vectors to store data
    years <- numeric()
    months <- character()  # Change to character type to store two-digit month strings
    days <- numeric()
    year_month <- character()  # New column to store YYYYMM01 format
    
    # Generate a sequence of months from start to end date
    date_sequence <- seq(startDate, endDate, by = "month")
    
    # Extract year, month, and days in each month and store in vectors
    for (date in date_sequence) {
        current_year <- lubridate::year(as.Date(date, origin="1970-01-01"))
        current_month <- stringr::str_pad(month(as.Date(date, origin="1970-01-01")), width = 2, pad = "0")  # Format month as two-digit string
        days_in_current_month <- Argos::days_in_month_argos(as.Date(date, origin="1970-01-01"))
        year_month_value <- paste0(current_year, current_month, "01")  # Concatenate year, month, and "01"
        
        # Append data to vectors
        years <- c(years, current_year)
        months <- c(months, current_month)
        days <- c(days, days_in_current_month)
        year_month <- c(year_month, year_month_value)
    }
    
    # Create a dataframe from the vectors
    year_month_table <- data.frame(year = years,
                                   month = months,
                                   days = days,
                                   year_month = year_month)  # Add the new 'year_month' column
    
    return(year_month_table)
}
