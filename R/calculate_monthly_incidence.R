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

#'calculate incidence rate
#' @param startDate
#' @param endDate           
#' @param timeInterval           minumal unit for cohort start date ('year' > 'quarter' > 'month' > 'week' > day')
#' @param final_pop
#'
#'
#'@import dplyr
#'@export
cal_incidencerate <- function(
    startDate = as.Date("2003-01-01"),
    endDate = as.Date("2013-12-31"),
    timeInterval = "months",
    final_pop = final_pop
  ){
  
  startDate = as.Date(startDate)
  endDate = as.Date(endDate)
    
  if (timeInterval == "weeks") {
    weekCorrection <- lubridate::days(1)
  } else {
    weekCorrection <- lubridate::days(0)
  }
  
  unit <- substr(timeInterval, 1, nchar(timeInterval) - 1)
  startDay <- lubridate::floor_date(startDate, unit = unit)
  #studyDays <- getStudyDaysElements(startDay, endDate, timeInterval)
  
  studyDays <- getStudyDaysElements(startDate, endDate, "days")
  studyDays <- studyDays %>%
    dplyr::mutate(overall = "overall") %>%
    dplyr::rename("time" = .env$timeInterval) %>%
    dplyr::mutate(time = as.character(.data$time)) %>%
    dplyr::rename("dates" = "start_time") %>%
    dplyr::group_by(.data$time) %>%
    dplyr::summarise(
      start_time = min(.data$dates, na.rm = TRUE),
      end_time = max(.data$dates, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  completeDatabaseIntervals = FALSE
  
  if (completeDatabaseIntervals == TRUE) {
    if (timeInterval == "weeks") {
      studyDays <- studyDays %>%
        dplyr::filter(difftime(studyDays$end_time,
                               studyDays$start_time,
                               units = "days"
        ) == 6)
    }
    if (timeInterval %in% c("months", "quarters", "years")) {
      studyDays <- studyDays %>%
        dplyr::filter(.data$start_time ==
                        lubridate::floor_date(.data$start_time,
                                              unit = timeInterval
                        ) +
                        weekCorrection) %>%
        dplyr::filter(.data$end_time == lubridate::floor_date(
          .data$end_time,
          unit = timeInterval
        ) + weekCorrection + switch(timeInterval,
                                    "months" = months(1) - lubridate::days(1),
                                    "quarters" = months(3) - lubridate::days(1),
                                    "years" = lubridate::years(1) - lubridate::days(1)
        ))
    }
  }
  
  
  ir <- list()
  for (i in seq_len(nrow(studyDays))) {
    workingStartTime <- studyDays$start_time[i]
    workingEndTime <- studyDays$end_time[i]
    
    # colnames(population_2) <- c("subject_id","cohort_start_date","cohort_end_date","outcomeCount" ,"outcome_start_date")
    
    workingPop <- final_pop %>%
      dplyr::filter(.data$cohort_end_date >= .env$workingStartTime) %>%
      dplyr::filter(.data$cohort_start_date <= .env$workingEndTime)
    
    # # people who can contribute to the period
    # workingPop <- studyPop %>%
    #   dplyr::filter(.data$cohort_end_date >= .env$workingStartTime) %>%
    #   dplyr::filter(.data$cohort_start_date <= .env$workingEndTime)
    
    if (nrow(workingPop) > 0) {
      # individuals start date for this period
      # which could be start of the period or later
      workingPop <- workingPop %>%
        dplyr::mutate(tStart = dplyr::if_else(.data$cohort_start_date <= .env$workingStartTime,
                                              as.Date(.env$workingStartTime),
                                              as.Date(.data$cohort_start_date)
        )) %>%
        # individuals end date for this period
        # end of the period or earlier
        dplyr::mutate(
          tEnd =
            dplyr::if_else(.data$cohort_end_date >= .env$workingEndTime,
                           as.Date(.env$workingEndTime),
                           as.Date(.data$cohort_end_date)
            )
        )
      
      # compute working days
      workingPop <- workingPop %>%
        dplyr::mutate(workingDays = as.numeric(difftime(
          .data$tEnd,
          .data$tStart,
          units = "days"
        )) + 1)
      
      # erase outcome_start_date if not during period
      workingPop <- workingPop %>%
        dplyr::mutate(outcome_start_date = dplyr::if_else(
          .data$outcome_start_date <= .data$tEnd &
            .data$outcome_start_date >= .data$tStart,
          as.Date(.data$outcome_start_date),
          as.Date(NA)
        ))
      
      ir[[paste0(i)]] <- workingPop %>%
        dplyr::summarise(
          n_persons = dplyr::n_distinct(.data$subject_id),
          person_days = sum(.data$workingDays),
          n_events = sum(!is.na(.data$outcome_start_date))
        ) %>%
        dplyr::mutate(incidence_start_date = .env$workingStartTime) %>%
        dplyr::mutate(incidence_end_date = .env$workingEndTime)
    }
  }
  
  ir <- dplyr::bind_rows(ir) %>%
    dplyr::mutate(person_years = .data$person_days / 365.25) %>%
    dplyr::mutate(
      incidence_100000_pys =
        (.data$n_events / .data$person_years) * 100000
    )
  
  return(ir)
}