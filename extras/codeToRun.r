## Load libraries
library(Argos)
library(DatabaseConnector)
library(checkmate)
library(dplyr)
library(lubridate)
library(stringr)

## Database settings for reading existing tables
cdmDatabaseSchema <- ""
vocabularyDatabaseSchema  <- ""
# oracleTempSchema <- ""

# Database settings for writing new tables
cohortDatabaseSchema <- ""
outcomeDatabaseSchema <- ""

outcomeTable <- ""

## Outcome folder settings
outputFolder <- "/home/rstudio/result"
options(fftempdir = "D:/FFtemp")

## Connection settings
pathToDriver <- "/home/rstudio/jdbc"
connectionDetails<-DatabaseConnector::createConnectionDetails(dbms = "",
                                                              server = "",
                                                              user = "",
                                                              password = "",
                                                              pathToDriver = pathToDriver)


## Do not change below codes ################################################################################################################

## Database setting for Original CDM
original_cohortTable = "cohort"
options(connectionObserver = NULL)

## Specify the cohort lists
targetCohortId <- 99
outcomeCohortList <- data.frame(outcomeCohortId <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                                outcomeCohortName <- c('HTN_Drug_Use','Atiral_fibrillation','Inpatient_Hospitalization', 
                                                       'Emergency_room_visits', 'All_cause_mortality', 
                                                       'Asthma_or_Chronic_obstructive_pulmonary_disease',
                                                       'Asthma_without_COPD', 'Chronic_obstructive_pulmonary_disease__without_asthma',
                                                       'Tuberculosis', 'Major_Depressive_Disorder', 
                                                       'Acute_myocardial_infarction_with_inpatient_admission', 
                                                       'Stroke_with inpatient_admission',
                                                       'Diabetes_Mellitus_Type_2_or_history_of_diabetes', 'Kawasaki_disease')) 

## Specify the range of years and months
dbStartDate <- lubridate::ymd("20180101")
dbEndDate <- lubridate::ymd("20220401")

ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt")) 

#Connection
connection<-DatabaseConnector::connect(connectionDetails)

####Create cohort####
#create the cohort table
ParallelLogger::logInfo("Creating table for the cohorts")
sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "CreateCohortTable.sql",
                                         packageName = "Argos",
                                         dbms = attr(connection,"dbms"),
                                         oracleTempSchema = oracleTempSchema,
                                         cohort_database_schema = cohortDatabaseSchema,
                                         cohort_table = original_cohortTable)
DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)

# create the target cohort table
ParallelLogger::logInfo("Creating target cohort")
sql <- SqlRender::loadRenderTranslateSql(sqlFilename= "Base_population.sql",
                                         packageName = "Argos",
                                         dbms = attr(connection,"dbms"),
                                         oracleTempSchema = oracleTempSchema,
                                         cdm_database_schema = cdmDatabaseSchema,
                                         target_database_schema = cohortDatabaseSchema,
                                         target_cohort_table = outcomeTable,
                                         target_cohort_id = targetCohortId)

DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)


ParallelLogger::logInfo("Creating outcome cohorts")
for (i in seq(outcomeCohortList$outcomeCohortId)){
    sql <- SqlRender::loadRenderTranslateSql(sqlFilename= paste(outcomeCohortList$outcomeCohortName[i],".sql", sep=""),
                                             packageName = "Argos",
                                             dbms = attr(connection,"dbms"),
                                             oracleTempSchema = oracleTempSchema,
                                             cdm_database_schema = cdmDatabaseSchema,
                                             vocabulary_database_schema = vocabularyDatabaseSchema,
                                             target_database_schema = cohortDatabaseSchema,
                                             target_cohort_table = outcomeTable,
                                             target_cohort_id = outcomeCohortList$outcomeCohortId[i])
    DatabaseConnector::executeSql(connection, sql, progressBar = TRUE, reportOverallTime = TRUE)
}


## outcome and base cohort settings
restrictArgosDataSettings <- Argos::createRestrictArgosDataSettings(studyStartDate = "",
                                                                    studyEndDate = "",
                                                                    firstExposureOnly = F,
                                                                    washoutPeriod = 0,
                                                                    sampleSize = NULL)

covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, 
                                                                useDemographicsAge = TRUE)


####################### 한번 이것만 해보기
## Get Argos data from the Database
ArgosData <- Argos::getArgosData(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 oracleTempSchema = cdmDatabaseSchema,
                                 targetId = targetCohortId,
                                 outcomeIds = outcomeCohortList$outcomeCohortId,
                                 restrictArgosDataSettings = restrictArgosDataSettings,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTable = original_cohortTable,
                                 outcomeDatabaseSchema = outcomeDatabaseSchema,
                                 outcomeTable = outcomeTable,
                                 cdmVersion = "5",
                                 # cdmVersion = "5.3", # 으로도 해보기!
                                 covariateSettings = covariateSettings)

## 제출 시 아래껄로!
saveRDS(ArgosData, file="/home/rstudio/result/ArgosData_1129.rds")
# saveRDS(ArgosData, file="/home/rstudio/result/ArgosData.rds")


## Create a dataframe from start and end date
yearMonthTable <- Argos::createYearMonthTable(startDate = dbStartDate,
                                              endDate = dbEndDate)

## Calculating Monthly incidence rate start
incidencePath = paste(outputFolder,"/incidence", sep="")

if (!file.exists(incidencePath)){
    dir.create(incidencePath)
}

for (i in 1:nrow(yearMonthTable)){
    
    # Setting for Incidence rate population
    populationSettings <- createStudyPopulationSettings(
        binary = T,
        includeAllOutcomes = T,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 730,
        requireTimeAtRisk = F,
        minTimeAtRisk = 0,
        riskWindowStart = 0,
        startAnchor = 'cohort start',
        riskWindowEnd = yearMonthTable$days[i]-1,
        endAnchor = 'cohort start',
        restrictTarToCohortEnd = F,
        changeCohortStartDate = T,
        newCohortStartDate = yearMonthTable$year_month[i]
    )
    
    monthlyRawPop <- data.frame()
    for (j in seq(outcomeCohortList$outcomeCohortId)){
        
        smallRawPop <- createStudyPopulation(argosData = ArgosData,
                                             outcomeId = j,
                                             populationSettings,
                                             population = NULL)
        monthlyRawPop <- rbind(monthlyRawPop, smallRawPop)
        
    }
    
    saveRDS(monthlyRawPop, file = paste(incidencePath, "/ir_pop_", as.character(yearMonthTable$year_month[i]), ".rds", sep = ""))
}

# IR calculation
calculatedResultPath <- paste(outputFolder,"/calculatedResult", sep="")

if (!file.exists(calculatedResultPath)){
    dir.create(calculatedResultPath)
}

calculatedResultPath <- calculatedResultPath
incidencePath <- incidencePath
fileNames <- list.files(incidencePath)

ir <- data.frame(matrix(ncol=5))
colnames(ir) <- c("outcomeId", "date", "htn_number", "total_tar", "IR_100")
for (i in 1:length(fileNames)){
    raw_data = readRDS(file = paste(incidencePath, fileNames[i], sep=""))
    
    outcomeId <- raw_data$outcomeId
    small_date <- str_extract_all(file_names[i], "\\d+")
    small_htn_number <- sum(raw_data$outcomeCount)
    small_total_tar <- sum(raw_data$timeAtRisk)
    small_IR_100 <- (small_htn_number / small_total_tar * (1/365.25) * 100)
    
    ir[i,] <-c(outcomeId <- outcomeId, date <-small_date, htn_number <- small_htn_number, total_tar <- small_total_tar, IR_100 <- small_IR_100)
    print(paste("Calculating incidence rate progress : ", i, "/", length(fileNames)))
}

print(ir)

## Save the result data
write.csv(ir, paste(calculatedResultPath, "/ir_calculated_231127.csv", sep = ""))

## 제출 시에는 아래껄로 하기
# write.csv(ir, paste(calculatedResultPath, "/ir_calculated.csv"))
# ir <- saveRDS(file = "/home/rstudio/result/ir_231127.rds")
# ir

#--------------------------------------------------------------------------------------------

## calculating Utilization
calculatedResultPath <- paste(outputFolder,"/calculatedResult", sep="")

utilizationPath = paste(outputFolder,"/utilization", sep="")
calculatedResultPath <- calculatedResultPath
fileNames <- list.files(utilizationPath)

if (!file.exists(utilizationPath)){
    dir.create(utilizationPath)
}

for (i in 1:nrow(yearMonthTable)){
    
    # Setting for Utilization population
    populationSettings <- createStudyPopulationSettings(
        binary = F,
        includeAllOutcomes = F,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeSubjectsWithPriorOutcome = FALSE,
        priorOutcomeLookback = 0,
        requireTimeAtRisk = F,
        minTimeAtRisk = 0,
        riskWindowStart = 0,
        startAnchor = 'cohort start',
        riskWindowEnd = yearMonthTable$days[i]-1,
        endAnchor = 'cohort start',
        restrictTarToCohortEnd = F,
        changeCohortStartDate = T,
        newCohortStartDate = yearMonthTable$year_month[i]
    )
    
    monthlyRawPop <- data.frame()
    for (j in seq(outcomeCohortList$outcomeCohortId)){
        
        # smallRawPop <- j
        smallRawPop <- createStudyPopulation(argosData = ArgosData,
                                             outcomeId = j,
                                             populationSettings,
                                             population = NULL)
        monthlyRawPop <- rbind(monthlyRawPop, smallRawPop)
        
    }
    saveRDS(monthlyRawPop, file = paste(incidencePath, "/util_pop_", as.character(yearMonthTable$year_month[i]), ".rds", sep = ""))
    
}

# Utilization calculation
calculatedResultPath <- paste(outputFolder,"/calculatedResult", sep="")

# if (!file.exists(calculatedResultPath)){
#   dir.create(calculatedResultPath)
# }

calculatedResultPath <- calculatedResultPath
utilizationPath <- utilizationPath
fileNames <- list.files(utilizationPath)

days_util <- data.frame(matrix(ncol=5))
colnames(days_util) <- c("outcomeId", "date", "htn_number", "days_in_current_month", "HTN_util_number")
for (i in 1:length(fileNames)){
    raw_data = readRDS(file = paste(utilizationPath, fileNames[i], sep=""))
    
    outcomeId <- raw_data$outcomeId
    small_date <- str_extract_all(file_names[i], "\\d+")
    small_htn_number <- sum(raw_data$outcomeCount)
    small_days_in_current_month <- days_in_month(as.Date(small_date[[1]], format = "%Y%m%d", origin = "1970-01-01"))
    small_HTN_util_number <- (small_htn_number / small_days_in_current_month)
    
    days_util[i,] <-c(outcomeId <- outcomeId, date <-small_date, htn_number <- small_htn_number, days_in_current_month <- small_days_in_current_month, HTN_util_number <- small_HTN_util_number)
    print(paste("Calculating utilization progress : ", i, "/", length(file_names)))
}


## Save the result data
write.csv(days_util, paste(calculatedResultPath, "/util_calculated_231129.csv", sep = ""))

## 제출 시에는 아래껄로 하기
# write.csv(days_util, paste(calculatedResultPath, "/util_calculated.csv", sep = ""))
# saveRDS(days_util, file = paste(calculatedResultPath, "/util_calculated_231129.rds", sep = ""))

