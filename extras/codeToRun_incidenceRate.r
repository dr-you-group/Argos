## Load libraries
library(DatabaseConnector)
library(Argos)
library(checkmate)
library(dplyr)

## Database settings
cdmDatabaseSchema <-""
vocabularyDatabaseSchema  <- ""
cohortDatabaseSchema <- ""

cohortDatabaseSchema = ""
cohortTable = ""
outcomeDatabaseSchema = ""
outcomeTable = ""

outputFolder <- "/home/rstudio/result"
options(fftempdir = "D:/FFtemp")

pathToDriver = "/home/rstudio/jdbc"
connectionDetails<-DatabaseConnector::createConnectionDetails(dbms = '',
                                                              server = "",
                                                              user = "",
                                                              password = "",
                                                              pathToDriver = pathToDriver)
options(connectionObserver = NULL)


## outcome and base cohort settings
restrictArgosDataSettings <- createRestrictArgosDataSettings(studyStartDate = "20020101",
                                                             # studyStartDate = "20170101",
                                                             studyEndDate = "20131231",
                                                             # studyEndDate = "20211231",
                                                             firstExposureOnly = F,
                                                             washoutPeriod = 0,
                                                             sampleSize = NULL)

covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE, 
                                                                useDemographicsAge = TRUE)

## Get Argos data from the Database
ArgosData <- getArgosData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          targetId = ,
                          outcomeIds = ,
                          restrictArgosDataSettings = restrictArgosDataSettings,
                          cohortDatabaseSchema = "",
                          cohortTable = "cohort",
                          outcomeDatabaseSchema = "",
                          outcomeTable = "cohort",
                          cdmVersion = "5",
                          covariateSettings = covariateSettings)

# saveArgosData(argosData, "/home/rstudio/data/sampleargosData1000")
argosData <- loadArgosData("/home/rstudio/result/ArgosData.Rda")


populationSettings <- createStudyPopulationSettings(
    binary = T,
    includeAllOutcomes = T,
    firstExposureOnly = TRUE,
    washoutPeriod = 365,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 730,
    requireTimeAtRisk = T,
    minTimeAtRisk = 0,
    riskWindowStart = 1,
    startAnchor = 'cohort start',
    riskWindowEnd = 99999,
    endAnchor = "cohort end",
    restrictTarToCohortEnd = F,
    changeCohortStartDate = T,
    newCohortStartDate = "20030101"
)


## Get the raw population data for calculate for incidence
raw_pop <- createStudyPopulation(argosData = ArgosData,
                                 outcomeId = ,
                                 populationSettings,
                                 population = NULL)

## Save the raw population data
saveRDS(raw_pop, file="/home/rstudio/result/raw_pop.Rda")
raw_pop <- readRDS(file="/home/rstudio/result/raw_pop.Rda")
raw_pop

## Make the population data to be fit for IncidencePrevalence package
final_pop <- raw_pop %>% 
    mutate(censoredDate = cohortStartDate + survivalTime, outcome_start_date = cohortStartDate + daysToEvent) %>% 
    select(subjectId, cohortStartDate, censoredDate,outcomeCount, outcome_start_date)
colnames(final_pop) <- c("subject_id","cohort_start_date","cohort_end_date","outcomeCount" ,"outcome_start_date")

## Save the final population data
saveRDS(final_pop, file="/home/rstudio/result/final_pop.Rda")
final_pop <- readRDS(file="/home/rstudio/result/final_pop.Rda")
# bar <- readRDS(file="/home/rstudio/samsung_ver/final_pop.Rda")
# bar

# sum(final_pop$outcomeCount)

## Setting 
# startDate = as.Date("2003-01-01")
# endDate = as.Date("2013-12-31")
startDate = "2003-01-01"
endDate = "2013-12-31"
timeInterval = "months"
final_pop = final_pop

## calculate incidence rate
ir <- cal_incidencerate(
    startDate = startDate,
    endDate = endDate,
    timeInterval = timeInterval,
    final_pop = final_pop
)

## Save the incidence rate data
saveRDS(ir, file="/home/rstudio/temp/ir.Rda")
# bar <- readRDS(file="/home/rstudio/temp/ir.Rda")
# bar

## Plot the incidence rate
plot(ir$incidence_start_date, ir$incidence_100000_pys, type = 'b')
