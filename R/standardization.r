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

#'Creating setting for age/gender/race/ethnicity standardization
#'
createStandSettings<-function(ageStandardization=TRUE,
                              ageGroupStandardization=FALSE,
                              genderStandardization=TRUE,
                              raceStandardization=FALSE,
                              ethnicityStandardization=FALSE,
                              mode = "direct",
                              basePopulation,
                              year = 2006){
    AGStandardSettings<-list(ageStandardization = ageStandardization,
                             ageGroupStandardization = ageGroupStandardization,
                             genderStandardization = genderStandardization,
                             raceStandardization = raceStandardization,
                             ethnicityStandardization = ethnicityStandardization,
                             basePopulation = basePopulation)
}