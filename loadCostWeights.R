library(dplyr)
library(rjson)
#
loadCostrParameters <- function(file_name = "modelParameters.json") {
  modelParameters <- fromJSON(file = file_name)
  dataFrameModelParameters <- as.data.frame(modelParameters)
  costParametersNames <- dataFrameModelParameters %>%
    select(starts_with("costWeights")) %>%
    names()
  costParameters <- dataFrameModelParameters %>%
    select(.dots = costParametersNames)
  colnames(costParameters) <- costParametersNames
  return(costParameters)
}
