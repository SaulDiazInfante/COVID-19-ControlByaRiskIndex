library(dplyr)
library(rjson)
#
loadTransferParameters <- function(file_name = "modelParameters.json") {
  modelParameters <- fromJSON(file = file_name)
  dataFrameModelParameters <- as.data.frame(modelParameters)
  transferParametersNames <- dataFrameModelParameters %>%
    select(!starts_with("initialConditions")) %>%
    names()
  transferParameters <- dataFrameModelParameters %>%
    select(.dots = transferParametersNames)
  colnames(transferParameters) <- transferParametersNames
  transferParametersNames <- transferParameters %>%
    select(!starts_with("costWeights")) %>%
    names()
  transferParameters <- dataFrameModelParameters %>%
    select(.dots = transferParametersNames)
  colnames(transferParameters) <- transferParametersNames
  return(transferParameters)
}
