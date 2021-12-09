library(dplyr)
library(rjson)
loadInitialConditions <- function(file_name = "modelParameters.json") {
    modelParameters <- fromJSON(file = file_name)
    dataFrameModelParameters <- as.data.frame(modelParameters)
    initialConditionNames <- dataFrameModelParameters %>%
        select(ends_with("0")) %>% names()
        initialCondition <- dataFrameModelParameters %>%
            select(.dots = initialConditionNames)
            initialCondition_var <- as.vector(t(initialCondition))
    odeVarNames <- c("S", "I", "V", "R", "C", "F")
    colnames(initialCondition) <- odeVarNames
    ans <- initialCondition
    return(as.vector(t(ans)))
}