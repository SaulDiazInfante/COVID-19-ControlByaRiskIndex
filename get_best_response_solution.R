source("loadInitialConditions.R")
source("loadTransferParameters.R")
source("evaluateRhsControlledODE.R")
source("getOdeSolution.R")
source("updatePolicy.R")
source("get_time_stamp_sate_solution.R")
source("get_time_stamp_policy.R")
source("get_contrafactual_solution.R")
source("get_controlled_solution.R")
source("get_timeline_policy_transitions.R")
source("plot_scene.R")
#
get_best_response_solution <- 
  function(
    par,
    initialConditions,
    timeLine = seq(0, 156, 1),
    data_path="./simulated_data",
    suffix=''
  ){
    refeSol <-
      get_contrafactual_solution(
        par,
        initialConditions = initialConditions,
        time_line = timeLine,
        outputh_path = data_path,
        suffix = suffix
      )
    controlledSol <-
      get_controlled_solution(
        par,
        initialConditions,
        time_line = timeLine,
        decision_period_lenght = 2,
        outputh_path = data_path,
        suffix = suffix
      )
    time_line_events <- 
      get_timeline_policy_transitions(
        policy_file_name = controlledSol$policy_path,
        solution_file = controlledSol$trajectory_path,
        output_path = data_path,
        suffix = suffix
      )
    res <- list()
    res$counterfactual <- refeSol$refSol
    res$controlledSol <- controlledSol$controlled_solution
    res$timeLineEvents <- time_line_events$timeline_events
    res$data_path <- 
      list(
        refeSol$conterfactual_path,
        controlledSol$trajectory_path,
        controlledSol$policy_path,
        time_line_events$timeline_events_path
        )
    return(res)
  }
