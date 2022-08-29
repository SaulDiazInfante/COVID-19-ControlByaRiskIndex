#
compute_parameter_gradient_solutions <- 
  function(
    par,
    initialConditions,
    timeLine = seq(0, 156, 1),
    target_par ="a_I",
    parameter_grid = list(1e-4, 5e-4, 1e-3, 5e-3)
  ){
  res <- list()
  header <- list()
  for (a_ii in parameter_grid)
  {
    par[[target_par]] <- a_ii
    header <- 
      append(
        header,
        paste("a_I_", as.character(a_ii), sep='')
      )
    cat(
      paste(
        "Computing solution with ",
        target_par, " = ", 
        as.character(par[[target_par]]), 
        sep=''
      )
    )
    cat("\n-------------------------------------------------\n \t")
    sim_a_ii <- get_best_response_solution(
      par,
      initialConditions,
      timeLine = seq(0, 156, 1),
      data_path="./simulated_data",
      suffix=paste("_a_I_", as.character(a_ii), sep='')
    )
    res <- append(res, list(sim_a_ii))
  }
  names(res) <- header
  return(res)
}
