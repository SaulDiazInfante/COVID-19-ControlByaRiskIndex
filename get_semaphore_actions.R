get_semaphore_actions <- 
  function(par, ligth){
  # TODO pull this weight from the json parameters file
  size_restriction <- c(
    "green" = par$meeting_size_actions.green,
    "yellow" = par$meeting_size_actions.yellow,
    "orange" = par$meeting_size_actions.orange,
    "red" = par$meeting_size_actions.green
  )
  
  movility_restriction <- c(
    "green" = par$beta_actions.green,
    "yellow" = par$beta_actions.yellow,
    "orange" = par$beta_actions.orange,
    "red" = par$beta_actions.red
  )
  action <- list()
  u_k <- as.numeric(size_restriction[ligth])
  u_beta <- as.numeric(movility_restriction[ligth])
  action$u_k <- u_k
  action$u_beta <- u_beta
  action$semaphore <- ligth
  return(action)
}
  