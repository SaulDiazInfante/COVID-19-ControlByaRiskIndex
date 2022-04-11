get_semaphore_actions <- function(ligth='green'){
  size_restriction <- c(
    "green" = 0.0,
    "yellow" = 0.5,
    "red" = 0.9
  )
  
  movility_restriction <- c(
    "green" = 0.0,
    "yellow" = 0.3,
    "red" = 0.6
  )
  action <- list()
  u_k <- as.numeric(size_restriction[ligth])
  u_beta <- as.numeric(movility_restriction[ligth])
  action$u_k <- u_k
  action$u_beta <- u_beta
  action$semaphore <- ligth
  return(action)
}
  