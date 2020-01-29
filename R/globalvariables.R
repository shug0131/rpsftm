rpsftm_env <- new.env(parent=emptyenv())
rpsftm_env$fn_count <- 0
rpsftm_env$n_eval_z <- 100 # the default
rpsftm_env$psi <- list()
rpsftm_env$value <- list()

reset_record <- function(n_eval_z){
  rpsftm_env$fn_count <- 0
  rpsftm_env$n_eval_z <- n_eval_z
  rpsftm_env$psi <- list()
  rpsftm_env$value <- list()
}

add_record <- function(psi_new, value_new){
  attach(rpsftm_env)
  on.exit(detach(rpsftm_env))
  if(fn_count<n_eval_z){
    rpsftm_env$fn_count <- fn_count+1
    rpsftm_env$psi <- c(psi, list(psi_new))
    rpsftm_env$value <- c(value, list(value_new))
  }
 
}
