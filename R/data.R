#' immdef
#'
#' Simulated data to use with the \code{\link[<pkg>:<pkg>-package]{rpsftm}} library.
#'
#' @format A data frame with 13 variables and 1000 observations represent a study where participants 
#' were randomly assigned to receive treatment immediately or deferred. Participants in the deferred 
#' arm could crossover and receive treatment. The primary endpoint was time to disease progression:
#' \describe{
#'    \item{id}{participant ID number}
#'    \item{def}{indicator that the participant was assigned to the Deferred treatment arm}
#'    \item{imm}{indicator that the participant was assigned to the Immediate treatment arm}
#'    \item{censyrs}{a real, or theoretical censoring time, corresponding to the close of study}
#'    \item{xo}{an indicator that crossover occured}
#'    \item{xoyrs}{the time at which crossover happened, or 0 for participants in the Immediate arm}
#'    \item{prog}{an indicator of disease progression (1), or censoring (0)}
#'    \item{progyrs}{time of disease progression or censoring}
#'    \item{X_st}{a constant of value 1. Maybe we should drop this?}
#'    \item{X_d}{equal to prog. Maybe we should drop this?}
#'    \item{X_t}{equal to progyrs. Maybe we shoudl drop this?}
#'    \item{X_t0}{a constnat value of 0. Maybe we should drop this?}
#'  
#'   }
"immdef"