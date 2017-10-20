#' immdef
#'
#' Simulated data to use with the \code{\link{rpsftm}} function.
#'
#' @format A simulated data frame with 9 variables and 1000 observations representing a study where participants 
#' were randomly assigned to receive treatment immediately or deferred. Participants in the deferred 
#' arm could crossover and receive treatment. The primary endpoint was time to disease progression.
#' 
#' The data are based on a randomized controlled trial Concorde \url{http://dx.doi.org/10.1016/S0140-6736(94)90006-X}
#' \describe{
#'    \item{id}{participant ID number}
#'    \item{def}{indicator that the participant was assigned to the Deferred treatment arm}
#'    \item{imm}{indicator that the participant was assigned to the Immediate treatment arm}
#'    \item{censyrs}{censoring time, in years, corresponding to the close of study minus the time of entry for each participant}
#'    \item{xo}{an indicator that crossover occurred}
#'    \item{xoyrs}{the time, in years, from entry to switching, or 0 for participants in the Immediate arm}
#'    \item{prog}{an indicator of disease progression (1), or censoring (0)}
#'    \item{progyrs}{time, in years, from entry to disease progression or censoring}
#'    \item{entry}{the time of entry into the study, measured in years from the date of randomisation}
#'  
#'   }
"immdef"