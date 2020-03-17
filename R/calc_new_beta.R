

#' Calculates updated beta given W, X, Y
#'
#' @param W weights
#' @param X X matrix
#' @param Y Y matrix
#' @return updated beta

calc_new_beta <- function(W, X, Y){
  #W <- diag(weights)
  Beta_new <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
  return(Beta_new)
}