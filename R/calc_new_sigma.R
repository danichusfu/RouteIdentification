

#' Calculates updated sigma given W, X, Y, Beta
#'
#' @param W weights
#' @param X X matrix
#' @param Y Y matrix
#' @param Beta updated betas
#' @return updated sigma

calc_new_sigma <- function(W, X, Y, Beta){
  #W <- diag(weights)
  Sigma_new <- diag(diag(as.matrix(Matrix::t(Y - X %*% Beta) %*% W %*% (Y - X %*% Beta) / sum(W))))
}