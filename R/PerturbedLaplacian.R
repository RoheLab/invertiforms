
#' Title
#'
#' @slot tau_row numeric.
#' @slot tau_col numeric.
#' @slot rsA numeric.
#' @slot csA numeric.
#' @slot tau_choice_row character.
#' @slot tau_choice_col character.
#'
#' @return
#' @export
#' @include s4-generics.R
#'
setClass(
  "PerturbedLaplacian",
  contains = "Invertiform",
  slots = c(
    tau = "numeric",
    tau_choice = "character",
    rsA = "numeric",
    csA = "numeric"
  )
)


#' Title
#'
#' @param iform TODO
#' @param A TODO
#'
#' @return TODO
#' @export
#'
#' @rdname PerturbedLaplacian
#' @examples
#'
#' library(igraph)
#' library(igraphdata)
#'
#' data("karate", package = "igraphdata")
#'
#' A <- get.adjacency(karate)
#'
#' iform <- PerturbedLaplacian(A)
#'
#' L <- transform(iform, A)
#' L
#'
#' A_recovered <- inverse_transform(iform, L)
#'
#' all.equal(A, A_recovered)
#'
PerturbedLaplacian <- function(A, tau = NULL) {

  # don't try to be clever and simplify the degree calculations,
  # there are at least two gotchas:
  #
  #   (1) don't use abs(), which returns a *vector*
  #
  #   (2) Matrix::rowSums() and rowSums() are different

  rsA <- Matrix::rowSums(A * sign(A))  # (absolute) out-degree
  csA <- Matrix::colSums(A * sign(A))  # (absolute) in-degree

  if (is.null(tau)) {
    tau <- Matrix::mean(A)
    tau_choice <- "Mean [default]"
  } else {
    tau_choice <- "User Specified"
  }

  new(
    "PerturbedLaplacian",
    rsA = rsA,
    csA = csA,
    tau = tau,
    tau_choice = tau_choice
  )
}

#' @rdname PerturbedLaplacian
#' @importFrom sparseLRMatrix sparseLRMatrix
#' @export
setMethod(
  "transform",
  signature = c("PerturbedLaplacian", "sparseMatrix"),
  definition = function(iform, A) {
    D_row <- Diagonal(n = nrow(A), x = 1 / sqrt(iform@rsA + iform@tau))
    D_col <- Diagonal(n = ncol(A), x = 1 / sqrt(iform@csA + iform@tau))
    sparse <- D_row %*% A %*% D_col

    U <- iform@tau * D_row %*% rep(1, ncol(D_row))
    V <- D_col %*% rep(1, nrow(D_col))

    sparseLRMatrix(sparse, U, V)
  }
)

#' @rdname PerturbedLaplacian
#' @export
setMethod(
  "inverse_transform",
  signature = c("PerturbedLaplacian", "sparseLRMatrix"),
  definition = function(iform, A) {
    .NotYetImplemented()
    D_row_inv <- Diagonal(n = nrow(A), x = sqrt(iform@rsA + iform@tau))
    D_col_inv <- Diagonal(n = ncol(A), x = sqrt(iform@csA + iform@tau))
    D_row_inv %*% A@sparse %*% D_col_inv
  }
)


