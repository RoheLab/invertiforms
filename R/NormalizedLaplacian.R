#' Title
#'
#' @slot rsA numeric.
#' @slot csA numeric.
#'
#' @return
#' @export
#'
#' @include s4-generics.R
#'
setClass(
  "NormalizedLaplacian",
  contains = "Invertiform",
  slots = c(
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
#' @rdname NormalizedLaplacian
#' @examples
#'
#' library(igraph)
#' library(igraphdata)
#'
#' data("karate", package = "igraphdata")
#'
#' A <- get.adjacency(karate)
#'
#' iform <- NormalizedLaplacian(A)
#'
#' L <- transform(iform, A)
#' L
#'
#' A_recovered <- inverse_transform(iform, L)
#'
#' all.equal(A, A_recovered)
#'
NormalizedLaplacian <- function(A) {

  # don't try to be clever and simplify the degree calculations,
  # there are at least two gotchas:
  #
  #   (1) don't use abs(), which returns a *vector*
  #
  #   (2) Matrix::rowSums() and rowSums() are different

  rsA <- Matrix::rowSums(A * sign(A))  # (absolute) out-degree
  csA <- Matrix::colSums(A * sign(A))  # (absolute) in-degree

  new(
    "NormalizedLaplacian",
    rsA = rsA,
    csA = csA
  )
}


#' @rdname NormalizedLaplacian
#' @export
setMethod(
  "transform",
  signature = c("NormalizedLaplacian", "sparseMatrix"),
  definition = function(iform, A) {
    D_row <- Diagonal(n = nrow(A), x = 1 / sqrt(iform@rsA))
    D_col <- Diagonal(n = ncol(A), x = 1 / sqrt(iform@csA))
    D_row %*% A %*% D_col
  }
)

#' @rdname NormalizedLaplacian
#' @export
setMethod(
  "inverse_transform",
  signature = c("NormalizedLaplacian", "sparseMatrix"),
  definition = function(iform, A) {
    D_row_inv <- Diagonal(n = nrow(A), x = sqrt(iform@rsA))
    D_col_inv <- Diagonal(n = ncol(A), x = sqrt(iform@csA))
    D_row_inv %*% A %*% D_col_inv
  }
)


