#' Normalized graph Laplacian transformation
#'
#' @slot rsA numeric.
#' @slot csA numeric.
#'
#' @export
#'
setClass(
  "NormalizedLaplacian",
  contains = "Invertiform",
  slots = c(
    rsA = "numeric",
    csA = "numeric"
  )
)

#' Construct and use NormalizedLaplacian transformation
#'
#' A convenience function to create [NormalizedLaplacian-class] S4 objects,
#' which are useful for finding the normalized Laplacian of the
#' adjacency matrix of a graph.
#'
#' @details
#'
#' We define the *normalized Laplacian* \eqn{L(A)} of an
#' \eqn{n \times n}{n by n} graph adjacency matrix \eqn{A} as
#'
#' \deqn{
#'   L(A)_{ij} = \frac{A_{ij}}{\sqrt{d^\text{out}_i} \sqrt{d^\text{in}_j}}
#' }{
#'   L[ij] = A[ij] / sqrt(d^out[i] d^in[j])
#' }
#'
#' where
#'
#' \deqn{
#'   d^\text{out}_i = \sum_{j=1}^n \lvert A_{ij} \rvert
#' }{
#'   d^out[i] = sum_j abs(A[ij])
#' }
#'
#' and
#'
#' \deqn{
#'   d^\text{in}_j = \sum_{i=1}^n \lvert A_{ij} \rvert.
#' }{
#'   d^in[j] = sum_i abs(A[ij]).
#' }
#'
#' Note that this documentation renders more clearly at
#' <https://rohelab.github.io/invertiforms/>.
#'
#' @inheritParams transform
#'
#' @return
#'
#'   - `NormalizedLaplacian()` creates a [NormalizedLaplacian-class] object.
#'
#'   - `transform()` returns the transformed matrix,
#'     typically as a [Matrix-class].
#'
#'   - `inverse_transform()` returns the inverse transformed matrix,
#'     typically as a [Matrix-class].
#'
#' @export
#' @include s4-generics.R
#'
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


