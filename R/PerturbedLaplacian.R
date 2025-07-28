#' Perturbed graph Laplacian transformation
#'
#' @slot tau numeric.
#' @slot rsA numeric.
#' @slot csA numeric.
#' @slot tau_choice character.
#'
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


#' Construct and use the Perturbed Laplacian
#'
#' @inheritParams transform
#'
#' @param tau Additive regularizer for row and column sums of `abs(A)`.
#'   Typically this corresponds to inflating the (absolute) out-degree
#'   and the (absolute) in-degree of each node by `tau`. Defaults to
#'   `NULL`, in which case we set `tau` to the mean value of `abs(A)`.
#'
#' @return
#'
#'   - `PerturbedLaplacian()` creates a [PerturbedLaplacian-class] object.
#'
#'   - `transform()` returns the transformed matrix,
#'     typically as a [Matrix::Matrix-class].
#'
#'   - `inverse_transform()` returns the inverse transformed matrix,
#'     typically as a [Matrix::Matrix-class].
#'
#' @export
#'
#'
#' @details
#'
#' We define the *perturbed Laplacian* \eqn{L^\tau(A)}{L_tau(A)} of an
#' \eqn{n \times n}{n by n} graph adjacency matrix \eqn{A} as
#'
#' \deqn{
#'   L^\tau(A)_{ij} = \frac{A_{ij} + \frac{\tau}{n}}{\sqrt{d^{out}_i + \tau}
#'   \sqrt{d^{in}_j + \tau}}
#' }{
#'   L[ij] = (A[ij] + \tau / n) / (sqrt(d^out[i] + \tau)  sqrt(d^in[j] + \tau))
#' }
#'
#' where
#'
#' \deqn{
#'   d^{out}_i = \sum_{j=1}^n \|A_{ij} \|
#' }{
#'   d^out[i] = sum_j abs(A[ij])
#' }
#'
#' and
#'
#' \deqn{
#'   d^{in}_j = \sum_{i=1}^n \|A_{ij} \|.
#' }{
#'   d^in[j] = sum_i abs(A[ij]).
#' }
#'
#' When \eqn{A_{ij}}{A[ij]} denotes the present of an edge *from* node \eqn{i}
#' *to* node \eqn{j}, which is fairly standard notation,
#' \eqn{d^{out}_i}{d^out[i]} denotes the (absolute) out-degree of node
#' \eqn{i} and \eqn{d^{in}_j}{d^in[j]} denotes the (absolute) in-degree
#' of node \eqn{j}.
#'
#' Note that this documentation renders more clearly at
#' <https://rohelab.github.io/invertiforms/>.
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
#' \dontrun{
#' A_recovered <- inverse_transform(iform, L)
#' all.equal(A, A_recovered)
#' }
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


