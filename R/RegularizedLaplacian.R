#' Regularized graph Laplacian transformation
#'
#' @slot tau_row numeric.
#' @slot tau_col numeric.
#' @slot rsA numeric.
#' @slot csA numeric.
#' @slot tau_choice_row character.
#' @slot tau_choice_col character.
#'
#' @export
#' @include s4-generics.R
#'
setClass(
  "RegularizedLaplacian",
  contains = "Invertiform",
  slots = c(
    tau_row = "numeric",
    tau_col = "numeric",
    rsA = "numeric",
    csA = "numeric",
    tau_choice_row = "character",
    tau_choice_col = "character"
  )
)


#' Construct and use the Regularized Laplacian
#'
#' @inheritParams transform
#'
#' @param tau_row Additive regularizer for row sums of `abs(A)`. Typically
#'   this corresponds to inflating the (absolute) out-degree of each node
#'   by `tau_row`. Defaults to `NULL`, in which case we set `tau_row` to
#'   the mean (absolute) row sum of `A`.
#' @param tau_col Additive regularizer for column sums of `abs(A)`. Typically
#'   this corresponds to inflating the (absolute) in-degree of each node
#'   by `tau_col`. Defaults to `NULL`, in which case we set `tau_col` to
#'   the mean (absolute) column sum of `A`.
#'
#' @return
#'
#'   - `RegularizedLaplacian()` creates a [RegularizedLaplacian-class] object.
#'
#'   - `transform()` returns the transformed matrix,
#'     typically as a [Matrix::Matrix-class].
#'
#'   - `inverse_transform()` returns the inverse transformed matrix,
#'     typically as a [Matrix::Matrix-class].
#'
#' @export
#'
#' @details
#'
#' We define the *regularized Laplacian* \eqn{L^\tau(A)}{L_tau(A)} of an
#' \eqn{n \times n}{n by n} graph adjacency matrix \eqn{A} as
#'
#' \deqn{
#'   L^\tau(A)_{ij} = \frac{A_{ij}}{\sqrt{d^{out}_i + \tau_{row}}
#'   \sqrt{d^{in}_j + \tau_{col}}}
#' }{
#'   L[ij] = A[ij] / (sqrt(d^out[i] + \tau_row)  sqrt(d^in[j] + \tau_col))
#' }
#'
#' where
#'
#' \deqn{
#'   d^{out}_i = \sum_{j=1}^n \| A_{ij} \|
#' }{
#'   d^out[i] = sum_j abs(A[ij])
#' }
#'
#' and
#'
#' \deqn{
#'   d^{in}_j = \sum_{i=1}^n \| A_{ij} \|.
#' }{
#'   d^in[j] = sum_i abs(A[ij]).
#' }
#'
#' When \eqn{A_{ij}}{A[ij]} denotes the present of an edge *from* node \eqn{i}
#' *to* node \eqn{j}, which is fairly standard notation,
#' \eqn{d^{out}_i}{d^out[i]} denotes the (absolute) out-degree of node
#' \eqn{i} and \eqn{d^{in}_j}{d^in[j]} denotes the (absolute) in-degree
#' of node \eqn{j}. Then \eqn{\tau_{row}}{\tau_row} is an additive
#' out-degree regularizer and \eqn{\tau_{col}}{\tau_col} is an
#' additive in-degree regularizer.
#'
#' Note that this documentation renders more clearly at
#' <https://rohelab.github.io/invertiforms/>.
#'
#' @rdname RegularizedLaplacian
#' @examples
#'
#' library(igraph)
#' library(igraphdata)
#'
#' data("karate", package = "igraphdata")
#'
#' A <- get.adjacency(karate)
#'
#' iform <- RegularizedLaplacian(A)
#'
#' L <- transform(iform, A)
#' L
#'
#' A_recovered <- inverse_transform(iform, L)
#'
#' all.equal(A, A_recovered)
#'
RegularizedLaplacian <- function(A, tau_row = NULL, tau_col = NULL) {

  # don't try to be clever and simplify the degree calculations,
  # there are at least two gotchas:
  #
  #   (1) don't use abs(), which returns a *vector*
  #
  #   (2) Matrix::rowSums() and rowSums() are different

  if (inherits(A, "sparseLRMatrix")) {

    rsA <- Matrix::rowSums(A@sparse * sign(A@sparse))  # (absolute) out-degree
    csA <- Matrix::colSums(A@sparse * sign(A@sparse))  # (absolute) in-degree

    # do the low rank matrix expansion row by row to avoid instantiating
    # a huge dense matrix

    for (i in 1:nrow(A@sparse)) {
      rsA[i] <- rsA[i] +
        sum(abs(Matrix::tcrossprod(A@U[i, , drop = FALSE], A@V)))
    }

    for (j in 1:ncol(A@sparse)) {
      csA[j] <- csA[j] +
        sum(abs(Matrix::tcrossprod(A@U, A@V[j, , drop = FALSE])))
    }
  } else if (inherits(A, "Matrix") || is.matrix(A)) {
    rsA <- Matrix::rowSums(A * sign(A))  # (absolute) out-degree
    csA <- Matrix::colSums(A * sign(A))  # (absolute) in-degree
  } else {
    .NotYetImplemented()
  }

  if (is.null(tau_row)) {
    tau_row <- mean(rsA)
    tau_choice_row <- "Mean Row Sum [default]"
  } else {
    tau_choice_row <- "User Specified"
  }

  if (is.null(tau_col)) {
    tau_col <- mean(csA)
    tau_choice_col <- "Mean Col Sum [default]"
  } else {
    tau_choice_col <- "User Specified"
  }

  new(
    "RegularizedLaplacian",
    rsA = rsA,
    csA = csA,
    tau_row = tau_row,
    tau_col = tau_col,
    tau_choice_row = tau_choice_row,
    tau_choice_col = tau_choice_col
  )
}

#' @rdname RegularizedLaplacian
#' @export
setMethod(
  "transform",
  signature = c("RegularizedLaplacian", "Matrix"),
  definition = function(iform, A) {
    D_row <- Diagonal(n = nrow(A), x = 1 / sqrt(iform@rsA + iform@tau_row))
    D_col <- Diagonal(n = ncol(A), x = 1 / sqrt(iform@csA + iform@tau_col))
    D_row %*% A %*% D_col
  }
)

#' @rdname RegularizedLaplacian
#' @export
setMethod(
  "transform",
  signature = c("RegularizedLaplacian", "matrix"),
  definition = function(iform, A) {
    D_row <- Diagonal(n = nrow(A), x = 1 / sqrt(iform@rsA + iform@tau_row))
    D_col <- Diagonal(n = ncol(A), x = 1 / sqrt(iform@csA + iform@tau_col))
    as.matrix(D_row %*% A %*% D_col)
  }
)

#' @rdname RegularizedLaplacian
#' @export
setMethod(
  "transform",
  signature = c("RegularizedLaplacian", "sparseLRMatrix"),
  definition = function(iform, A) {
    D_row <- Diagonal(n = nrow(A), x = 1 / sqrt(iform@rsA + iform@tau_row))
    D_col <- Diagonal(n = ncol(A), x = 1 / sqrt(iform@csA + iform@tau_col))

    sparseLRMatrix::sparseLRMatrix(
      sparse = D_row %*% A@sparse %*% D_col,
      U = D_row %*% A@U,
      V = D_col %*% A@V
    )

  }
)


#' @rdname RegularizedLaplacian
#' @export
setMethod(
  "inverse_transform",
  signature = c("RegularizedLaplacian", "Matrix"),
  definition = function(iform, A) {
    D_row_inv <- Diagonal(n = nrow(A), x = sqrt(iform@rsA + iform@tau_row))
    D_col_inv <- Diagonal(n = ncol(A), x = sqrt(iform@csA + iform@tau_col))
    D_row_inv %*% A %*% D_col_inv
  }
)

#' @rdname RegularizedLaplacian
#' @export
setMethod(
  "inverse_transform",
  signature = c("RegularizedLaplacian", "matrix"),
  definition = function(iform, A) {
    D_row_inv <- Diagonal(n = nrow(A), x = sqrt(iform@rsA + iform@tau_row))
    D_col_inv <- Diagonal(n = ncol(A), x = sqrt(iform@csA + iform@tau_col))
    as.matrix(D_row_inv %*% A %*% D_col_inv)
  }
)


# trick to allow S4 dispatch to dispatch on S3 class
setOldClass("vsp_fa")

#' @rdname RegularizedLaplacian
#' @export
setMethod(
  "inverse_transform",
  signature = c("RegularizedLaplacian", "vsp_fa"),
  definition = function(iform, A) {
    fa <- A

    n <- nrow(fa$u)
    d <- nrow(fa$v)

    D_row_inv <- Diagonal(n = n, x = sqrt(iform@rsA + iform@tau_row))
    D_col_inv <- Diagonal(n = d, x = sqrt(iform@csA + iform@tau_col))

    fa$Z <- D_row_inv %*% fa$Z
    fa$Y <- D_col_inv %*% fa$Y
    fa
  }
)
