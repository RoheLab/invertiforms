
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


#' Title
#'
#' @param iform TODO
#' @param A A [Matrix::Matrix()] or [sparseLRMatrix::sparseLRMatrix()] object.
#'
#' @return TODO
#' @export
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
  } else if (inherits(A, "sparseMatrix")) {
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
  signature = c("RegularizedLaplacian", "sparseMatrix"),
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
  signature = c("RegularizedLaplacian", "sparseMatrix"),
  definition = function(iform, A) {
    D_row_inv <- Diagonal(n = nrow(A), x = sqrt(iform@rsA + iform@tau_row))
    D_col_inv <- Diagonal(n = ncol(A), x = sqrt(iform@csA + iform@tau_col))
    D_row_inv %*% A %*% D_col_inv
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
