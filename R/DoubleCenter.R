#' Row and column centering transformation
#'
#' @slot row_means numeric.
#' @slot col_means numeric.
#' @slot overall_mean numeric.
#'
#' @export
#'
setClass(
  "DoubleCenter",
  contains = "Invertiform",
  slots = c(
    row_means = "numeric",
    col_means = "numeric",
    overall_mean = "numeric"
  )
)

#' Construct and use DoubleCenter transformations
#'
#' A convenience function to create [DoubleCenter-class] S4 objects,
#' which are useful for **simultaneously row and column centering**
#' a matrix.
#'
#' @inheritParams transform
#'
#' @return
#'
#'   - `DoubleCenter()` creates a [DoubleCenter-class] object.
#'
#'   - `transform()` returns the transformed matrix,
#'     typically as a [sparseLRMatrix::sparseLRMatrix-class].
#'
#'   - `inverse_transform()` returns the inverse transformed matrix,
#'     typically as a [sparseLRMatrix::sparseLRMatrix-class] in most cases.
#'     When possible reduces the [sparseLRMatrix::sparseLRMatrix-class] to a
#'     [Matrix::sparseMatrix()].
#'
#' @export
#' @rdname DoubleCenter
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
#' iform <- DoubleCenter(A)
#'
#' A_tilde <- transform(iform, A)
#' A_recovered <- inverse_transform(iform, A_tilde)
#'
#' all.equal(A, A_recovered)
#'
DoubleCenter <- function(A) {
  new(
    "DoubleCenter",
    row_means = rowMeans(A),
    col_means = colMeans(A),
    overall_mean = mean(A)
  )
}


#' @rdname DoubleCenter
#' @export
setMethod(
  "transform",
  signature = c("DoubleCenter", "sparseMatrix"),
  definition = function(iform, A) {

    U <- cbind(
      -iform@row_means,
      rep(1, length(iform@row_means)),
      iform@overall_mean * rep(1, length(iform@row_means))
    )

    V <- cbind(
      rep(1, length(iform@col_means)),
      -iform@col_means,
      rep(1, length(iform@col_means))
    )

    sparseLRMatrix(A, U, V)
  }
)

#' @rdname DoubleCenter
#' @export
setMethod(
  "inverse_transform",
  signature = c("DoubleCenter", "sparseLRMatrix"),
  definition = function(iform, A) {

    U <- cbind(
      -iform@row_means,
      rep(1, length(iform@row_means)),
      iform@overall_mean * rep(1, length(iform@row_means))
    )

    V <- cbind(
      rep(1, length(iform@col_means)),
      -iform@col_means,
      rep(1, length(iform@col_means))
    )

    if (isTRUE(all.equal(A@U, Matrix(U), check.attributes = FALSE)) &&
        isTRUE(all.equal(A@V, Matrix(V), check.attributes = FALSE)))
      return(A@sparse)

    A@U <- cbind(A@U, -U)
    A@V <- cbind(A@V, V)

    A
  }
)

# trick to allow S4 dispatch to dispatch on S3 class
setOldClass("vsp_fa")

#' @rdname DoubleCenter
#' @export
setMethod(
  "inverse_transform",
  signature = c("DoubleCenter", "vsp_fa"),
  definition = function(iform, A) {

    # TODO: this needs major sanity checking
    fa <- A

    n <- nrow(fa$u)
    d <- nrow(fa$v)

    D_inv <- Diagonal(n = length(fa$d), x = 1 / fa$d)

    mu_z <- sqrt(n) * iform@col_means %*% fa$v %*% D_inv %*% fa$R_U
    mu_y <- sqrt(d) * iform@row_means %*% fa$u %*% D_inv %*% fa$R_V

    fa$Z <- sweep(fa$Z, 2, mu_z, "+")
    fa$Y <- sweep(fa$Y, 2, mu_y, "+")

    fa
  }
)


