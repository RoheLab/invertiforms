
#' Title
#'
#' @slot row_means numeric.
#' @slot col_means numeric.
#' @slot overall_mean numeric.
#'
#' @return
#' @export
#'
#' @include s4-generics.R
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

#' Title
#'
#' @param iform TODO
#' @param A TODO
#'
#' @return TODO
#' @export
#'
#' @rdname DoubleCenter
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
#' A_tilde
#'
#' A_recovered <- inverse_transform(iform, A_tilde)
#'
#' all.equal(A, A_recovered)
#'
DoubleCenter <- function(A, tau = NULL) {
  new(
    "DoubleCenter",
    row_means = Matrix::rowMeans(A),
    col_means = Matrix::colMeans(A),
    overall_mean = Matrix::mean(A)
  )
}

#' @rdname DoubleCenter
#' @importFrom sparseLRMatrix sparseLRMatrix
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


