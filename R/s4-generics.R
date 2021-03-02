#' An abstract S4 class representing an invertible transformation
setClass(
  "Invertiform",
  contains = "VIRTUAL"
)

#' Apply an invertible transformation
#'
#' @param iform An [Invertiform-class] object describing the transformation.
#' @param A A matrix to transform.
#'
#' @return The transformed matrix.
#' @export
setGeneric(
  "transform",
  function(iform, A) standardGeneric("transform")
)

#' Apply the inverse of an invertible transformation
#'
#' @param iform An [Invertiform-class] object describing the transformation.
#' @param A A matrix to inverse transform.
#'
#' @return The inverse transformed matrix.
#' @export
setGeneric(
  "inverse_transform",
  function(iform, A) standardGeneric("inverse_transform")
)
