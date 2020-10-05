setClass(
  "Invertiform",
  contains = "VIRTUAL"
)

setGeneric(
  "transform",
  function(iform, A) standardGeneric("transform")
)

setGeneric(
  "inverse_transform",
  function(iform, A) standardGeneric("inverse_transform")
)
