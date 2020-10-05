dispatch_error <- function(iform, A) {
  stop(
    glue(
      "{class(iform)[1]} objects cannot transform objects of type {class(A)}."
    ),
    call. = FALSE
  )
}
