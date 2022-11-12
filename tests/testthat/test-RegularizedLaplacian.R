library(igraph)
library(igraphdata)

data("karate", package = "igraphdata")

A <- get.adjacency(karate)

test_that("RegularizedLaplacian correctness", {

  iform <- RegularizedLaplacian(A)

  A_tilde <- transform(iform, A)

  expected <- rowSums(A)

  # TODO

  expect_true(TRUE)
})

test_that("RegularizedLaplacian recovery", {

  iform <- RegularizedLaplacian(A)

  A_tilde <- transform(iform, A)

  A_recovered <- inverse_transform(iform, A_tilde)

  expect_true(
    all.equal(A, A_recovered, check.attributes = FALSE)
  )
})
