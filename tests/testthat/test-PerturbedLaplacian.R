library(igraph)
library(igraphdata)

data("karate", package = "igraphdata")

A <- get.adjacency(karate)

test_that("PerturbedLaplacian correctness", {

  iform <- PerturbedLaplacian(A)

  A_tilde <- transform(iform, A)

  expected <- rowSums(A)

  # TODO

  expect_true(TRUE)
})

test_that("PerturbedLaplacian recovery", {

  skip("Not yet implemented.")

  iform <- PerturbedLaplacian(A)

  A_tilde <- transform(iform, A)

  A_recovered <- inverse_transform(iform, A_tilde)

  expect_true(
    all.equal(A, A_recovered)
  )
})
