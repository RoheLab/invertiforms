library(igraph)
library(igraphdata)

data("karate", package = "igraphdata")

A <- get.adjacency(karate)


test_that("DoubleCenter correctness", {

  iform <- DoubleCenter(A)

  A_tilde <- transform(iform, A)

  # TODO
  # L <- sweep(L, 1, Matrix::rowMeans(L))
  # L <- sweep(L, 2, Matrix::colMeans(L))
  # L

  expect_true(TRUE)
})

test_that("DoubleCenter recovery", {

  iform <- DoubleCenter(A)

  A_tilde <- transform(iform, A)

  A_recovered <- inverse_transform(iform, A_tilde)

  expect_true(
    all.equal(A, A_recovered)
  )
})
