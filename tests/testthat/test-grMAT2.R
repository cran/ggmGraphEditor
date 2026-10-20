test_that("a square matrix is returned unchanged", {
  amat <- matrix(c(0, 1, 0, 0), 2, 2)
  expect_identical(grMAT2(amat), amat)
})

test_that("a non-square matrix is rejected", {
  bad <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(grMAT2(bad), "quadrata")
})

test_that("a character vector is converted to a mixed adjacency matrix", {
  vec <- c("a", 1, 2, "l", 2, 3, "b", 1, 3)
  m <- grMAT2(vec)

  expect_true(is.matrix(m))
  expect_identical(rownames(m), c("1", "2", "3"))
  expect_identical(colnames(m), c("1", "2", "3"))

  # "a" edge 1 -> 2
  expect_equal(m["1", "2"], 1)
  expect_equal(m["2", "1"], 0)

  # "l" edge 2 - 3 is symmetric
  expect_equal(m["2", "3"], 10)
  expect_equal(m["3", "2"], 10)

  # "b" edge 1 <-> 3 is symmetric
  expect_equal(m["1", "3"], 100)
  expect_equal(m["3", "1"], 100)
})

test_that("isolated nodes declared with '*' are included with all-zero rows", {
  vec <- c("a", 1, 2, "*", 3, 3)
  m <- grMAT2(vec)

  expect_true("3" %in% rownames(m))
  expect_equal(unname(m["3", ]), c(0, 0, 0))
  expect_equal(unname(m[, "3"]), c(0, 0, 0))
})

test_that("overlapping edges of different types accumulate", {
  vec <- c("a", 1, 2, "l", 1, 2, "b", 1, 2)
  m <- grMAT2(vec)

  expect_equal(m["1", "2"], 1 + 10 + 100)
  expect_equal(m["2", "1"], 10 + 100)
})

test_that("a vector of invalid length is rejected", {
  expect_error(grMAT2(c("a", 1, 2, "l")), "valid form")
})

test_that("an invalid edge type is rejected", {
  expect_error(grMAT2(c("z", 1, 2)), "valid form")
})

test_that("an igraph object is converted via its adjacency matrix", {
  skip_if_not_installed("igraph")
  g <- igraph::graph_from_edgelist(matrix(c(1, 2), ncol = 2))
  m <- grMAT2(g)

  expect_true(is.matrix(m))
  expect_equal(dim(m), c(2, 2))
})

test_that("unsupported input classes are rejected", {
  expect_error(grMAT2(list(1, 2)), "graphNEL, igraph, or character")
})
