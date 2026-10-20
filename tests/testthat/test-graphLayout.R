# graphLayout() always launches an interactive Shiny gadget, so these tests
# only exercise the input-validation code paths that run before the app
# would be started; they must not trigger shiny::runGadget().

test_that("an invalid Viewer argument is rejected before launching the app", {
  amat <- matrix(c(0, 1, 0, 0), 2, 2)
  expect_error(
    graphLayout(amat, Viewer = "not-a-real-viewer"),
    "should be one of"
  )
})

test_that("an invalid adjacency matrix is rejected before launching the app", {
  bad <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(graphLayout(bad, Viewer = "browser"), "quadrata")
})
