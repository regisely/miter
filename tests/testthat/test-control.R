test_that("control_miter creates valid control object", {
  ctrl <- control_miter()
  expect_s3_class(ctrl, "control_miter")
  expect_true(ctrl$progress)
  expect_false(ctrl$verbose)
})

test_that("control_miter_race creates valid control object", {
  ctrl_race <- control_miter_race()
  expect_s3_class(ctrl_race, "control_miter")
  expect_equal(ctrl_race$burn_in, 3)
  expect_equal(ctrl_race$alpha, 0.05)
})

test_that("control parameters can be customized", {
  ctrl <- control_miter(progress = FALSE, verbose = TRUE)
  expect_false(ctrl$progress)
  expect_true(ctrl$verbose)
})
