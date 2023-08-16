data("ObsEst")
a <- Accuracy(obs_est = ObsEst, conf.int = "No")

test_that("accuracy works w/ conf.int = 'No'", {
  expect_s3_class(a, c("PowerSDI.accuracy", "list"))
  expect_length(a, 4)
})

test_that("print.PowerSDI.accuracy() returns a proper summary", {
  x <- utils::capture.output(print(a))
  expect_type(x, "character")
  expect_equal(x[[1]], "      AME     RMSE     dorig      dmod     dref     RQuad")
  expect_equal(x[[2]], " 2.470223 3.144231 0.9718557 0.8386579 0.838266 0.9590222")
})

test_that("plot.PowerSDI.Accuracy works properly", {
  accuracy_a <- plot(a)
  expect_doppelganger("disp-accuracy-a", accuracy_a)
})

rm(a)

test_that("accuracy fails w/ incorrect conf.int value", {
  expect_error(Accuracy(obs_est = ObsEst, conf.int = "POWER"))
})

test_that("accuracy fails w/ conf.int value yes and incorrect sig.level", {
  expect_error(Accuracy(obs_est = ObsEst, conf.int = "Yes", sig.level = 0.96))
})

test_that("accuracy fails w/ to short ObsEst values", {
  short_obs_est <- ObsEst[1:5,]
  expect_error(Accuracy(obs_est = short_obs_est, conf.int = "No"))
  rm(short_obs_est)
})

test_that("accuracy fails w/ `NA` in ObsEst values", {
  na_obs_est <- ObsEst[1:20,]
  na_obs_est[5, 2] <- NA
  expect_error(Accuracy(obs_est = na_obs_est, conf.int = "No"))
  rm(na_obs_est)
})




