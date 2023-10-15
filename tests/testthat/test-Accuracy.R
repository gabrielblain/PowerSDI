
## conf.int = "no" ----

a <- Accuracy(obs_est = ObsEst, conf.int = "No")

test_that("accuracy works w/ conf.int = 'No'", {
  expect_s3_class(a, c("PowerSDI.accuracy", "list"))
  expect_length(a, 4)
})

test_that("print.PowerSDI.accuracy() returns a proper summary w/ ci no", {
  x <- utils::capture.output(print(a))
  expect_type(x, "character")
  expect_equal(x[[1]], "      AME     RMSE     dorig      dmod     dref     RQuad")
  expect_equal(x[[2]], " 2.470223 3.144231 0.9718557 0.8386579 0.838266 0.9590222")
})

test_that("plot.PowerSDI.Accuracy works properly w/ ci no", {
  accuracy_a <- plot(a)
  vdiffr::expect_doppelganger("disp-accuracy-a-no", accuracy_a)
})

rm(a)

## conf.int = "yes" ----
b <- Accuracy(obs_est = ObsEst, conf.int = "Yes")

test_that("accuracy works w/ conf.int = 'Yes'", {
  expect_s3_class(b, c("PowerSDI.accuracy", "list"))
  expect_length(b, 4)
})

test_that("plot.PowerSDI.Accuracy works properly w/ ci yes", {
  accuracy_b <- plot(b)
  vdiffr::expect_doppelganger("disp-accuracy-b-yes", accuracy_b)
})


test_that("print.PowerSDI.accuracy() returns a proper summary w/ ci yes", {
  expect_type(b[[3]], "double")
  expect_equal(dimnames(b[[3]]),
               list(
                 "",
                 c(
                   "AMECIinf",
                   "AME",
                   "AMECIsup",
                   "RMSECIinf",
                   "RMSE",
                   "RMSECIsup",
                   "dorig_CIinf",
                   "dorig",
                   "dorigCIsup",
                   "dmodCIinf",
                   "dmod",
                   "dmodCIsup",
                   "dref_CIinf",
                   "dref",
                   "dref_CIsup",
                   "RQuad_CIinf",
                   "RQuad",
                   "RQuad_CIsup"
                 )
               ))
  expect_equal(
    b[[3]][1:9],
    c(
      2.467015,
      2.470223,
      2.473082,
      3.139687,
      3.144231,
      3.147687,
      0.9717416,
      0.9718557,
      0.9719249
    ),
    tolerance = 0.01
  )
})

rm(b)

test_that("accuracy fails w/ incorrect conf.int value", {
  expect_error(Accuracy(obs_est = ObsEst, conf.int = "POWER"))
})

test_that("accuracy fails w/ conf.int value yes and incorrect sig.level", {
  expect_error(Accuracy(
    obs_est = ObsEst,
    conf.int = "Yes",
    sig.level = 0.96
  ))
})

test_that("accuracy fails w/ to short ObsEst values", {
  short_obs_est <- ObsEst[1:5, ]
  expect_error(Accuracy(obs_est = short_obs_est, conf.int = "No"))
  rm(short_obs_est)
})

test_that("accuracy fails w/ `NA` in ObsEst values", {
  na_obs_est <- ObsEst[1:20, ]
  na_obs_est[5, 2] <- NA
  expect_error(Accuracy(obs_est = na_obs_est, conf.int = "No"))
  rm(na_obs_est)
})
