box::use(
  gsignal[fir1, freqz, filter],
  testthat[...],
  checkmate,
  rwavelet[MakeONFilter]
)
box::use(
  app/logic/utils[
    create_FIR_filter,
    create_WT_filter,
    ],
)

# create_FIR_filter ------------------------------------------------------------
# Test expected outcome
test_that("create_FIR_filter: returns expected outcome", {
  filter_low <- create_FIR_filter(n = 10, w = 10, f_s = 400, type = "low")
  filter_pass <- create_FIR_filter(n = 10, w = c(5, 10), f_s = 400, type = "pass")
  expect_equal(filter_low, fir1(10, 0.05, "low"))
  expect_equal(filter_pass, fir1(10, c(0.025, 0.05), "pass"))
})

# Test invalid n argument
test_that("create_FIR_filter: invalid n param is handled", {
  expect_error(create_FIR_filter(n = 0, w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = -1, w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 2.5, w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = NULL, w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = "", w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = NA, w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = NaN, w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = c(1, 2), w = 10, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = "invalid", w = 10, f_s = 400, type = "low"))
})

# Test invalid w argument for low- and high-pass filters
test_that("create_FIR_filter: invalid w param for type 'low' and 'high' is handled", {
  expect_error(create_FIR_filter(n = 10, w = -1, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = -1, f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = NULL, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = NULL, f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = "", f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = "", f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = NA, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = NA, f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = NaN, f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = NaN, f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = "invalid", f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = "invalid", f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = c(10, 20), f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = c(10, 20), f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = c(), f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = c(), f_s = 400, type = "high"))
  expect_error(create_FIR_filter(n = 10, w = c(1, 2, 3), f_s = 400, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = c(1, 2, 3), f_s = 400, type = "high"))
})

# Test invalid w argument for stop- and pass-band filters
test_that("create_FIR_filter: invalid w param for type 'stop' and 'pass' is handled", {
  expect_error(create_FIR_filter(n = 10, w = c(-1, 0), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(-1, 0), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(NULL, NULL), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(NULL, NULL), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c("", ""), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c("", ""), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c("in", "in"), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c("in", "in"), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(NA, NA), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(NA, NA), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(0, 400.01), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(0, 400.01), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(0.01, -0.01), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(0.01, -0.01), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(200, 100), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(200, 100), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(0, 0), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(0, 0), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(400, 400), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(400, 400), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(200, 300), f_s = 200, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(200, 300), f_s = 200, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(), f_s = 400, type = "pass"))
  expect_error(create_FIR_filter(n = 10, w = c(1, 2, 3), f_s = 400, type = "stop"))
  expect_error(create_FIR_filter(n = 10, w = c(1, 2, 3), f_s = 400, type = "pass"))
})

# Test invalid f_s argument
test_that("create_FIR_filter: invalid f_s param is handled", {
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = "invalid", type = "low"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 0, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = -1, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = NULL, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = NA, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = NaN, type = "low"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 0.9, type = "low"))
})

# Test invalid type argument
test_that("create_FIR_filter: invalid type param is handled", {
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = "invalid"))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = 1))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = NA))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = NULL))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = NaN))
  expect_error(create_FIR_filter(n = 10, w = 10, f_s = 400, type = c("low", "high")))
})

# create_WT_filter -------------------------------------------------------------
# Test expected outcome
test_that("create_WT_filter: returns expected outcome", {
  expect_equal(
    create_WT_filter(type = "Haar", type_ver = 2),
    MakeONFilter(Type = "Haar", Par = 2)
  )
  expect_equal(
    create_WT_filter(type = "Beylkin", type_ver = 2),
    MakeONFilter(Type = "Beylkin", Par = 2)
  )
  expect_equal(
    create_WT_filter(type = "Coiflet", type_ver = 4),
    MakeONFilter(Type = "Coiflet", Par = 4)
  )
  expect_equal(
    create_WT_filter(type = "Daubechies", type_ver = 6),
    MakeONFilter(Type = "Daubechies", Par = 6)
  )
  expect_equal(
    create_WT_filter(type = "Symmlet", type_ver = 8),
    MakeONFilter(Type = "Symmlet", Par = 8)
  )
  expect_equal(
    create_WT_filter(type = "Vaidyanathan", type_ver = 8),
    MakeONFilter(Type = "Vaidyanathan", Par = 8)
  )
  expect_equal(
    create_WT_filter(type = "Battle", type_ver = 3),
    MakeONFilter(Type = "Battle", Par = 3)
  )
})

# Test invalid type argument
test_that("create_WT_filter: invalid 'type' param is handled", {
  expect_error(create_WT_filter(type = "invalid", type_ver = 2))
  expect_error(create_WT_filter(type = c("Haar", "Battle"), type_ver = 2))
  expect_error(create_WT_filter(type = c("Haar", "Haar"), type_ver = 2))
  expect_error(create_WT_filter(type = NA, type_ver = 2))
  expect_error(create_WT_filter(type = NaN, type_ver = 2))
  expect_error(create_WT_filter(type = NULL, type_ver = 2))
  expect_error(create_WT_filter(type = 1, type_ver = 2))
  expect_error(create_WT_filter(type = "", type_ver = 2))
  expect_error(create_WT_filter(type = "Daubechiess", type_ver = 2))
})

# Test invalid type_ver argument
test_that("create_WT_filter: invalid 'type_ver' param is handled", {
  expect_error(create_WT_filter(type = "Daubechies", type_ver = -1))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 0))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 1))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 1.5))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 3))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 5))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 7))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 9))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 11))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 13))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 15))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 17))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 19))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 21))
  expect_error(create_WT_filter(type = "Daubechies", type_ver = 21.5))
  expect_error(create_WT_filter(type = "Coiflet", type_ver = -1))
  expect_error(create_WT_filter(type = "Coiflet", type_ver = 0))
  expect_error(create_WT_filter(type = "Coiflet", type_ver = 1.5))
  expect_error(create_WT_filter(type = "Coiflet", type_ver = 6))
  expect_error(create_WT_filter(type = "Coiflet", type_ver = 6.5))
  expect_error(create_WT_filter(type = "Symmlet", type_ver = -1))
  expect_error(create_WT_filter(type = "Symmlet", type_ver = 0))
  expect_error(create_WT_filter(type = "Symmlet", type_ver = 1))
  expect_error(create_WT_filter(type = "Symmlet", type_ver = 4.5))
  expect_error(create_WT_filter(type = "Symmlet", type_ver = 11))
  expect_error(create_WT_filter(type = "Battle", type_ver = -1))
  expect_error(create_WT_filter(type = "Battle", type_ver = 0))
  expect_error(create_WT_filter(type = "Battle", type_ver = 1.5))
  expect_error(create_WT_filter(type = "Battle", type_ver = 2))
  expect_error(create_WT_filter(type = "Battle", type_ver = 4))
  expect_error(create_WT_filter(type = "Battle", type_ver = 6))
  expect_error(create_WT_filter(type = "Vaidyanathan", type_ver = -10))
  expect_error(create_WT_filter(type = "Vaidyanathan", type_ver = c(2, 3)))
})
