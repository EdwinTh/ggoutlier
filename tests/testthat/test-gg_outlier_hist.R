context("gg_outlier_hist - input checking")

test_that("gg_outlier_hist throws informative error when var_name not in x", {
  expect_error(gg_outlier_hist(mtcars, "Adriaantje", 42),
               "Adriaantje is not a column in x")
  expect_error(gg_outlier_hist(mtcars, "disp", 100), NA)
})

test_that("gg_outlier_hist throws informative for cut_offs", {
  expect_error(gg_outlier_hist(mtcars, "disp"),
               "Neither cut_off_floor, nor cut_off_ceiling are specified")
  expect_error(gg_outlier_hist(mtcars, "disp", 42, 41),
               "cut_off_floor should be smaller than cut_off_ceiling")
  expect_error(gg_outlier_hist(mtcars, "disp", 42, 42),
               "cut_off_floor should be smaller than cut_off_ceiling")
  expect_error(gg_outlier_hist(mtcars, "disp", 100, 200), NA)
  expect_error(gg_outlier_hist(mtcars, "disp", cut_off_floor = 20),
               "cut_off_floor lower than the lowest value in disp")
  expect_error(gg_outlier_hist(mtcars, "disp", cut_off_floor = 500),
               "cut_off_floor higher than the highest value in disp")
  expect_error(gg_outlier_hist(mtcars, "disp", cut_off_ceiling = 500),
               "cut_off_ceiling higher than the highest value in disp")
})


context("gg_outlier_hist - get_printing_min_max")

test_that("get_printing_min_max gives correct output", {
  mm <- get_printing_min_max(mtcars, "disp")
  expect_equal(length(mm), 2)
  expect_named(mm, c("min", "max"))
  expect_equal(unname(mm[1]), 71.1)
  expect_equal(unname(mm[2]), 472)
})

test_that("get_printing_min_max works with NA values", {
  mtcars_copy <- mtcars
  mtcars_copy$disp[1:3] <- NA
  expect_equal(get_printing_min_max(mtcars, "disp"),
               get_printing_min_max(mtcars_copy, "disp"))
})

context("gg_outlier_hist - filter_regular_x")

test_that("filter_regular_x gives the correct output", {
  x <- data.frame(a = 1:10, b = 1:10)
  expect_equal(filter_regular_x(x, "b", 5),
               x[6:10, 2, drop = FALSE])
  expect_equal(filter_regular_x(x, "b", cut_off_ceiling = 6),
               x[1:5, 2, drop = FALSE])
  expect_equal(filter_regular_x(x, "b", 2, 6),
               x[3:5, 2, drop = FALSE])
})
