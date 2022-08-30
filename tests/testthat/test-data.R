test_that("lipid_class_order exists", {

  expect_length(lipid_class_order, 71)
  expect_type(lipid_class_order, "character")

})

test_that("test_raw_data exists", {

  expect_length(test_raw_data, 4)
  expect_equal(nrow(test_raw_data), 10764)
  expect_type(test_raw_data, "list")

})

test_that("test_metadata exists", {

  expect_length(test_metadata, 23)
  expect_equal(nrow(test_metadata), 21)
  expect_type(test_metadata, "list")

})
