library(mockr)

test_that('Tests for candid_check.', {
  candidates_with_a_twist = dplyr::select(candidates_with_a_twist, -1)
  expect_equal(candid_check(candidates), TRUE)
  expect_equal(candid_check(candidates_with_a_twist), FALSE)

  mockr::with_mock(age_checker = function(var) TRUE, {
    expect_equal(candid_check(candidates), TRUE)
    expect_equal(candid_check(candidates_with_a_twist), TRUE)
  })
})
