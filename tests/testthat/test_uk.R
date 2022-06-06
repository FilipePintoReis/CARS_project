context("UK-algortithm")

test_that("Donor-recipient Risk Index Combination", {
})

test_that("Donor recipient age difference", {
  expect_equal(age_diff(dage = 60,
                        cage = 50), -50)
  expect_equal(age_diff(dage = 20,
                        cage = 60), -800)
  expect_equal(age_diff(dage = 20,
                        cage = 20), 0)
  expect_equal(age_diff(dage = 60,
                        cage = 40), -200)
  expect_equal(age_diff(dage = 60,
                        cage = 20), -800)
  expect_equal(age_diff(dage = 18,
                        cage = 69), -1300.5)
})

test_that("blood group B match points", {
  expect_equal(b_blood(dABO = "B",
                       cABO = "O",
                       tier = "B",
                       pts = -1000), 0)
  expect_equal(b_blood(dABO = "O",
                       cABO = "B",
                       tier = "B",
                       pts = -1000), -1000)
  expect_equal(b_blood(dABO = "A",
                       cABO = "A",
                       tier = "A",
                       pts = -1000), 0)
  expect_equal(b_blood(dABO = "O",
                       cABO = "B",
                       tier = "B",
                       pts = -2000), -2000)
  expect_equal(b_blood(dABO = "O",
                       cABO = "AB",
                       tier = "B",
                       pts = -2000), 0)
})

test_that("test for ABO compatibility on UK transplant", {
  expect_true(abo_uk(dABO = "A",
                      cABO = "A",
                      tier = "B"))
  expect_true(abo_uk(dABO = "A",
                     cABO = "AB",
                     tier = "B"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "O",
                     tier = "B"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "B",
                     tier = "B"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "A",
                     tier = "A"))
  expect_true(abo_uk(dABO = "O",
                     cABO = "AB",
                     tier = "A"))
  expect_true(abo_uk(dABO = "B",
                     cABO = "B",
                     tier = "B"))
  expect_true(abo_uk(dABO = "B",
                     cABO = "B",
                     tier = "A"))
  expect_true(abo_uk(dABO = "AB",
                     cABO = "AB",
                     tier = "A"))
  expect_true(abo_uk(dABO = "AB",
                     cABO = "AB",
                     tier = "B"))

  expect_false(abo_uk(dABO = "A",
                     cABO = "O",
                     tier = "B"))
  expect_false(abo_uk(dABO = "B",
                      cABO = "O",
                      tier = "B"))
  expect_false(abo_uk(dABO = "AB",
                      cABO = "B",
                      tier = "B"))
})

# test_that("Points age differences PT's algorithm", {
#   expect_equal(pts_age(dage = 60, cage = 40, pts = 4), 4)
#   expect_equal(pts_age(dage = 60, cage = 40, pts = 6), 6)
#   expect_equal(pts_age(dage = 20, cage = 60, pts = 4), 0)
#   expect_equal(pts_age(dage = 20, cage = 20, pts = 4), 4)
#   expect_equal(pts_age(dage = 70, cage = 70, pts = 4), 4)
# })
