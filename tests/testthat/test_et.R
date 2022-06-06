context("ET-algortithm")

test_that(" Mismatch Probability (MMP) from ETKAS", {
})

test_that("mmHLA points from ET's algorithm", {

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,1))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 333.33)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 500,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 500)

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,2))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 266.67)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 500,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 500)

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,3))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 200)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 500,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 500)

  mockr::local_mock(mmHLA = function(dA, dB, dDR,
                                     cA, cB, cDR) c(0,0,0,6))
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 0)[['ptsHLA']], 0)
  expect_equal(et_mmHLA(mm0 = 400,
                        mm1 = 333.33,
                        mm2 = 266.67,
                        mm3 = 200,
                        mm4 = 133.33,
                        mm5 = 66.67,
                        mm6 = 500)[['ptsHLA']], 500)
  })


test_that("ET points for time on dialysis (in months)", {
  expect_equal(et_dial(dial = 0, month = 2.78), 0)
  expect_equal(et_dial(dial = 1, month = 2.78), 2.78)
  expect_equal(et_dial(dial = 10, month = 2.78), 27.8)
  expect_equal(et_dial(dial = 100, month = 2.78), 278)
  })

# test_that("Points age differences PT's algorithm", {
#   expect_equal(pts_age(dage = 60, cage = 40, pts = 4), 4)
#   expect_equal(pts_age(dage = 60, cage = 40, pts = 6), 6)
#   expect_equal(pts_age(dage = 20, cage = 60, pts = 4), 0)
#   expect_equal(pts_age(dage = 20, cage = 20, pts = 4), 4)
#   expect_equal(pts_age(dage = 70, cage = 70, pts = 4), 4)
# })
