context("plots")

test_that("freqpolygon", {
  vdiffr::expect_doppelganger(
    "freqpolygon 1",
    freqpolygon(~age | substance, data = HELPrct, v = 35)
  )
}
)
