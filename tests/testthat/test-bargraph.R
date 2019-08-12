testthat::test_that("bargraph works", {
  
  require(mosaicData) 
  data(HELPrct)
  HELPrct2 <- mutate(HELPrct, older = age > 40)
  
  vdiffr::expect_doppelganger("bargraph1", bargraph( ~ substance, data = HELPrct))
  vdiffr::expect_doppelganger("bargraph2", bargraph( ~ substance, data = HELPrct, horizontal = TRUE))
  vdiffr::expect_doppelganger("bargraph3", bargraph( ~ substance | sex, groups = homeless, auto.key = TRUE, data = HELPrct))
  vdiffr::expect_doppelganger("bargraph4", bargraph( ~ substance, groups = homeless, auto.key=TRUE,
                                                     data = HELPrct %>% filter(sex == "male")))
  vdiffr::expect_doppelganger("bargraph5", bargraph( ~ substance | older, data = HELPrct2))
})