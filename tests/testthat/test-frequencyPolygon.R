
# Examples
# freqpolygon(~age | substance, data=HELPrct, v=35)
# freqpolygon(~age, data=HELPrct, labels=TRUE, type='count')
# freqpolygon(~age | substance, data=HELPrct, groups=sex)
# freqpolygon(~age | substance, data=HELPrct, groups=sex, ylim=c(0,0.11))
testthat::test_that("Examples are working",{
 
  vdiffr::expect_doppelganger("FirstExample", freqpolygon(~age | substance, data=HELPrct, v=35))
  
  vdiffr::expect_doppelganger("SecondExample", freqpolygon(~age, data=HELPrct, labels=TRUE, type='count'))
  
  vdiffr::expect_doppelganger("ThirdExample", freqpolygon(~age | substance, data=HELPrct, groups=sex))
  
  vdiffr::expect_doppelganger("FourthExample", freqpolygon(~age | substance, data=HELPrct, groups=sex, ylim=c(0,0.11)))
})