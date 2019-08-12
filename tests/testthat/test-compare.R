testthat::test_that("Compare Mean works", {
  require(mosaicData)
  data(HELPrct)
  set.seed(5)
  # calculate the observed difference
  mean(age ~ sex, data=HELPrct)
  obs <- diffmean(age ~ sex, data=HELPrct); obs
  # calculate the permutation distribution
  nulldist <- do(100) * diffmean(age ~ shuffle(sex),
    data=HELPrct)
  vdiffr::expect_doppelganger("mean1", histogram(~ diffmean, groups=(diffmean >= obs), nulldist,
                                                 xlab="difference in means")) 
  
})

testthat::test_that("Compare Proportion works", {
  require(mosaicData)
  data(HELPrct)
  set.seed(6)
  # calculate the observed difference
  mean(homeless=="housed" ~ sex, data=HELPrct)
  obs <- diffprop(homeless=="housed" ~ sex, data=HELPrct); obs
  # calculate the permutation distribution
  nulldist <- do(100) * diffprop(homeless=="housed" ~ shuffle(sex), data=HELPrct)
  vdiffr::expect_doppelganger("prop1", histogram(~ diffprop, groups=(diffprop>= obs), nulldist,
    xlab="difference in proportions"))
  
})