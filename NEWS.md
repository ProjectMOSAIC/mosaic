# mosaic (development version)

# mosaic 1.8.4

* Bug fix in `mplot.lm()` for models where `broom::tidy()` doesn't record residuals.
* Due to changes in `mosaicCore`, `makeFun()` now accepts one-sided formulas such
  as `makeFun(~ x^2)`.
* Distribution plotting functions can now handle distributions for which supplying
  `p = 0` or `p = 1` to the q-function throws an error.  [See #779]
* Some previously imported packages are now only suggested.  Users will be prompted
  to install packages that are missing only when they are used.

# mosaic 1.8.3

* Addressing CRAN issues with use of vdiffr and order().
* A couple small bug fixes.
* Introduction of a new functions: `compare()`, and `design_plot()`.
* `mplot(model, which = 1)` now uses raw residuals rather than standardized/studentized. This mathes
behavior in `plot()`.

# mosaic 1.8.0

* Fixed dplyr 1.0.0 failure by stabilizing types
* Fixed a few bugs leftover from migration from tidyeval to rlang
* Moved several vignettes out of the package to reduce size of package.  These are 
  still available at <https://www.mosaic-web.org/mosaic/>

# mosaic 1.6.0

 * Added `na.rm` argument to prop.test().
 * Changed return type for `qdata()` so that it is always a named vector.
 * Changed return type for `cdata()` so that is is always a data frame.  Also changed 
 names to "lo" and "hi".
 * Fixed bug in `xpchisq()` caused by introducing explicit arguments and failing
 to retain `...`.  (Issue #737)
 * Fixed bug in `xpt()` caused by introducing explicit arguments and failing
 to handle missing `ncp` correctly.  (Issue #736)

# mosaic 1.5.0

 * `googleMap()` has be deprecated due to change in policy at google. Try `leaflet_map()` as an alternative.
 * Improved documentation about naming when using `do()`.
 * Functions like `xpt()`, `xqt()`, etc. now have more explicit arguments.  This provides additional help and prompts for the user.
 * `percs()` and `counts()` are re-exported from `mosaicCore`
 * When using `confint()`, attempting to set the confidence level using `conf.level` instead of `level` throws and error and provides a reminder to use `level` for that purpose. 
 * `confint()` methods for `binom.test()` have been modified a bit.  See documentation for how names map to methods.
 * Several supporting documents have been update/improved. `ggformula` is used
 for plotting in more places (replacing older `lattice` code).
 * Bug fix: `CIdata()` now handles negative numbers correctly.
 
# mosaic 1.4.0

 * Bug fix: `mplot.lm()` now removed points with leverage 1 to avoid errors and warnings; a 
   warning messages notifies which points have been removed.
 * Bug fix: `TukeyHSD()` now correctly follows `system = "gg"`
 * `mplot.lm()` now uses `ggrapel` to place labels and offers additional controls for 
   the smooth curve that is overlaid. [gg version of plots only]
 * viridis color palette used in some plots to improve readability for color-blind viewers.
 * `orrr()`, `oddsRatio()`, and `relrisk()` now accept a 2x2 data frame to match claims in
   documentation.
 * Support added for `cor(~y, ~x)`
 * New website: http://www.mosaic-web.org/mosaic/
   
# mosaic 1.3.0

 * Bug fix in `prop.test()` so it handles `success` argument properly for 2-way tables.
 * Updated template documents now use `ggformula`.
 * `which` argument added to `mplot.TukeyHSD()`.
 * Updated vignettes to reflect new or updated resources and to use `ggformula`.
 * Some changes implemented to make `mosaic` compatible with `ggplot2` version 3.0.
 
# mosaic 1.2.0

 * A few more plots now default to using `ggplot2` rather than `lattice` by default.
 * Some long deprecated functions are now defunct.
 * `cnorm()`, `ct()`, `xcnorm()`, and `xct()` added to find central portions of distributions.
 * several functions have been moved to `mosaicCore`
 * A number of bug fixes and behavior enhancements.

# mosaic 1.1.0

 * A few more things have moved to `mosaicCore`.
 * Two of the vignettes have been moved out of the package to reduce CRAN size.
 * Several plots now default to using `ggplot2` rather than `lattice`.
 * Improvements to `mplot()` on linear models when system = `"gg"`.
 * Work-around added to avoid the old work-around no longer needed due to the updated `formals()`.
 
# mosaic 1.0.0

 * `xpnorm()` and friends now use `ggplot2` and can return the plot object, if requested.
 * `t.test()` has been completely reimplemented.  It no longer supports "bare variable mode",
 but it is more similar to `stats::t.test()` in some cases.
 * `gwm()` has been removed since it no longer works with the current version of `dplyr`.  
 We anticipate a better collection of modeling utilities in the forthcoming `mosaicModel` package.
 * `props()` and `counts()` have been added.  They are a bit like `tally()` but designed to
 play well with `df_stats()`. Currently the formula versions drop missing data, but that will
 likely be determined by a user-supplied option in the future.
 * Calculus functions have been moved to `mosaicCalc`.
 * `mosaic` depends on `ggformula`, so users will have `lattice`, `ggplot2`, and `ggformula`
 available after loading `mosaic`.
 * `mplot()` on a data frame supports `ggformula` now.
 * A vignette showing "minimal R" with `ggformula` has been added.
 * A vignette comparing `lattice` and `ggformula` has been added.
 * Some functions have been move from `mosaic` to `mosaicCore`.  This should not affect users of 
 `mosaic`.
 
 
# mosaic 0.14.4

 * Tweaks to `tally()` now provide names to dimnames in cases where they were previously missing.
 This was needed for the refactoring of `bargaph()`.
 * Refactored `bargraph()` to use `tally()` for tabulation.  This means the behavior of `bargraph()` should match expectations of users of `tally()` better than it did before. In particular, proportions now sum to 1 in each panel of a multi-panel plot.
 * Bug/Feature fix: Definition of "conditional" is now tighter in `tally()` so the proportions
 computed when `format = "proportion"` are easier to predict.
 * Bug Fix: `prop(x ~ y)` was reporting overall proportions rather than marginal proportions.
 * Made CIsim() more flexible.  It is now easier to run multiple simulations
 at once and compare the results.  Also, non-covering intervals are now classified
 as missing high or missing low, so one can investigate non-symmetric failure to cover.
 * Added option to plotFun() for creating non-interactive 
 surface plots 
 * Added `value()`, a generic with several methods for extracting a "value" from a more complicated object.  Useful for extracting values
 from output of `uniroot()`, `nlm()`, `integrate()`, `cubature::adaptIntegrat()` without needing to know just how those values are stored in the object.
 * Replaced use of reshape2 with functions from tidyr to remove dependency on reshape2
 * Add spline model as option to mplot.data.frame()
 
 
# mosaic 0.14.1

 * Bug fix that caused `prop(a ~ b)` to compute joint rather than conditional proportions.
 * add stat and geom for spline smoothing
 
# mosaic 0.14.0

 * Aggregating functions (`favstats()`, `mean()`, `sd()`, etc.) now require that the 
 first argument be a formula.  This
 was always the preferred method, but some functions allowed bare variable names
 to be used instead.  As a specific example, the following code now generates an
 error (unless there is another object named `age` in your environment).
 
```
favstats(age, data = HELPrct)
## Error in typeof(x) : object 'age' not found
```
Replace this with
```
favstats( ~ age, data = HELPrct)
##  min Q1 median Q3 max     mean       sd   n missing
##   19 30     35 40  60 35.65342 7.710266 453       0
```


 * A new shiny doc template has been added
 * A geom and stat have been added for creating average shifted histograms (ASH plots) using `ggplot2`.
 * Improvements to `mplot.data.frame()` allow it to work with an expression that evaluates to a data frame. ASH plots are now a choice for 1-variable plots.
 * `deltaMethod()` has been moved to a separate package (called `deltaMethod`) to reduce package dependencies
 * `cull_for_do.lm()` now returns a data frame instead of a vector.  This makes it easier for `do()` to bind things together by column name.
 * `makeMap()` updated to work with new version of `ggplot2`.
 
 
# mosaic 0.13.0

 * Arguments to `cdata()`, `ddata()`, `pdata()`, `qdata()` and `rdata()` have been reordered 
 so that the formula comes first.
 * The print method for objects created by `rflip()` has been improved.
 * Bug fix in `dfapply()`, also default value for `select` changed to `TRUE`.
 * Introduce `inspect()`, which is primarily intended to give an over view of the variables in a data frame, but handles some additional objects as well.
  
# mosaic 0.12.0

 * Aggregating functions now generate user friendly errors when the `data` argument is not an environment or data frame.
 * We have fixed some bugs that arose in the "emergency" release of 0.11
 * `mm()` has been deprecated and replaced with `gwm()` which does groupwise models where the response may be either categorical or quantitative.
 * Improvements have been made to `plotModel()`.  This is likely still not the final version,
 but we are getting closer.
 * Improvements have been made to naming in objects created with `do()`.
 * Dots in `dotPlot()` are now the same size in all panels of multi-panel plots.
 * `cdist()` has been rewritten.
 * 
# mosaic 0.11.0

 * `mplot()` on a data frame now (a) prompts the user for the type of plot to create and (b) has an added option to make line plots for time series and the like.
 * `resample()` can now do residual resampling from a linear model.
 * Improvements have been made that make it easier to use `do()` to create common 
 bootstrap confidence intervals.  In particular, `confint()` can now 
 calculate three kinds of intervals in many common situations.
 * `fetchData()`, `fetchGoogle()`, and `fetchGapminder()` have been moved to a 
 separate package, called `fetch()`.
 * `plotModel()` can be used to show data and model fits for a variety of models
 created with `lm()` or `glm()`.
 
 

# mosaic 0.10.0

 * At the request of several users, and with CRAN's approval, we have made 
 `mosaicData` a dependency of `mosaic`.  This avoids the problem of users 
 forgetting to separately load the `mosaicData` package.
 * We are planning to remove `fetchGoogle()` (and perhaps `read.file()`) from future versions 
 of the package.  More and more packages are providing utilities for bringing data into R and it
 doesn't make sense for us to duplicate those efforts in this package.  For google sheets, you
 might take a look at the `googlesheets` package which is available via github now and will be 
 on CRAN soon.
 * Improved output to `binom.test()`, `prop.test()`, and `t.test()`, which have
 also undergone some internal restructuring.  The objects returned now do a 
 better job of reporting about the test conducted.  In particular,
 `binom.test()` and `prop.test()` will report the value of `success` used.(#450, #455)
 * `binom.test()` can now compute several different kinds of confidence intervals including the Wald, Plus-4 and Agresti-Coull intervals.  (#449)
 * `derivedFactor()` now handles NAs without throwing a warning.  (#451)
 * Improved `pdist()`, `pdist()` and related functions now do a better (i.e., useful) job with discrete distributions (#417)
 * Bug fixes in several functions that use non-standard evaluation improve their robustness and scope.  This affects `t.test()` and all the "aggregating" functions like `mean()` and `favstats()`.  In particular, it is now possible to reference variables both in the `data` argument and in the calling environment.  (#435)
 * `CIAdata()` now provides a message indicating the source URL for the data retrieved (#444)
 * Bug fixes to `CIAdata()` that seem to be related to a changed in file format at
 the CIA World Factbook website.  The "inflation" data set is still broken (on the CIA website).  (#441)
 * `read.file()` now uses functions from `readr` in some cases.  A message is produced indicating which reader is being used.  There are also some API changes.  In particular, character data will be returned as character rather than factor.  See `factorize()` for an easy way to convert things with few unique values into factors. (#442)
 * A major vignette housecleaning has happened.  Some vignettes have been removed from the package and vignettes inside the package are now compiled as part of package building to allow for more consistent checking of vignette contents.  "Less Volume, More Creativity" has been reformatted from slides into a more typical vignette format. (#438)
 * `mutate()` is used in place of `transform()` in the examples. (#452)
 * Some minor tidying of the markdown templates (#454)

# mosaic 0.9.2

 * `tally()` now produces counts by default for all formula shapes.  Proportions or percentages must be requested explicitly.  This is to avoid common errors, especially when feeding the results into `chisq.test()`.
 * Introduction of `msummary()`.  Usually this is identical to `summary()`, but for a few kids of objects it provides modified output that is less verbose.  
 * By default `do * lm( )` will now keep track of the F statistic, too.
\item
`confint()` applied to an object produced using `do()` now does more appropriate things.
 * `binom.test()` and `prop.test()` now set `success = 1` by default
on 0-1 data to treat 0 like failure and 1 like success. Similarly, `prop()` and `count()` set `level = 1` by default.
 * `CIsim()` can now produce plots and does so by default when `samples <= 200`.
 * implementation of `add=TRUE` improved for `plotDist()`.
 * Added `swap()` which is useful for creating randomization
distributions for paired designs.  The current implementation is a bit slow.  
We will improve that by implementing part of the code in C++.
 * Some additional functions are now formula-aware: `MAD()`, `SAD()`, and `quantile()`.
 * `docFile()` introduced to simplify accessing files included
with package documentation.  `read.file()` enhanced to take a package
as an argument and look among package documentation files.
 * `factorize()` introduced as a way to convert vectors with
few unique values into factors.  Can be applied to an entire data frame.

# mosaic 0.9.1

 * The data sets formerly in this package have been separated out into two
additional packages:  `NHANES` contains the `NHANES` data set
and `mosaicData` contains the other data sets.  
 * `MAD()` and `SAD()` were added to compute mean and sum
of all pairs of absolute differences.
 * Facilities for making choropleth maps has been added.  The API for these 
tools is still under development and may change in future releases.
 * `rspin()` has been added to simulate spinning a spinner.
 * Two additional vignettes are included.  Less Volume, More Creativity
outlines how to use the `mosaic` package to simplify R for beginners.  
The other vignette 
illustrates many of the plotting features added by the `mosaic` package.
 * The mosaic package now contains two RMarkdown templates (one fancy and one plain).
 * `plotFun()` has been improved so that it does a better job of selecting points
where the function is evaluated and no longer warns about `NaN`s encountered
while exploring the domain of the function.
 * `oddsRatio()` has been redesigned and `relrisk()` has been added.
Use their `summary()` methods or `verbose=TRUE` to see more information
(including confidence intervals).
 * Added `Birthdays` data set.

# mosaic 0.9.0

 * A generic `mplot()` and several instances have been
  added to make a number of plots easy to generate.  There are methods 
  for objects of classes 
  `"data.frame"`, 
  `"lm"`, 
  `"summary.lm"`, 
  `"glm"`, 
  `"summary.glm"`, 
  `"TukeyHSD"`,  and 
  `"hclust"`.  For several of these there are also
  `fortify` methods that return the data frame created to 
  facilitate plotting.
 * `read.file()` now handles (some?) https URLs and accepts 
an optional argument `filetype` that can be used to
declare the type of data file when it is not identified 
by extension.
 * The default for `useNA` in the `tally()` function
  has changed to `"ifany"`.
 * `mosaic` now depends on `dplyr` both to use some
  of its functionality and to avoid naming collisions with functions 
  like `tally()` and `do()`, allowing `mosaic` and `dplyr` 
  to coexist more happily.
 * some improvements to dot plots with `dotPlot()`. In particular,
  the size of the dots is determined differently and works better 
  more of the time.  Dots were also shifted down by .5 units so that they  
  do not hover above the x-axis so much.  This means that (with default
  sizing) the tops of the dots are approximately located at a height
  equivalent to the number of dots rather than the center of the dots.
 * fixed a bug in `do()` that caused it to scope incorrectly
 in some edge cases when a variable had the same name as a function.
 * `ntiles()` has been reimplemented and now has more 
 formatting options.
 * introduction of `derivedFactor()` for creating factors
  from logical "cases".

# mosaic 0.8.0

 * The `HELP` data set has been removed from the package.  
It was deprecated in version 0.5.  Use `HELPrct` instead.
 * `plotDist()` now accepts `add=TRUE` and `under=TRUE`, making it easy to add
plots of distributions over (or under) plots of data (e.g., histograms, densityplots, etc.)
or other distributions.
 * Plotting functions with with the option `add=TRUE` have been reimplemented using
`layer` from `latticeExtra`.  See documentation of these functions for details.
 * `ladd()` has been completely reimplemented using `layer()` from `latticeExtra`.  See 
documentation of `ladd()` for details, including some behavior changes.
 * aggregating functions (`mean()`, `sd()`, `var()`, *et al*) now use `getOptions("na.rm")` to determine the default value of `na.rm`.  Use `options(na.rm=TRUE)` to
change the default behavior to remove `NA`s and options(na.rm=NULL) to restore 
defaults.
 * `do()` has been largely rewritten with an eye toward improved 
efficiency.  In particular, `do()` will take advantage of multiple cores
if the `parallel` package is available. At this point, sluggishness in applications of `do()` are 
mostly likely due to the sluggishness of what is being done, not to `do()` itself.
 * Added an additional method to `deltaMethod()` from the `car` package to make it easier to propagate uncertainty in some situations
that commonly arise in the physical sciences and engineering.
 * Added `cdist()` to compute critical values for the central portion
of a distribution.
 * Some changes to the API for `qdata()`.  For interactive use, this should
not cause any problems, but old programmatic uses of `qdata()` should be 
checked as the object returned is now different.
 * Fixed a bug that caused aggregating functions (`sum()`, `mean()`, `sd()`, etc.) to produce counter-intuitive results (but with a warning).  The results are now what one would expect (and the warning is removed).
 * Added `rsquared()` for extracting r-squared from models and model-like objects (`r.squared()` has been deprecated).
 * `do()` now handles ANOVA-like objects better
 * `maggregate()` is now built on some improved behind the scenes functions.  Among
other features, the `groups` argument is now incorporated as an alternative method
of specifying the groups to aggregate over and the `method` argument can be set to 
`"ddply"` to use `ddply()` from the `plyr` package for aggregation.  This results
in a different output format that may be desired in some applications.
\item
The `cdata()`, `pdata()` and `qdata()` functions have been largely rewritten.  In addition,
`cdata_f()`, `pdata_f()` and `qdata_f()` are provided which produce similar results
but have a formula in the first argument slot.
 * Fixed bug in vignette generation.  Static PDFs are now installed in `doc/` and so
are available from within the package as well as via links to external files.
 * Added `fetchGapminder()` for fetching data sets originally from
Gapminder.
 * Added `cdata()` for finding end points of a central portion of a variable.
 * Name changes in functions like `prop()` to avoid internal `:` which makes downstream processing messier.
 * Improved detection of the availability of `manipulate()` (RStudio)
 * Surface plots produced by `plotFun()` can be used without 
`manipulate()`.  This makes it possible to put surface plots into RMarkdown or Rnw files or to generate them outside of RStudio.
 * `do() * rflip()` now records proportion heads as well
as counts of heads and tails.
 * Added functions `mosaicLatticeOptions()` and `restoreLatticeOptions()` to switch back and forth between `lattice` defaults and 
`mosaic` defaults.
 * `dotPlot()` uses a different algorithm to determine dot sizes.
(Still not perfect, but `cex` can be used to further scale the dots.)
 * adjustments to `histogram()` so that `nint` matches the number of bins used more accurately.
 * fixed coding error in the HELP datasets so that `i2`: max number of drinks is at least as large as `i1`: the average number of drinks.
 * removed the deprecated HELP dataset (now called HELPrct)
 * Various minor bug fixes and internal improvements. 

# mosaic 0.7.0

 * Various improvements and bug fixes to `D()` and `antiD()`.
 * In RStudio, `mPlot()` provides an interactive environment for
    creating `lattice` and `ggplot2` plots.
 * Some support for producing maps has been introduced, notably `sp2df()` for converting SpatialPolygonDataFrames to regular data frames (which is useful for plotting with `ggplot2`, for example).  Also the `Countries` data frame facilitates
    mapping country names among different sources of map data.
 * Data frames returned by `do()` are now marked as such so that `confint()`
    can behave differently for such data frames and for "regular" data frames.
 * `t.test()` can now do 1-sample t-test described using a formula.
 * Aggregating functions (e.g. `mean()`, `var()`, etc. using a formula
    interface) have been completely
    reimplemented and additional aggregating functions are provided.
 * An `ntiles()` function has been added to facilitate creating
    factors based on quantile ranges.
 * Changes in format to `RailTrail` dataset.
 * Minor changes in documentation.
 * Added vignettes: Starting with R and A Compendium of Commands to Teach Statistics.
 * Plan to deprecate datasets from the Carnegie Melon University Online Learning Initiative Statistics Modules in next release.
 * `xhistogram()` is now deprecated.  Use `histogram()` instead.

# mosaic 0.6.0

 * Added vignette: Minimal R for Intro Stats.
 * Implemented symbolic integration for simple functions.
 * Aggregating functions (`mean()`, `max()`, `median()`, `var()`, etc.) now use `getOption('na.rm')`
			to determine default behavior.
 * Various bug fixes in `var()` allow it to work in a wider range of situations.
 * Augmented `TukeyHSD()` so that explicit use of `aov()` is no longer required 
 * Added `panel.lmbands()` for plotting confidence and prediction bands in linear regression
 * Some data cleaning in the Carnegie Melon University Online Learning Initiative Statistics Modules.  In particular
		the name collision with `Animals` from `MASS` has been 
		removed by renaming the data set `GestationLongevity`.
 * Added `freqpolygon()` for making frequency polygons.
 * Added `r.squared()` for extracting r-squared from models and model-like objects.
 * Modified names of data frame produced by `do()` so that hyphens ('-') are turned into dots ('.')
 * Improvements to `fetchData()`.

# mosaic 0.5.0

We are still in beta, but we hope things are beginning to stabilize as we settle on 
syntax and coding idioms for the package.  Here are some of the key updates since 0.4:

 * removed dependency on RCurl since it caused installation problems for some PC users.  (Code requiring RCurl now checks at run time whether the package is available.)
 * further improvements to formula interfaces to common functions.  The conditional | now works in more situations and & has been replaced by + so that formulas look more like the formulas
				used in `lm()` and its cousins. 
 * inclusion of the datasets from the Carnegie Mellon University Online Learning Initiative Statistics modules.  These are in alpha form and some additional 
				data cleaning and renaming may happen in the near future.
				\item
				`makeFun()` now has methods for glm and nls objects
 * `D()` improved to use symbolic differentiation in more cases and allow pass through to
				`stats::D()` when that makes sense.  This allows functions like deltaMethod() from the car package
				to work properly even when the mosaic package is loaded.
 * The API for `antiD()` has been modified somewhat.  This may go through another revision
					if/when we add in symbolic differentiation, but we think we are now close to the end state.
 * The HELP dataset has been replaced by the HELPrct dataset, and the former will be deprecated in the next release.
 * The CPS data set has been renamed CPS85.
 * `fitSpline()` and `fitModel()` have been added as wrappers around linear models using ns(), bs(), and nls().
						Each of these returns the model fit as a function.
 * improvements to the vignettes.

# mosaic 0.4.0

 * renamed mtable() to tally(), added new functionality
 * reimplemented D() and antiD()
 * improvements to statTally()
 * new confint() functionality
 * makeFun() and plotFun() interface to plotting using formulas
 * added new vignette on Teaching Calculus using R
 * added new vignette on Resampling-Based Inference using R 
 * changed default behavior for aggregating functions na.rm option so that it defaults to usual behavior unless given a formula as argument

