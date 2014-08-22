\name{NEWS}
\title{NEWS file for the mosaic package}
\section{Changes in version 0.9.1}{
\itemize{
\item The data sets formerly in this pacakge have been separated out into two
additional packages:  \pkg{NHANES} contains the \code{NHANES} data set
and \pkg{mosaicData} contains the other data sets.  
\item \code{MAD} and \code{SAD} were added to compute mean and sum
of all pairs of absolute differences.
\item Facilities for making choropleth maps has been added.  The API for these 
tools is still under development and may change in future releases.
\item \code{rspin} has been added to simulate spinning a spinner.
\item Two additional vignettes are included.  Less Volume, More Creativity
outlines how to use the \pkg{mosaic} package to simplify R for beginners.  
The other vignette 
illustrates many of the plotting features added by the \pkg{mosaic} package.
\item The mosaic package now contains two RMarkdown templates (one fancy and one plain).
\item \code{plotFun} has been improved so that it does a better job of selecting points
where the function is evaluated and no longer warns about \code{NaN}s encountered
while exploring the domain of the function.
\item \code{oddsRatio} has been redesigned and \code{relrisk} has been added.
Use their \code{summary()} methods or \code{verbose=TRUE} to see more information
(including confidence intervals).
\item Added \code{Birthdays} data set.
}
}
\section{Changes in version 0.9}{
\itemize{
\item A generic \code{mplot} and several instances have been
  added to make a number of plots easy to generate.  There are methods 
  for objects of classes 
  \code{"data.frame"}, 
  \code{"lm"}, 
  \code{"summary.lm"}, 
  \code{"glm"}, 
  \code{"summary.glm"}, 
  \code{"TukeyHSD"},  and 
  \code{"hclust"}.  For several of these there are also
  \code{fortify} methods that return the data frame created to 
  facilitate plotting.
\item \code{read.file} now handles (some?) https URLs and accepts 
an optional argument \code{filetype} that can be used to
declare the type of data file when it is not identified 
by extension.
\item The default for \code{useNA} in the \code{tally} function
  has changed to \code{"ifany"}.
\item \pkg{mosaic} now depends on \pkg{dplyr} both to use some
  of its functionality and to avoid naming collisions with functions 
  like \code{tally} and \code{do}, allowing \pkg{mosaic} and \pkg{dplyr} 
  to coexist more happily.
\item some improvements to dot plots with \code{dotPlot}. In particular,
  the size of the dots is determined differently and works better 
  more of the time.  Dots were also shifted down by .5 units so that they  
  don't hover above the x-axis so much.  This means that (with default
  sizing) the tops of the dots are approximately located at a height
  equivalent to the number of dots rather than the center of the dots.
\item fixed a bug in \code{do} that caused it to scope incorrectly
 in some edge cases when a variable had the same name as a function.
\item \code{ntiles} has been reimplemented and now has more 
 formatting options.
\item introduction of \code{derivedFactor} for creating factors
  from logical "cases".
}
}
\section{Changes in version 0.8}{
\itemize{
\item The \code{HELP} data set has been removed from the package.  
It was deprecated in version 0.5.  Use \code{HELPrct} instead.
\item \code{plotDist} now accepts \code{add=TRUE} and \code{under=TRUE}, making it easy to add
plots of distributions over (or under) plots of data (e.g., histograms, densityplots, etc.)
or other distributions.
\item Plotting funcitons with with the option \code{add=TRUE} have been reimplemented using
\code{layer} from \pkg{latticeExtra}.  See documentaiton of these functions for details.
\item \code{ladd} has been completely reimplemented using \code{layer()} from \pkg{latticeExtra}.  See 
documentation of \code{ladd()} for details, including some behavior changes.
\item aggregating functions (\code{mean}, \code{sd}, \code{var}, et al) now use \code{getOptions("na.rm")} to determine the default value of \code{na.rm}.  Use \code{options(na.rm=TRUE)} to
change the default behavior to remove \code{NA}s and options(na.rm=NULL) to restore 
defaults.
\item \code{do} has been largely rewritten with an eye toward improved 
efficiency.  In particular, \code{do} will take advantage of multiple cores
if the \pkg{parallel} package is availalbe. At this point, sluggishness in applications of \code{do} are 
mostly likely due to the sluggishness of what is being done, not to \code{do} itself.
\item Added an additional method to \code{deltaMethod} from the \pkg{car} package to make it easier to do propagation of uncertainty is some situations
that arrise commonly in the physical sciences and engineering.
\item Added \code{cdist} to compute critical values for the central portion
of a distribution.
\item Some changes to the API for \code{qdata}.  For interactive use, this 
not cause any problem, but old programmatic uses of \code{qdata} should be 
checked as the object returned is now different.
\item Fixed a bug that caused aggregating functions (\code{sum}, \code{mean}, \code{sd}, etc.) to produce counter-intuititve results (but with a warning).  The results are now what one would expect (and the warning is removed).
\item Added \code{rsquared} for extracting r-squared from models and model-like objects (\code{r.squared} has been deprecated).
\item \code{do} now handles ANOVA-like objects better
\item \code{maggregate} is now built on some improved behind the scenes functions.  Among
other features, the \code{groups} argument is now incorporated as an alternative method
of specifying the goups to aggregate over and the \code{method} argument can be set to 
\code{"ddply"} to use \code{ddply} from the \pkg{plyr} package for aggregation.  This results
in a different output format that may be desired in some applications.
\item
The \code{cdata}, \code{pdata} and \code{qdata} functions have been largely rewritten.  In addition,
\code{cdata_f}, \code{pdata_f} and \code{qdata_f} are provided which produce similar results
but have a formula in the first arguemnt slot.
\item Fixed bug in vignette generation.  Static PDFs are now installed in \code{doc/} and so
are available from within the package as well as via links to external files.
\item Added \code{fetchGapminder} for fetching data sets originally from
Gapminder.
\item Added \code{cdata} for finding end points of a central portion of a variable.
\item Name changes in functions like \code{prop} to avoid internal \code{:} which makes downstream processing messier.
\item Improved detection of the availability of \code{manipulate} (RStudio)
\item Surface plots produced by \code{plotFun} can be used without 
\code{manipulate}.  This makes it possible to put surface plots into RMarkdown or Rnw files or to generate them outside of RStudio.
\item \code{do() * rflip()} now records proportion heads as well
as counts of heads and tails.
\item Added functions \code{mosaicLatticeOptions} and \code{restoreLatticeOptions} to switch back and forth between \code{lattice} defaults and 
\code{mosaic} defaults.
\item \code{dotPlot} uses a different algorithm to determine dot sizes.
(Still not perfect, but \code{cex} can be used to further scale the dots.)
\item adjustments to \code{histogram} so that \code{nint} matches the number of bins used more accurately.
\item fixed coding error in the HELP datasets so that \code{i2}: max number of drinks is at least as large as \code{i1}: the average number of drinks.
\item removed the deprecated HELP dataset (now called HELPrct)
\item Various minor bug fixes and internal improvements. 
}
}

\section{Changes in version 0.7}{
	\itemize{
    \item Various improvements and bug fixes to \code{D} and \code{antiD}.
    \item In RStudio, \code{mPlot} provides an interactive environment for
    creating \pkg{lattice} and \pkg{ggplot2} plots.
    \item Some support for producing maps has been introduced, notably \code{sp2df} for converting SpatialPolygonDataFrames to regular data frames (which is useful for plotting with \pkg{ggplot2}, for example).  Also the \code{Countries} data frame facilitates
    mapping country names among different sources of map data.
    \item Data frames returned by \code{do} are now marked as such so that \code{confint}
    can behave differently for such data frames and for "regular" data frames.
    \item \code{t.test} can now do 1-sample t-test described using a formula.
    \item Aggregating functions (e.g. \code{mean}, \code{var}, etc. using a formula
    interface) have been completely
    reimplemented and additional aggregating functions are provided.
    \item An \code{ntiles} function has been added to facilitate creating
    factors based on quantile ranges.
		\item Changes in format to \code{RailTrail} dataset.
		\item Minor changes in documentation.
		\item Added vignettes: Starting with R and A Compendium of Commands to Teach Statistics.
		\item Plan to deprecate datasets from the Carnegie Melon University Online Learning Initiative Statistics Modules in next release.
    \item \code{xhistogram} is now deprecated.  Use \code{histogram} instead.
	}
}
\section{Changes in version 0.6}{
	\itemize{
		\item Added vignette: Minimal R for Intro Stats.
		\item Implemented symbolic integration for simple functions.
		\item Aggregating functions (\code{mean}, \code{max}, \code{median}, \code{var}, etc.) now use \code{getOption('na.rm')}
			to determine default behavior.
		\item Various bug fixes in \code{var()} allow it to work in a wider range of situations.
		\item Augmented \code{TukeyHSD} so that explicit use of \code{aov} is no longer required 
		\item Added \code{panel.lmbands} for plotting confidence and prediction bands in linear regression
		\item Some data cleaning in the Carnegie Melon University Online Learning Initiative Statistics Modules.  In particular
		the name collision with \code{Animals} from \code{MASS} has been 
		removed by renaming the data set \code{GestationLongevity}.
		\item Added \code{freqpolygon} for making frequency polygons.
		\item Added \code{r.squared} for extracting r-squared from models and model-like objects.
		\item Modified names of data frame produced by \code{do} so that hyphens ('-') are turned into dots ('.')
		\item Improvements to \code{fetchData}.
	}
}
\section{Changes in version 0.5}{
We're still in beta, but we hope things are beginning to stabilize as we settle on 
syntax and coding idioms for the package.  Here are some of the key updates since 0.4:
\itemize{
		\item removed dependency on RCurl since it caused installation problems for some PC users.  (Code requiring RCurl now checks at run time whether the package is available.)
				\item further improvements to formula interfaces to common functions.  The conditional | now works in more situations and & has been replaced by + so that formulas look more like the formulas
				used in \code{lm()} and its cousins. 
				\item inclusion of the datasets from the Carnegie Mellon University Online Learning Initiative Statistics modules.  These are in alpha form and some additional 
				data cleaning and renaming may happen in the near future.
				\item
				\code{makeFun()} now has methods for glm and nls objects
				\item \code{D()} improved to use symbolic differentiation in more cases and allow pass through to
				\code{stats::D()} when that makes sense.  This allows functions like deltaMethod() from the car package
				to work properly even when the mosaic package is loaded.
				\item The API for \code{antiD()} has been modified somewhat.  This may go through another revision
					if/when we add in symbolic differentiation, but we think we are now close to the end state.
				\item The HELP dataset has been replaced by the HELPrct dataset, and the former will be deprecated in the next release.
				\item The CPS data set has been renamed CPS85.
				\item \code{fitSpline()} and \code{fitModel()} have been added as wrappers around linear models using ns(), bs(), and nls().
						Each of these returns the model fit as a function.
				\item improvements to the vignettes.
}
}

\section{Changes in version 0.4}{
  \itemize{
    \item renamed mtable() to tally(), added new functionality
    \item reimplemented D() and antiD()
    \item improvements to statTally()
    \item new confint() functionality
    \item makeFun() and plotFun() interface to plotting using formulas
    \item added new vignette on Teaching Calculus using R
    \item added new vignette on Resampling-Based Inference using R 
    \item changed default behavior for aggregating functions na.rm option so that it defaults to usual behavior unless given a formula as argument
}}

