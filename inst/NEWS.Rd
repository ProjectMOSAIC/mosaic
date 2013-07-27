\name{NEWS}
\title{NEWS file for the mosaic package}
\section{Changes in version 0.7}{
	\itemize{
    \item improvements and bug fixes to \code{D} and \code{anitD}
    \item in RStudio, \code{mScatter} provides an interactive environment for
    creating \pkg{lattice} and \pkg{ggplot2} scatter plots.
    \item Some support for producing maps, notably \code{sp2df} for converting SpatialPolygonDataFrames to regular data frames (which is useful for plotting with \pkg{ggplot2}, for example)
    \item Data frames returned by \code{do} are now marked as such so that \code{confint}
    can behave differently for such data frames and for "regular" data frames.
    \item \code{t.test} can now do 1-sample t-test described using a formula.
    \item Aggregating funcitons (e.g. \code{mean}, \code{var}, etc. using a formula
    interface) have been completely
    reimplemented and additional aggregating functions are provided.
		\item Plan to deprecate Carnegie Melon University Online Learning Initiative Statistics Modules datasets in next release.
		\item changes in format to \code{RailTrail} dataset.
		\item minor changes in documentation.
		\item Added vignettes: Starting with R and A Compendium of Commands to Teach Statistics.
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

