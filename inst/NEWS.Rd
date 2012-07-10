\name{NEWS}
\title{NEWS file for the mosaic package}
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

