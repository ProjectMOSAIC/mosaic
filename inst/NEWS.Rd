\name{NEWS}
\title{NEWS file for the mosaic package}
\section{Changes in version 0.5}{
We're still in beta, but we hope things are beginning to stabilize as we settle on 
syntax and coding idioms for the package.  Here are some of the key updates since 0.4:
\itemize{
\item removed dependency on RCurl since it caused installation problems for some PC users.  (Code requiring RCurl now checks at run time whether the package is available.)
\item further improvements to formula interfaces to common functions.  The conditional | now works in more situations and & has been replaced by + so that formulas look more like the formulas
used in lm() and its cousins. 
\item inclusion of the datasets from the Carnegie Mellon University Online Learning Initiative Statistics modules.  These are in alpha form and some additional 
data cleaning and renaming may happen in the near future.
\item
makeFun() now has methods for glm and nls objects
\item D() improved to use symbolic differentiation in more cases and allow pass through to
stats::D when that makes sense.  This allows functions like deltaMethod() from the car package
to work properly even when the mosaic package is loaded.
\item The HELP dataset has been replaced by the HELPrct dataset, and the former will be deprecated in the next release.
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

