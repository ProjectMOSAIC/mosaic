\name{.fetchData.storage.helper}
\alias{.fetchData.storage.helper}
\alias{fetchData}
\title{A Web and Library Data-Loading Facility}
\usage{
  .fetchData.storage.helper()

  fetchData(name = NULL, add.to.path = FALSE,
    drop.from.path = FALSE, add.to.library = FALSE,
    directory = NULL, var = NULL)
}
\arguments{
  \item{name}{a character string naming a data set.  This
  will often end in \code{.csv}.  It can also name a web
  directory (always ending in \code{/}). It can also name a
  data set to be stored in the cached library.}

  \item{add.to.path}{If \code{TRUE}, indicates that the web
  search path is to printed out, or, if \code{name} is
  specified, the name should be a web directory (ending in
  \code{/}), which should be pre-pended to the search
  path.}

  \item{drop.from.path}{If \code{TRUE}, wipes out the web
  search path, or, if \code{name} is specified, removes
  that web directory from the search path.}

  \item{add.to.library}{If \code{TRUE}, indicates that a
  data set is to be pre-loaded into the cached library.
  This allows, for instance, users to pre-load on-line data
  to be used when they are off-line.}

  \item{var}{The data frame to be put in the cached library
  if \code{add.to.library=TRUE}.}

  \item{directory}{The name of a web directory to be
  searched but not added to the search path.}

  \item{name}{}

  \item{add.to.path}{path to add to search path}

  \item{drop.from.path}{path to remove from search path}

  \item{add.to.library}{}

  \item{directory}{}

  \item{var}{}
}
\value{
  a data frame.

  a function

  a data frame containing data
}
\description{
  \code{fetchData} provides a means for students and others
  to locate and load data sets provided by instructors.
  Data can be pre-loaded for off-line sessions, can be
  positioned on identified web sites, and can be loaded
  from packages. \code{fetchData} also will load local
  \code{.csv} files using \code{file.choose()}.

  Fetch data sets from internet repositories
}
\details{
  There are two major purposes for this function. One is to
  provide a consistent interface to reading data: a file
  name is given and a data frame is returned, which can be
  assigned to an object as the user desires.  This differs
  from the behavior of \code{data}, which doesn't return a
  value but instead creates an object without explicit
  assignment.

  The other is to allow instructors (or other group
  leaders) to post data on web sites that can be searched
  as naturally as if the data were on the users' own
  machines.  For instance, an instructor might want to post
  a new data set just before class, enabling her students
  to access it in class.

  To support this, \code{fetchData} allows new web sites to
  be added to the web search path.  Typically, the command
  to add a site would be in a script file that is provided
  to the student that could be run automatically at start
  up or \code{source}d over the web.  That is, an
  instructor might create a script file stored on a website
  and, using a web page, provide students with the text of
  the command to \code{source} it.
}
\examples{
kids <- fetchData("KidsFeet.csv")
carbon <- fetchData("CO2")
fetchData(add.to.path=TRUE)
fetchData(add.to.path=TRUE,name="http://www.macalester.edu/~kaplan/ISM/datasets/")
fetchData(drop.from.path=TRUE,name="http://www.macalester.edu/~kaplan/ISM/datasets/")
fetchData(drop.from.path=TRUE)
fetchData(add.to.library=TRUE,name="mydata.csv",var=data.frame(x=c(1,2,3), y=c(7,1,4)))
}
\author{
  Daniel Kaplan (\email{kaplan@macalester.edu}) and Randall
  Pruim (\email{rpruim@calvin.edu})
}
\keyword{data}
\keyword{fetch}
\keyword{function}
\keyword{internal}
\keyword{mosaic}

