\name{max}
\alias{max}
\title{rdname aggregating-methods}
\usage{
  max(x, ..., na.rm = TRUE)
}
\description{
  rdname aggregating-methods
}
\examples{
max(age, data=HELPrct)
max(~age, data=HELPrct)
max(age ~ ., data=HELPrct)
max(age ~ 1, data=HELPrct)
max(age ~ NULL, data=HELPrct)
max(HELPrct$age)
max(age ~ sex, data=HELPrct)
max(age ~ sex + treat, data=HELPrct)
}

