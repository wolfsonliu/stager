% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stage_plot.r
\name{plot.stage}
\alias{plot.stage}
\title{Plotting of stage result}
\usage{
\method{plot}{stage}(x, ...)
}
\arguments{
\item{x}{stage result.}

\item{...}{other parameters for plot function.}
}
\description{
Plot method function for calculation result of
    staging bird variables, with input class of 'stage'.
}
\details{
Input data should be of 'stage' class, generated by
    \code{calculate.stage.variables} function. The stage class is
    a list, with item names of: day, daycount, population,
    immigrate.mean, immigarate.sd, emigrate.mean, emigrate.sd. You
    can make a list with the corresponding value and assign it the
    class 'stage' to use this \code{plot.stage} function.
}
\examples{
data <- data.frame(
    day=c(11, 16, 20, 22, 24, 34, 39, 40, 47, 60),
    daycount=c(10, 65, 60, 45, 66, 52, 35, 50, 27, 0)
)

result <- calculate.stage.variables(
    day=data$day,
    daycount=data$daycount,
    population=85,
    immigrate.mean=c(10, 20),
    emigrate.mean=c(20, 50),
    immigrate.sd=c(4, 10),
    emigrate.sd=c(10, 30)
)

plot(result)

}
\seealso{
[calculate.stage.variables()] for calculation of stage class.
}
