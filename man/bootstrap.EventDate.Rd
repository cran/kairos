% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/bootstrap.R
\docType{methods}
\name{bootstrap.EventDate}
\alias{bootstrap.EventDate}
\alias{bootstrap,EventDate-method}
\title{Bootstrap Event Dates}
\usage{
\S4method{bootstrap}{EventDate}(
  object,
  level = 0.95,
  probs = c(0.05, 0.95),
  n = 1000,
  calendar = get_calendar(),
  progress = getOption("kairos.progress"),
  ...
)
}
\arguments{
\item{object}{An \code{\linkS4class{EventDate}} object (typically returned by \code{\link[=event]{event()}}).}

\item{level}{A length-one \code{\link{numeric}} vector giving the confidence level.}

\item{probs}{A \code{\link{numeric}} vector of probabilities with values in
\eqn{[0,1]}.}

\item{n}{A non-negative \code{\link{integer}} specifying the number of bootstrap
replications.}

\item{calendar}{An \code{\link[aion:TimeScale-class]{aion::TimeScale}} object specifying the target
calendar (see \code{\link[aion:calendar]{aion::calendar()}}). If \code{NULL}, \emph{rata die} are returned.}

\item{progress}{A \code{\link{logical}} scalar: should a progress bar be displayed?}

\item{...}{Currently not used.}
}
\value{
A \code{\link{data.frame}}.
}
\description{
Generates bootstrap estimations of an \link[=event]{event date}.
}
\details{
A large number of new bootstrap assemblages is created, with the same sample
size, by resampling each of the original assemblage with replacement. Then,
examination of the bootstrap statistics makes it possible to pinpoint
assemblages that require further investigation.

A five columns \code{data.frame} is returned, giving the bootstrap
distribution statistics for each replicated assemblage (in rows)
with the following columns:
\describe{
\item{\code{min}}{Minimum value.}
\item{\code{mean}}{Mean value (event date).}
\item{\code{max}}{Maximum value.}
\item{\code{Q5}}{Sample quantile to 0.05 probability.}
\item{\code{Q95}}{Sample quantile to 0.95 probability.}
}
}
\seealso{
\code{\link[=event]{event()}}

Other resampling methods: 
\code{\link{bootstrap.MeanDate}},
\code{\link{jackknife.EventDate}},
\code{\link{jackknife.MeanDate}}
}
\author{
N. Frerebeau
}
\concept{resampling methods}
