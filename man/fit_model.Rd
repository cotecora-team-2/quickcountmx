% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_arrivals.R
\name{fit_model}
\alias{fit_model}
\title{Simulation of arrival times according to model based on 2018 data}
\usage{
fit_model(arrivals_tbl, states)
}
\arguments{
\item{arrivals_tbl}{\code{tibble} with the arrival data}

\item{states}{character vector with states for which model should
be fit}
}
\description{
Fit model and simulate arrival times (hours after polling stations closing).
This times correspond to different CST times depending on the timezone
of each polling station.
\code{fit_model} fits a survival regression model to the arrival times
}
