% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Baseline_Simulation.R
\name{Baseline_Simulation}
\alias{Baseline_Simulation}
\title{Baseline Simulation}
\usage{
Baseline_Simulation(n, mu_latency, sd_latency, mu_duration, sd_duration)
}
\arguments{
\item{n}{Number of Observations}

\item{mu_latency}{Average latency between consecutive calls per caller.}

\item{sd_latency}{Uncertainty around the average latency between calls.}

\item{mu_duration}{Average duration of each call.}

\item{sd_duration}{Uncertainty in average call duration.}
}
\value{
A data frame object that contains our dyadic interlocutors
}
\description{
Simulates baseline dyadic vocal turn taking with no interaction between the callers.
}
\examples{

}
