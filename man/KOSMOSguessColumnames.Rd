% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KOSMOSguessColumnnames.R
\name{KOSMOSguessColumnames}
\alias{KOSMOSguessColumnames}
\title{Function to adjust data tables in a way that even slightly mismatching column names can be plotted correctly within this package.}
\usage{
KOSMOSguessColumnames(
  dataset = KOSMOStestdata,
  required_columns = c("Day", "Mesocosm", KOSMOScurrentCategoricalVar, "Delta_TA",
    "Treat_Meso")
)
}
\arguments{
\item{dataset}{A data set object roughly following the common KOSMOS layout for which the column names shall be checked.}

\item{required_columns}{Vector of those columns to be matched, all others are left untouched. Defaults to all potentially required columns for the available plot functions. This argument is supplied by the plotting function if this function is called automatically, so that only strictly necessary ones are marked as required.}
}
\value{
Returns an info value on how well the found columns matched the original, and the column numbers of the matches, and a boiled down version of the \code{KOSMOScolumntable} for internal use by \code{KOSMOSadjustColumnnames}.
}
\description{
Called by the plotting functions directly, this tool tries to guess the names of the required columns.
}
\examples{
KOSMOSguessColumnames(KOSMOStestdata)
}
