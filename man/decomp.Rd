% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decomp.R
\name{decomp}
\alias{decomp}
\title{Data decomposition}
\usage{
decomp(miss_data, init_method = "na_interpolation", freq = NULL, ...)
}
\arguments{
\item{miss_data}{matrix (ts object), missing data with n rows (observation) and k
columns (variables).}

\item{init_method}{character, initial imputation method (univariate) that will
be used. The univariate imputation methods are based on imputeTS package,
and must be one of the "na_interpolation", "na_kalman", "na_locf",
"na_ma", "na_mean", "na_replace", "na_seadec".}

\item{freq}{integer, the seasonal frequency of the data. If freq is set to NULL,
the frequency will be estimated.}

\item{...}{varargs, arguments of ts() function}
}
\value{
A list of two matrices, each with n rows and k columns.
\code{no_season} matrix is the time series matrix with removed seasonal
components, while \code{season} matrix is the time series matrix with
seasonal component.
}
\description{
This function performs simultaneous STL decomposition on each variable.
}
\examples{
# `decomp` can be used for completed data
decomp_comp = decomp(monthly_weather, freq = 12)

# or incomplete data. The missing values will be estimated using the specified
# univariate imputation method.
miss_data = omit(monthly_weather, seed = 1, num_miss = 15)
colSums(is.na(miss_data))
decomp_incomp = decomp(monthly_weather, freq = 12, init_method = "na_ma")
colSums(is.na(decomp_incomp$no_season))
colSums(is.na(decomp_incomp$season))

}
