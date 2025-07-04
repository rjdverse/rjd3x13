% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_x11_spec.R
\name{set_x11}
\alias{set_x11}
\title{Set X-11 Specification}
\usage{
set_x11(
  x,
  mode = c(NA, "Undefined", "Additive", "Multiplicative", "LogAdditive",
    "PseudoAdditive"),
  seasonal.comp = NA,
  seasonal.filter = NA,
  henderson.filter = NA,
  lsigma = NA,
  usigma = NA,
  fcasts = NA,
  bcasts = NA,
  calendar.sigma = c(NA, "None", "Signif", "All", "Select"),
  sigma.vector = NA,
  exclude.forecast = NA,
  bias = c(NA, "LEGACY")
)
}
\arguments{
\item{x}{the specification to be modified, object of class "JD3_X11_SPEC",
default X11 spec can be obtained as 'x=x11_spec()'}

\item{mode}{character: the decomposition mode. Determines the mode of the
seasonal adjustment decomposition to be performed:
\code{"Undefined"} - no assumption concerning the relationship between the time
series components is made;
\code{"Additive"} - assumes an additive relationship;
\code{"Multiplicative"} - assumes a multiplicative relationship;
\code{"LogAdditive"} - performs an additive decomposition of the logarithms of the
series being adjusted;
\code{"PseudoAdditive"} - assumes an pseudo-additive relationship. Could be
changed by the program, if needed.}

\item{seasonal.comp}{logical: if \code{TRUE}, the program computes a seasonal
component. Otherwise, the seasonal component is not estimated and its values
are all set to 0 (additive decomposition) or 1 (multiplicative
decomposition).}

\item{seasonal.filter}{a vector of character(s) specifying which seasonal
moving average (i.e. seasonal filter) will be used to estimate the seasonal
factors for the entire series. The vector can be of length: 1 - the same
seasonal filter is used for all periods (e.g.: \code{seasonal.filter = "Msr"} or
\code{seasonal.filter = "S3X3"} ); or have a different value for each quarter
(length 4) or each month (length 12) - (e.g. for quarterly series:
\code{seasonal.filter = c("S3X3", "Msr", "S3X3", "Msr")}). Possible filters are:
\code{"Msr"}, \code{"Stable"}, \code{"X11Default"}, \code{"S3X1"}, \code{"S3X3"}, \code{"S3X5"}, \code{"S3X9"},
\code{"S3X15"}. \code{"Msr"} - the program chooses the final seasonal filter
automatically.}

\item{henderson.filter}{numeric: the length of the Henderson filter (odd
number between 3 and 101). If \code{henderson.filter = 0} an automatic selection
of the Henderson filter's length for the trend estimation is enabled.}

\item{lsigma}{numeric: the lower sigma boundary for the detection of extreme
values, > 0.5, default=1.5.}

\item{usigma}{numeric: the upper sigma boundary for the detection of extreme
values, > lsigma, default=2.5.}

\item{bcasts, fcasts}{numeric: the number of backasts (\code{bcasts}) or forecasts
(\code{fcasts}) generated by the RegARIMA model in periods (positive values) or
years (negative values).Default values: fcasts=-1 and bcasts=0.}

\item{calendar.sigma}{character to specify if the standard errors used for
extreme values detection and adjustment are computed: from 5 year spans of
irregulars (\code{"None"}, default value); separately for each calendar period
(\code{"All"}); separately for each period only if Cochran's hypothesis test
determines that the irregular component is heteroskedastic by calendar
month/quarter (\code{"Signif"}); separately for two complementary sets of calendar
months/quarters specified by the x11.sigmaVector parameter (\code{"Select"}, see
parameter \code{sigma.vector}).}

\item{sigma.vector}{a vector to specify one of the two groups of periods for
which standard errors used for extreme values detection and adjustment will
be computed separately. Only used if \code{calendar.sigma = "Select"}. Possible
values are: \code{1} or \code{2}.}

\item{exclude.forecast}{Boolean to exclude forecasts and backcasts. If
\code{TRUE}, the RegARIMA model forecasts and backcasts are not used during the
detection of extreme values in the seasonal adjustment routines.
Default = FALSE.}

\item{bias}{TODO.}
}
\value{
a "JD3_X11_SPEC" object, containing all the parameters.
}
\description{
Set X-11 Specification
}
\examples{
\dontshow{if (jversion >= 17) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
init_spec <- x11_spec()
new_spec <- set_x11(init_spec,
    mode = "LogAdditive",
    seasonal.comp = 1,
    seasonal.filter = "S3X9",
    henderson.filter = 7,
    lsigma = 1.7,
    usigma = 2.7,
    fcasts = -1,
    bcasts = -1,
    calendar.sigma = "All",
    sigma.vector = NA,
    exclude.forecast = FALSE,
    bias = "LEGACY"
)
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=x13_spec]{x13_spec()}} and \code{\link[=x11_spec]{x11_spec()}}.
}
