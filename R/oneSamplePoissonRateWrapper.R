#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' One Rate
#'
#' @param count, A variable containing the observed event counts.
#' @param exactTest, Exact Poisson test based on the Poisson distribution. Valid for any number of occurrences, including zero; the confidence interval (Garwood, 1936) is conservative, i.e., its coverage is at least the nominal level (Barker, 2002; Patil & Kulkarni, 2012).
#'    Defaults to \code{TRUE}.
#' @param interval, Total interval (observation time or exposure).
#' @param normalApprox, Large-sample normal approximation. Included because it is easy to compute by hand and common in textbooks; the exact test is generally preferred. The z statistic uses the standard error under the hypothesized rate; the confidence interval (Wald) uses the standard error at the observed rate. Unreliable for small numbers of occurrences (Barker, 2002).
#'    Defaults to \code{FALSE}.
#' @param observedOccurrences, Total number of observed occurrences.
#' @param rateCi, Confidence interval for the event rate λ. Each row uses its own method: exact (Garwood, 1936) or normal approximation (Wald). For one-sided alternatives, a one-sided confidence bound is shown.
#'    Defaults to \code{FALSE}.
#' @param testRate, The null hypothesis rate (events per unit time).
#' @param time, A variable containing the interval, observation time or exposure for each row. If omitted, each row is assumed to contribute one unit of time.
oneSamplePoissonRate <- function(
          data = NULL,
          version = "1",
          alternative = "two.sided",
          confLevel = 0.95,
          count = list(types = list(), value = ""),
          exactTest = TRUE,
          inputType = "rawData",
          interval = 1,
          normalApprox = FALSE,
          observedOccurrences = 1,
          plotHeight = 320,
          plotWidth = 480,
          rateCi = FALSE,
          testRate = 1,
          time = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("count", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspClassicalTests", "oneSamplePoissonRate", "oneSamplePoissonRate.qml", options, version, TRUE))
}