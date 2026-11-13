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

#' Two Rates
#'
#' @param count, A variable containing the observed event counts per row.
#' @param descriptiveCi, Exact per-group confidence intervals for the individual rates.
#'    Defaults to \code{FALSE}.
#' @param descriptives, Show per-group event counts, times, and rate estimates.
#'    Defaults to \code{TRUE}.
#' @param exactTest, Exact conditional test (C-test). For the difference, only available for a hypothesized difference of 0.
#'    Defaults to \code{TRUE}.
#' @param group, A variable with exactly 2 levels identifying the two groups.
#' @param groupOneInterval, Total interval for group 1.
#' @param groupOneName, Label for group 1 (optional).
#' @param groupOneOccurrences, Number of observed occurrences in group 1.
#' @param groupTwoInterval, Total interval for group 2.
#' @param groupTwoName, Label for group 2 (optional).
#' @param groupTwoOccurrences, Number of observed occurrences in group 2.
#' @param normalApprox, Large-sample normal approximation.
#'    Defaults to \code{FALSE}.
#' @param pooledSe, Use the pooled rate estimate for the z statistic. Only relevant when testing the difference. This affects the test statistic and p-value only: The confidence interval is always based on the unpooled standard error, because the pooled estimate is valid only under the null hypothesis. The pooled estimate assumes a hypothesized difference of 0; with a nonzero hypothesized difference, select the unpooled standard error.
#'    Defaults to \code{TRUE}.
#' @param ratioCi, Confidence interval for the effect (ratio or difference). For the ratio, an exact interval (based on the conditional binomial) or a normal-approximation interval on the log scale is available. For the difference, the exact test reports a MOVER interval that combines the exact single-rate Poisson intervals (Zou & Donner, 2008), while the normal approximation reports an unpooled Wald interval.
#'    Defaults to \code{FALSE}.
#' @param testDifference, Null hypothesis value for the rate difference λ₁ - λ₂.
#' @param testRatio, Null hypothesis value for the rate ratio λ₁/λ₂.
#' @param time, Per-row interval, observation time or exposure. If omitted, each row contributes one unit.
twoSamplePoissonRate <- function(
          data = NULL,
          version = "1",
          alternative = "two.sided",
          ciMethod = "exact",
          confLevel = 0.95,
          count = list(types = list(), value = ""),
          descriptiveCi = FALSE,
          descriptiveConfLevel = 0.95,
          descriptives = TRUE,
          exactTest = TRUE,
          group = list(types = list(), value = ""),
          groupOneInterval = 1,
          groupOneName = "",
          groupOneOccurrences = 1,
          groupTwoInterval = 1,
          groupTwoName = "",
          groupTwoOccurrences = 1,
          inputType = "rawData",
          normalApprox = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          pooledSe = TRUE,
          ratioCi = FALSE,
          testDifference = 0,
          testRatio = 1,
          testTarget = "difference",
          time = list(types = list(), value = "")) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("count", "group", "time")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspClassicalTests", "twoSamplePoissonRate", "twoSamplePoissonRate.qml", options, version, TRUE))
}