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

#' One Variance
#'
#' @param bootstrapSamples, Number of bootstrap replicates for the BCa interval.
#' @param ciMethod, Method for the confidence interval. Bonett's method and the bootstrap need the raw observations, so with summarized data only the χ² interval is available and the method cannot be changed.
#' \itemize{
#'   \item \code{"chiSquare"}
#'   \item \code{"bonett"}
#'   \item \code{"bootstrap"}
#' }
#' @param dependent, In this box the dependent variable is selected.
#' @param normalityTest, Shapiro-Wilk test of normality.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Q-Q plot of the standardized residuals.
#'    Defaults to \code{FALSE}.
#' @param sampleSize, The number of observations the sample variance is based on.
#' @param sampleVariance, The observed sample variance.
#' @param sdCi, Confidence interval for the population standard deviation. It is the square root of the variance interval, which preserves the coverage exactly.
#'    Defaults to \code{FALSE}.
#' @param varianceCi, Confidence interval for the population variance.
#'    Defaults to \code{FALSE}.
singleVariance <- function(
          data = NULL,
          version = "1",
          alternative = "two.sided",
          bootstrapSamples = 1000,
          ciMethod = "chiSquare",
          confLevel = 0.95,
          dependent = list(types = list(), value = list()),
          inputType = "rawData",
          normalityTest = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          qqPlot = FALSE,
          sampleSize = 2,
          sampleVariance = 1,
          sdCi = FALSE,
          sdEstimate = FALSE,
          seed = 1,
          setSeed = FALSE,
          testVariance = 1,
          varEstimate = FALSE,
          varianceCi = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dependent")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspClassicalTests", "singleVariance", "singleVariance.qml", options, version, TRUE))
}