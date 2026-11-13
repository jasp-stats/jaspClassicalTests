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

#' Multiple Variances
#'
#' @param bartlettTest, Bartlett's test for equality of variances; assumes normality.
#'    Defaults to \code{FALSE}.
#' @param bonettTest, Bonett's test for equality of variances using Winsorized kurtosis. Requires raw data.
#'    Defaults to \code{FALSE}.
#' @param bootstrapSamples, Number of bootstrap replicates for the BCa interval.
#' @param boxPlot, Box plot of the dependent variable across groups. Requires raw data.
#'    Defaults to \code{FALSE}.
#' @param ciMethod, Method for the per-group variance confidence interval. Bonett's method and the bootstrap need the raw observations, so with summarized data only the χ² interval is available and the method cannot be changed.
#' \itemize{
#'   \item \code{"chiSquare"}
#'   \item \code{"bonett"}
#'   \item \code{"bootstrap"}
#' }
#' @param dependent, Scale variable(s) whose variances are compared across groups.
#' @param fTest, F-test for equality of two variances.
#'    Defaults to \code{FALSE}.
#' @param factor, Nominal variable defining the groups.
#' @param leveneTest, Levene's test for equality of variances based on the median. Requires raw data.
#'    Defaults to \code{TRUE}.
#' @param normalityTest, Shapiro-Wilk test of normality.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Q-Q plot of the residuals.
#'    Defaults to \code{FALSE}.
#' @param rainCloudPlot, Raincloud plot of the group-demeaned values: each group is centered at mean 0 while its variance is preserved, so the plot compares the spread of the groups. Requires raw data.
#'    Defaults to \code{FALSE}.
#' @param rainCloudPlotHorizontal, Changes the orientation of the raincloud plot.
#'    Defaults to \code{FALSE}.
#' @param ratioCiMethod, Method for the variance ratio confidence interval. Bonett's method needs the raw observations (it relies on the sample kurtosis), so with summarized data only the F-test interval is available and the method cannot be changed.
#' \itemize{
#'   \item \code{"fTest"}
#'   \item \code{"bonett"}
#' }
#' @param sdCi, Confidence interval for the population standard deviation of each group. It is the square root of the variance interval, which preserves the coverage exactly.
#'    Defaults to \code{FALSE}.
#' @param summarizedGroups, Per-group summary statistics. Add one row per group with its sample variance and sample size.
#' @param varEstimatePlot, Plot of the variance estimates with confidence intervals.
#'    Defaults to \code{FALSE}.
#' @param varRatioPlot, Plot of the variance ratio with confidence interval (F-test based, 2 groups only).
#'    Defaults to \code{FALSE}.
#' @param varianceCi, Confidence interval for the population variance of each group.
#'    Defaults to \code{FALSE}.
#' @param varianceRatioCi, Confidence interval for the variance ratio (2 groups only).
#'    Defaults to \code{FALSE}.
multipleVariances <- function(
          data = NULL,
          version = "1",
          bartlettTest = FALSE,
          bonettTest = FALSE,
          bootstrapSamples = 1000,
          boxPlot = FALSE,
          ciMethod = "chiSquare",
          confLevel = 0.95,
          dependent = list(types = list(), value = list()),
          descriptives = FALSE,
          fTest = FALSE,
          factor = list(types = list(), value = ""),
          inputType = "rawData",
          leveneTest = TRUE,
          normalityTest = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          qqPlot = FALSE,
          rainCloudPlot = FALSE,
          rainCloudPlotHorizontal = FALSE,
          ratioCiMethod = "fTest",
          sdCi = FALSE,
          seed = 1,
          setSeed = FALSE,
          summarizedGroups = list(list(groupName = "1"), list(groupName = "2")),
          varEstimatePlot = FALSE,
          varRatioPlot = FALSE,
          varianceCi = FALSE,
          varianceRatioCi = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("dependent", "factor", "summarizedGroups")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspClassicalTests", "multipleVariances", "multipleVariances.qml", options, version, TRUE))
}