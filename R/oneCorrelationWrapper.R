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

#' One Correlation
#'
#' @param ci, Confidence interval for the population correlation. Without bootstrapping it is available only for Pearson's r; with bootstrapping it is available for every selected coefficient.
#'    Defaults to \code{FALSE}.
#' @param ciBootstrap, Percentile bootstrap confidence intervals, available for Pearson's r, Spearman's rho and Kendall's tau-b.
#'    Defaults to \code{FALSE}.
#' @param effectSize, The Fisher transformed effect size with its standard error.
#'    Defaults to \code{FALSE}.
#' @param firstVariable, The first variable of the correlation.
#' @param kendall, Kendall's tau-b rank-order correlation coefficient.
#'    Defaults to \code{FALSE}.
#' @param pearson, Pearson's product moment correlation coefficient.
#'    Defaults to \code{TRUE}.
#' @param scatterPlot, Scatter plot of the correlated variables.
#'    Defaults to \code{TRUE}.
#' @param scatterPlotDensity, Adds marginal densities above and to the right of the scatter plot.
#'    Defaults to \code{TRUE}.
#' @param scatterPlotRegressionLine, Adds a linear regression line.
#'    Defaults to \code{TRUE}.
#' @param secondVariable, The second variable of the correlation.
#' @param spearman, Spearman's rank-order correlation coefficient.
#'    Defaults to \code{FALSE}.
#' @param testValue, The hypothesized value of the population correlation. The p-value is computed against this value (Fisher's z when non-zero).
#' @param vovkSellke, The maximum ratio of the likelihood of the observed p-value under H1 vs H0.
#'    Defaults to \code{FALSE}.
oneCorrelation <- function(
          data = NULL,
          version = "1",
          alternative = "two.sided",
          ci = FALSE,
          ciBootstrap = FALSE,
          ciBootstrapSamples = 1000,
          ciLevel = 0.95,
          effectSize = FALSE,
          firstVariable = list(types = list(), value = ""),
          kendall = FALSE,
          pearson = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          scatterPlot = TRUE,
          scatterPlotDensity = TRUE,
          scatterPlotRegressionLine = TRUE,
          scatterPlotRegressionLineCi = FALSE,
          scatterPlotRegressionLineCiLevel = 0.95,
          secondVariable = list(types = list(), value = ""),
          seed = 1,
          setSeed = FALSE,
          spearman = FALSE,
          testValue = 0,
          vovkSellke = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("firstVariable", "secondVariable")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspClassicalTests", "oneCorrelation", "oneCorrelation.qml", options, version, TRUE))
}