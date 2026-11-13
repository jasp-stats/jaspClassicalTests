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

#' Two Correlations
#'
#' @param ci, Confidence intervals for the individual Pearson correlations (Fisher's z) and for their difference (Zou, 2007).
#'    Defaults to \code{FALSE}.
#' @param commonVariable, The variable shared by both correlations.
#' @param groupingVariable, Nominal variable with exactly two levels defining the two independent groups.
#' @param independentVariable1, First variable of the correlation.
#' @param independentVariable2, Second variable of the correlation.
#' @param overlapVariable1, Correlated with the common variable in the first correlation.
#' @param overlapVariable2, Correlated with the common variable in the second correlation.
#' @param scatterPlot, Scatter plot of the correlated variables. Independent groups are shown in one plot, coloured by group; dependent correlations get one plot each.
#'    Defaults to \code{TRUE}.
#' @param scatterPlotDensity, Adds marginal densities above and to the right of the scatter plot.
#'    Defaults to \code{TRUE}.
#' @param scatterPlotRegressionLine, Adds a linear regression line.
#'    Defaults to \code{TRUE}.
twoCorrelations <- function(
          data = NULL,
          version = "1",
          alternative = "two.sided",
          ci = FALSE,
          ciLevel = 0.95,
          commonVariable = list(types = list(), value = ""),
          dependentType = "overlapping",
          groupingVariable = list(types = list(), value = ""),
          independentVariable1 = list(types = list(), value = ""),
          independentVariable2 = list(types = list(), value = ""),
          nonoverlapVariable1 = list(types = list(), value = ""),
          nonoverlapVariable2 = list(types = list(), value = ""),
          nonoverlapVariable3 = list(types = list(), value = ""),
          nonoverlapVariable4 = list(types = list(), value = ""),
          overlapVariable1 = list(types = list(), value = ""),
          overlapVariable2 = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          samples = "independent",
          scatterPlot = TRUE,
          scatterPlotDensity = TRUE,
          scatterPlotRegressionLine = TRUE,
          scatterPlotRegressionLineCi = FALSE,
          scatterPlotRegressionLineCiLevel = 0.95) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("commonVariable", "groupingVariable", "independentVariable1", "independentVariable2", "nonoverlapVariable1", "nonoverlapVariable2", "nonoverlapVariable3", "nonoverlapVariable4", "overlapVariable1", "overlapVariable2")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspClassicalTests", "twoCorrelations", "twoCorrelations.qml", options, version, TRUE))
}