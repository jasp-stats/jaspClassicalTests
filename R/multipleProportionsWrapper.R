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

#' > 2 Proportions
#'
#' Tests whether the proportion of successes is the same across several groups.
#' ## Assumptions
#' - Independent observations within and between groups.
#' - A binary outcome (success/failure) recorded for each group.
#'
#' @param continuityCorrection, Applies Yates' continuity correction to the chi-square statistic.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlot, Displays a plot of observed counts or proportions.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlotCi, Adds Clopper-Pearson confidence intervals for θ to the plot. Only available when the descriptives are displayed as proportions.
#'    Defaults to \code{TRUE}.
#' @param descriptivesTable, Displays a table of observed counts or proportions and the sample size per group. Rows with an empty cell in any of the assigned variables are excluded, and a footnote reports how many were removed.
#'    Defaults to \code{FALSE}.
#' @param descriptivesTableCi, Per-group Clopper-Pearson confidence intervals for the proportion θ. Only available when the descriptives are displayed as proportions.
#'    Defaults to \code{FALSE}.
#' @param factor, The grouping variable that defines the groups whose proportions are compared.
#' @param sampleSize, For aggregated data, the total number of observations per group. Leave empty for individual data. Rows with an empty cell in any assigned variable are excluded from the analysis.
#' @param successes, For aggregated data, the number of successes per group. For individual data, the binary variable indicating a success or failure. Rows with an empty cell in any assigned variable are excluded from the analysis.
#' @param vovkSellke, An upper bound on how much more likely the data are under the alternative hypothesis than under the null.
#'    Defaults to \code{FALSE}.
multipleProportions <- function(
          data = NULL,
          version = "1",
          continuityCorrection = FALSE,
          descriptivesDisplay = "counts",
          descriptivesPlot = FALSE,
          descriptivesPlotCi = TRUE,
          descriptivesPlotCiLevel = 0.95,
          descriptivesTable = FALSE,
          descriptivesTableCi = FALSE,
          descriptivesTableCiLevel = 0.95,
          factor = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          sampleSize = list(types = list(), value = ""),
          successes = list(types = list(), value = ""),
          vovkSellke = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("factor", "sampleSize", "successes")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspClassicalTests", "multipleProportions", "multipleProportions.qml", options, version, TRUE))
}