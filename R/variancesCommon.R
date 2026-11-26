#
# Copyright (C) 2013-2018 University of Amsterdam
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

# Shared helpers for the variance analyses (singleVariance, multipleVariances).

# Build a sample with exactly the requested size and variance so summarized input
# runs through the same var.test / bartlett.test code path as raw data (multipleVariances).
.syntheticSampleSV <- function(n, variance) {
  z <- seq_len(n) - (n + 1) / 2 # centered sequence
  z / sd(z) * sqrt(variance)
}

# Bonett needs the raw values (kurtosis) and the bootstrap needs a real sample; the synthetic
# sample built by .syntheticSampleSV has the requested variance but is a deterministic, equally
# spaced sequence, so resampling it is meaningless. Fall back to the chi-square interval, which
# depends on nothing but the variance and the sample size.
.ciMethodVar <- function(options) {
  if (options[["inputType"]] == "rawData")
    return(options[["ciMethod"]])

  return("chiSquare")
}

# Adapted from Andri Signorell's VarCI in DescTools 0.99.60 (GPL >= 2).
# https://github.com/cran/DescTools/blob/0.99.60/R/StatsAndCIs.r#L4377-L4424
.varCi <- function(x, method = c("classic", "bonett", "norm", "basic", "stud", "perc", "bca"),
                   conf.level = 0.95, sides = c("two.sided", "left", "right"), na.rm = FALSE, R = 999) {
  if (na.rm)
    x <- na.omit(x)

  method <- match.arg(method)
  sides <- match.arg(sides)
  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)

  if (method == "classic") {
    alpha <- (1 - conf.level) / 2
    df <- length(x) - 1
    estimate <- var(x)
    lower <- df * estimate / stats::qchisq(alpha, df, lower.tail = FALSE)
    upper <- df * estimate / stats::qchisq(alpha, df)
  } else if (method == "bonett") {
    z <- qnorm(1 - (1 - conf.level) / 2)
    n <- length(x)
    correction <- n / (n - z)
    estimate <- var(x)
    trimmedMean <- mean(x, trim = 1 / (2 * (n - 4)^0.5))
    kurtosis <- n * sum((x - trimmedMean)^4) / sum((x - mean(x))^2)^2
    se <- correction * sqrt((kurtosis - (n - 3) / n) / (n - 1))
    lower <- exp(log(correction * estimate) - z * se)
    upper <- exp(log(correction * estimate) + z * se)
  } else {
    resamples <- boot::boot(x, function(data, indices) var(data[indices], na.rm = na.rm), R = R)
    ci <- boot::boot.ci(resamples, conf = conf.level, type = method)
    estimate <- resamples$t0
    # boot.ci stores the bounds in different positions for the normal interval.
    firstBound <- if (method == "norm") 2L else 4L
    lower <- ci[[4L]][firstBound]
    upper <- ci[[4L]][firstBound + 1L]
  }

  result <- c(var = estimate, lwr.ci = lower, upr.ci = upper)
  if (sides == "left")
    result[3L] <- Inf
  else if (sides == "right")
    result[2L] <- 0
  return(result)
}

# Map the GUI method name onto the variance CI method argument.
.varCiMethodArg <- function(method) {
  switch(method,
         "chiSquare" = "classic",
         "bonett"    = "bonett",
         "bootstrap" = "bca")
}

# One entry point for every variance CI in the module. Returns list(lower, upper, error).
# The SD interval is the square root of this interval (monotone reparameterisation preserves
# coverage exactly), so callers never bootstrap twice.
.varianceCi <- function(x, options, sides = "two.sided") {
  method <- .varCiMethodArg(.ciMethodVar(options))

  args <- list(x = x, method = method, conf.level = options[["confLevel"]], sides = sides)
  if (method == "bca")
    args$R <- options[["bootstrapSamples"]]

  ciRes <- try(do.call(.varCi, args), silent = TRUE)
  if (isTryError(ciRes))
    return(list(lower = NA_real_, upper = NA_real_, error = .extractErrorMessage(ciRes)))

  return(list(lower = unname(ciRes["lwr.ci"]), upper = unname(ciRes["upr.ci"]), error = NULL))
}

# A CI for the variance maps to a CI for the standard deviation by sqrt(): coverage is preserved
# exactly because the transformation is monotone. Guards against a numerically negative lower
# bound (bootstrap endpoints are always >= 0, but Bonett's are computed on the log scale).
.sdCiFromVarianceCi <- function(ci) {
  return(list(lower = sqrt(max(ci$lower, 0)), upper = sqrt(max(ci$upper, 0)), error = ci$error))
}

# Footnote naming the CI method actually used, so the user can tell a BCa interval from a
# chi-square one (and sees why the choice was overridden for summarized input).
.varianceCiFootnotesVar <- function(table, options, minN = Inf) {
  method <- .ciMethodVar(options)

  table$addFootnote(switch(method,
    "chiSquare" = gettext("Confidence intervals are \u03C7\u00B2 intervals."),
    "bonett"    = gettext("Confidence intervals are based on Bonett's method."),
    "bootstrap" = gettextf("Confidence intervals are BCa bootstrap intervals based on %i replicates.",
                           options[["bootstrapSamples"]])
  ))

  if (options[["inputType"]] != "rawData" && options[["ciMethod"]] != "chiSquare")
    table$addFootnote(gettext("Bonett's method and the bootstrap require the raw observations, so the \u03C7\u00B2 interval is used for summarized data."))

  # Schenker (1985): the resampled EDF cannot see tail behaviour beyond the observed values,
  # so every nonparametric bootstrap interval for a variance under-covers in small samples.
  if (method == "bootstrap" && minN < 20)
    table$addFootnote(gettext("Bootstrap intervals for a variance can under-cover in small samples; Bonett's interval is preferable here."))

  return()
}

# JASP removes an output element only when one of its dependencies changes. Elements that a
# builder simply stops creating (raw-data-only plots after a switch to summarized input, an
# assumption-check container whose boxes are all unticked) would otherwise linger, so remove
# them explicitly on every invocation.
.removeJaspElement <- function(parent, key) {
  if (!is.null(parent) && !is.null(parent[[key]]))
    parent[[key]] <- NULL

  return()
}
