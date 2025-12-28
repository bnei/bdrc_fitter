#' Estimate Breakpoints for Segmented Regression
#'
#' Uses segmented linear regression to find optimal breakpoint(s) in the
#' relationship between a predictor and response variable.
#'
#' @param formula A formula of the form `response ~ predictor`
#' @param data A data frame containing the variables in the formula
#' @param num_breaks Number of breakpoints to estimate (default: 1)
#'
#' @return A numeric vector of estimated breakpoint values
#'
#' @details
#' Fits an initial linear model and uses [segmented::segmented()] to identify
#' optimal locations where the linear relationship changes slope.
#'
estimate_breaks <- function(
  formula,
  data,
  num_breaks = 1
) {

  # Fit an initial linear model to the curve
  fit_naive <- lm(formula, data = data)

  # Extract the predictor variable name from formula
  predictor <- all.vars(formula)[2]

  # Estimate breakpoint(s) using segmented regression
  segfit <- segmented::segmented(
    fit_naive,
    seg.Z = as.formula(paste0("~", predictor)),
    npsi = num_breaks
  )

  # Extract breakpoint values from the result
  breakpoints <- segfit$psi[, 2]

  return(as.numeric(breakpoints))
}

#' Extract Median Parameters from BDRC Model
#'
#' Extracts the three power-law parameters (a, b, c) from a fitted BDRC model's
#' posterior median estimates.
#'
#' @param model A BDRC model object (from [bdrc::plm()]) or NULL
#'
#' @return A list with names `a`, `b`, `c` containing median posterior estimates,
#'   or NULL if the model is invalid or missing param_summary.
#'
#' @details
#' This is a helper function used by [extract_parameters()] and Shiny reactives.
#' Returns NULL for graceful handling of NULL/incomplete models.
#'
get_medians <- function(model) {
  if (is.null(model) || is.null(model$param_summary)) {
    return(NULL)
  }
  med <- model$param_summary$median
  list(a = med[[1]], b = med[[2]], c = med[[3]])
}

#' Split Data into Segments
#'
#' Partitions data into segments based on specified breakpoint values.
#'
#' @param formula A formula of the form `response ~ predictor`
#' @param data A data frame containing the variables in the formula
#' @param breakpoints A numeric vector of breakpoint values to use for segmentation
#'
#' @return A list of data frames, one for each segment. Segments are ordered by
#'   predictor value with boundaries defined by breakpoints.
#'
#' @details
#' Each segment includes data points where the predictor is strictly greater than
#' the lower bound and less than or equal to the upper bound.
#'
#' @import dplyr
#' @import purrr
#'
split_data <- function(
  formula,
  data,
  breakpoints
) {

  # Sort breakpoints and add -Inf and Inf to cover all ranges
  breakpoints <- sort(c(-Inf, breakpoints, Inf))
  predictor <- all.vars(formula)[2]

  # Use map() to create segments for each breakpoint interval
  seq_len(length(breakpoints) - 1) %>%
    purrr::map(~data %>%
      dplyr::filter(
        .data[[predictor]] > breakpoints[.x] &
          .data[[predictor]] <= breakpoints[.x + 1]
      ))
}

#' Fit Multiple BDRC Models to Data Segments
#'
#' Applies Bayesian Discharge Rating Curve (BDRC) model fitting to each segment
#' of data separately.
#'
#' @param formula A formula of the form `response ~ predictor`
#' @param data A data frame containing the variables in the formula
#' @param breakpoints A numeric vector of breakpoint values
#'
#' @return A list of BDRC model objects (class "plm"), one per segment.
#'   Segments with <= 1 row are returned as `NULL`.
#'
#' @details
#' Uses [bdrc::plm()] to fit power-law models of the form `Q = a(H-c)^b`
#' to each segment independently.
#'
#' @import purrr
#'
fit_multiple <- function(
  formula,
  data,
  breakpoints
) {

  # Split data into segments and fit models using functional programming
  split_data(formula, data, breakpoints) %>%
    purrr::map(~if (nrow(.x) > 1) bdrc::plm(formula, data = .x) else NULL)
}

#' Extract Parameters from BDRC Models
#'
#' Extracts median posterior estimates of rating curve parameters (a, b, c)
#' from fitted BDRC models.
#'
#' @param models A list of BDRC model objects (output from [fit_multiple()])
#'
#' @return A tibble with columns:
#'   - `a`, `b`, `c`: median posterior parameter estimates
#'   - `segment`: segment index (1-based)
#'   Rows correspond to non-NULL models in the input list.
#'
#' @details
#' Uses [get_medians()] to extract the three power-law parameters.
#' Skips NULL models (segments with insufficient data).
#'
#' @import purrr
#' @import dplyr
#'
extract_parameters <- function(models) {
  models %>%
    purrr::map_dfr(get_medians) %>%
    dplyr::mutate(segment = dplyr::row_number())
}

#' Refine Breakpoints via Curve Intersection
#'
#' Computes intersection points between adjacent fitted power-law segments.
#' These intersections provide refined (continuous) breakpoint estimates.
#'
#' @param parameters A tibble with columns `a`, `b`, `c` (output from [extract_parameters()])
#'
#' @return A numeric vector of refined breakpoint values, one for each pair of
#'   adjacent segments. Returns `NA` if no intersection is found for a pair.
#'
#' @details
#' For adjacent segments with parameters (a1, b1, c1) and (a2, b2, c2),
#' solves: `a1 * (x - c1)^b1 = a2 * (x - c2)^b2`
#' using numerical root finding ([uniroot()]). A grid search is used to locate
#' a sign-change bracket first.
#'
refine_breakpoints <- function(parameters) {
  refined_breakpoints <- c()

  # Return empty vector for null or too-small input
  if (is.null(parameters) || nrow(parameters) < 2) {
    return(refined_breakpoints)
  }

  for (i in 1:(nrow(parameters) - 1)) {
    a1 <- parameters$a[i]
    b1 <- parameters$b[i]
    c1 <- parameters$c[i]

    a2 <- parameters$a[i + 1]
    b2 <- parameters$b[i + 1]
    c2 <- parameters$c[i + 1]

    # Define function for root finding; only valid for x > max(c1,c2)
    intersection_func <- function(x) {
      left <- a1 * (x - c1)^b1
      right <- a2 * (x - c2)^b2
      left - right
    }

    lower_bound <- max(c1, c2) + 1e-6

    # Search over a grid to find a bracket where the function changes sign
    max_search <- lower_bound + 1e5
    grid <- seq(lower_bound, max_search, length.out = 1000)
    vals <- suppressWarnings(tryCatch(intersection_func(grid), error = function(e) rep(NA_real_, length(grid))))

    # Find adjacent indices where sign change occurs and values are finite
    bracket_idx <- which(is.finite(vals[-1]) & is.finite(vals[-length(vals)]) & (vals[-1] * vals[-length(vals)] <= 0))

    intersection <- NA_real_
    if (length(bracket_idx) > 0) {
      j <- bracket_idx[1]
      lower <- grid[j]
      upper <- grid[j + 1]
      intersection <- tryCatch({
        uniroot(intersection_func, lower = lower, upper = upper)$root
      }, error = function(e) NA_real_)
    }

    refined_breakpoints <- c(refined_breakpoints, intersection)
  }

  return(refined_breakpoints)
}

#' Plot Rating Curve with Confidence Bands
#'
#' Creates a ggplot2 visualization of observed discharge points and fitted
#' power-law rating curve with confidence bands.
#'
#' @param points A data frame with columns `H` (stage) and `Q` (discharge)
#' @param curve_data A data frame from [bdrc::plm()]$rating_curve with columns
#'   `h`, `median`, `upper`, `lower`
#'
#' @return A ggplot2 object ready for display
#'
#' @details
#' Displays observed data as points, median fit as a solid line, and confidence
#' interval bounds as dotted lines. Suitable for use in Shiny reactives.
#'
#' @import ggplot2
#'
plot_rating_curve <- function(points, curve_data) {
  ggplot2::ggplot() +
    ggplot2::geom_point(data = points, ggplot2::aes(x = .data[["Q"]], y = .data[["H"]])) +
    ggplot2::geom_line(data = curve_data, ggplot2::aes(x = .data[["median"]], y = .data[["h"]])) +
    ggplot2::geom_line(data = curve_data, ggplot2::aes(x = .data[["upper"]], y = .data[["h"]]), linetype = "dotted") +
    ggplot2::geom_line(data = curve_data, ggplot2::aes(x = .data[["lower"]], y = .data[["h"]]), linetype = "dotted") +
    ggplot2::labs(x = "Discharge (Q)", y = "Stage (H)", title = "Rating Curve Fit") +
    ggplot2::theme_minimal()
}