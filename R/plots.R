#' Autoplot method for miter predictions
#'
#' @description
#' Creates visualization of model predictions. Automatically detects date
#' variables for time series plots, otherwise creates scatter plots.
#'
#' @param x A `miter_pred` object.
#' @param date_var Character string naming the date variable, or "auto" for
#'   automatic detection. Default is "auto".
#' @param ... Additional arguments passed to `facet_wrap()`.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Plot predictions
#' autoplot(predictions)
#'
#' # Specify date variable
#' autoplot(predictions, date_var = "date")
#' }
#'
#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_abline geom_point labs facet_wrap scale_color_manual geom_errorbar theme_bw geom_dotplot
#' @importFrom rlang sym
#' @export
autoplot.miter_pred <- function(x, date_var = "auto", ...) {
  outcome_var <- attr(x, "outcome")
  ids <- attr(x, "ids")
  if (!is.null(ids)) {
    facet_formula <- as.formula(paste("~", paste(ids, collapse = " + ")))
  }

  has_test <- x %>%
    tidyr::drop_na(.pred, .data[[outcome_var]]) %>%
    nrow()

  if (date_var == "auto") {
    col_types <- purrr::map_lgl(x, is.Date)
    date_var <- names(col_types[which(col_types)])[1]
  }

  if (is.null(date_var) | is.na(date_var)) {
    if(has_test == 0) {
      g <- x %>%
        tidyr::drop_na(.pred) %>%
        dplyr::group_by(across({{ ids }}), models) %>%
        dplyr::mutate(.row = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        ggplot(aes(x = .row, y = .pred, col = models)) +
        geom_point(alpha = 0.5)
    } else {
      g <- x %>%
        ggplot(aes(x = !!sym(outcome_var), y=.pred, col = models)) +
        geom_abline(color = "gray50", lty = 2) + 
        geom_point(alpha = 0.5) + 
        labs(x = "observed", y = "predicted")
    }
  } else {
    g <- ggplot(x, aes(x=!!sym(date_var))) +
      geom_line(aes(y = !!sym(outcome_var), col = "ACTUAL")) +
      geom_line(aes(y = .pred, col = models)) +
      scale_color_manual(
        values = c("black", gg_color_hue(length(unique(x$models))))
      ) +
      labs(color = "models")
  }

  if (!is.null(ids)) {
    g <- g + facet_wrap(facet_formula, scales = "free", ...)
  }
  
  suppressWarnings(g)
}

#' Autoplot method for miter metrics
#'
#' @description
#' Creates visualization of model performance metrics with optional standard
#' error bars if resamples were used.
#'
#' @param x A `miter_metrics` object.
#' @param ... Additional arguments passed to `facet_wrap()`.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' metrics <- calculate_metrics(predictions)
#' autoplot(metrics)
#' }
#'
#' @export
autoplot.miter_metrics <- function(x, ...) {
  ids <- attr(x, "ids")
  if (!is.null(ids)) {
    facet_formula <- as.formula(paste("~", paste(ids, collapse = " + ")))
  }
  has_std <- "std_err" %in% names(x)
  metric_nm <- attr(x, "metric")

  g <- x %>%
    ggplot(aes(x = rank, y = metric, col = models)) +
    geom_point() +
    theme_bw() +
    labs(y = metric_nm)

  if (has_std) {
    g <- g +
      geom_errorbar(
        aes(
          ymin = metric - std_err,
          ymax = metric + std_err
        ),
        width = 0.2
      )
  }

  if (!is.null(ids)) {
    g <- g + facet_wrap(facet_formula, scales = "free", ...)
  }

  suppressWarnings(g)
}
