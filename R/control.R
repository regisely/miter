#' Control parameters for miter fitting
#'
#' @description
#' Creates a control object for tuning and fitting models in miter. This function
#' wraps `tune::control_grid()` with additional progress tracking options.
#'
#' @param progress Logical indicating whether to show progress bar. Default is `TRUE`.
#' @param verbose Logical for printing additional information during fitting.
#' @param allow_par Logical to allow parallel processing.
#' @param extract Function for extracting model components during fitting.
#' @param save_pred Logical to save predictions from resampling.
#' @param pkgs Character vector of package names to load for parallel processing.
#' @param save_workflow Logical to save workflow objects.
#' @param event_level Character string for event level handling.
#' @param parallel_over Character string specifying what to parallelize over.
#'
#' @return A `control_miter` object containing control parameters.
#'
#' @examples
#' \dontrun{
#' ctrl <- control_miter(progress = TRUE, verbose = FALSE)
#' }
#'
#' @export
control_miter <- function (progress = TRUE,
                           verbose = FALSE,
                           allow_par = TRUE,
                           extract = NULL,
                           save_pred = TRUE,
                           pkgs = NULL,
                           save_workflow = FALSE,
                           event_level = "first",
                           parallel_over = NULL) {
  tune_control <- tune::control_grid(
                          verbose = verbose,
                          allow_par = allow_par,
                          extract = extract, 
                          save_pred = save_pred,
                          pkgs = pkgs,
                          save_workflow = save_workflow,
                          event_level = event_level, 
                          parallel_over = parallel_over
                        ) 
  out <- c(progress = progress, tune_control)
  class(out) <- "control_miter"
  out
}

#' Control parameters for miter racing
#'
#' @description
#' Creates a control object for racing (ANOVA-based early stopping) during model
#' tuning in miter. This function wraps `finetune::control_race()` with additional
#' progress tracking options.
#'
#' @inheritParams control_miter
#' @param verbose_elim Logical for verbose elimination messages.
#' @param burn_in Integer for minimum number of resamples before elimination.
#' @param num_ties Integer for number of models kept in case of ties.
#' @param alpha Numeric significance level for ANOVA comparisons.
#' @param randomize Logical to randomize the order of parameters.
#' @param backend_options Additional backend options for parallel processing.
#'
#' @return A `control_miter` object configured for racing.
#'
#' @examples
#' \dontrun{
#' ctrl_race <- control_miter_race(burn_in = 5, alpha = 0.01)
#' }
#'
#' @export
control_miter_race <- function (progress = TRUE,
                                verbose = FALSE,
                                verbose_elim = FALSE,
                                allow_par = TRUE,
                                extract = NULL,
                                save_pred = TRUE,
                                burn_in = 3,
                                num_ties = 10,
                                alpha = 0.05,
                                randomize = TRUE,
                                pkgs = NULL,
                                save_workflow = FALSE,
                                event_level = "first",
                                parallel_over = "everything",
                                backend_options = NULL) {
  tune_control <- finetune::control_race(
                          verbose = verbose,
                          verbose_elim = verbose_elim,
                          allow_par = allow_par,
                          extract = extract, 
                          save_pred = save_pred,
                          burn_in = burn_in,
                          num_ties = num_ties,
                          alpha = alpha,
                          randomize = randomize,
                          pkgs = pkgs,
                          save_workflow = save_workflow,
                          event_level = event_level, 
                          parallel_over = parallel_over,
                          backend_options = backend_options
                        ) 
  out <- c(progress = progress, tune_control)
  class(out) <- "control_miter"
  out
}

#' @export
print.control_miter <- function(x, ...) {
  cat("miter control object\n")
  invisible(x)
}

#' @export
print.control_miter_race <- function(x, ...) {
  cat("miter control object\n")
  invisible(x)
}
