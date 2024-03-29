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
