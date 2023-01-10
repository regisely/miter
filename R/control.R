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
print.control_miter <- function(x, ...) {
  cat("miter control object\n")
  invisible(x)
}
