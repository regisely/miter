
add_attr <- function(x, name, attribute) {
  attr(x, name) <- attribute
  x
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

control_miter_to_tune <- function (control = control_miter()) {
  out <- tune::control_grid(
                 verbose = control$verbose,
                 allow_par = control$allow_par,
                 extract = control$extract,
                 save_pred = control$save_pred,
                 pkgs = control$pkgs,
                 save_workflow = control$save_workflow,
                 event_level = control$event_level,
                 parallel_over = control$parallel_over
               )
  out
}
