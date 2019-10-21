# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

#' Run the included shiny app
#' @export
run_shiny_app <- function () {
  while (!requireNamespace("shiny", quietly = TRUE)) {
    write("Can't run the app without the 'shiny' package.", stderr())
    if (
      interactive() &&
      1 == utils::menu(choices=c("Yes","No"), title="Would you like to install 'shiny' now?") ) {
      utils::install.packages("shiny")
    } else {
      stop("Please install 'shiny' to run the app.", call. = FALSE)
    }
  }

  app_dir <- system.file("csnem-app", package = "csnem")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing 'csnem'.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode="normal")
}
