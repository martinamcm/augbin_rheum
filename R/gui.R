#' Graphical user interface to analyse composite endpoints using the augmented binary method
#'
#' \code{gui()} runs an R Shiny web browser based graphical user interface for
#' \code{\link{augbinrheum}}. For details of Shiny app functionality, 
#' see \href{https://github.com/martinamcm/AugBin}{AugBin}
#' @examples
#' # Launch the graphical user interface
#' \dontrun{gui()}
#' @export
gui <- function() {
  app_dir <- system.file("shiny", "AugBin", package = "augbinrheum")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing augbinrheum.")
  }
  shiny::runApp(app_dir, launch.browser = TRUE, display.mode = "normal")
}
