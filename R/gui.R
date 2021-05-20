#' Graphical user interface to multi-arm trial design determination
#'
#' \code{gui()} run an R Shiny web browser based graphical user interface for
#' augbinrheum. For details of app functionality, see \href{https://github.com/martinamcm/AugBin}{AugBin}
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
