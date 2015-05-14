
#' @title Concurrency Simulation Shiny App
#'
#' @description Runs a web browser-based GUI of the concurrency microsimulation.
#'
#' @keywords GUI
#' @export
#'
#' @examples
#' \dontrun{
#' concweb()
#' }
#'
concweb <- function() {
  shiny::runApp(system.file("shiny", "app", package = "concurrency.sim"))
}
