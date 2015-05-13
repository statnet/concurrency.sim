

#' @title Concurrency Simulation Shiny App
#'
#' @description Runs a web browser-based GUI of the concurrency microsimulation.
#'
#'
#' @seealso \code{\link{dcm}}, \code{\link{icm}}
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
