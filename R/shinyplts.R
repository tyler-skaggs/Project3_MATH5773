#' shinyplts: Runs included shiny app
#'
#'
#' @return Shiny Server
#'
#' @export
#'
#' @examples
#' \dontrun{shinyplts()}
shinyplts <- function(){
  shiny::runApp(system.file("shiny", package="MATH5773Skag0011Proj3"),launch.browser = TRUE)
}
