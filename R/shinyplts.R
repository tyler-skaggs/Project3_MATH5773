#' shinyplts: Runs included shiny app
#'
#' @importFrom shiny runApp
#'
#' @description The app allows one to select the data points on a plot by brushing (points then highlighted in orange)
#'    and then you can plot a new regression line for the selected data. To reset the selected points, double click. Each
#'    additional line will give some data output on the left about the lines. Note that selected points are still viewable
#'    when x and y values are changed.
#'
#' @return Shiny Server app
#'
#' @export
#'
#' @examples
#' \dontrun{shinyplts()}
shinyplts <- function(){
  shiny::runApp(system.file("shiny", package="MATH5773Skag0011Proj3"),launch.browser = TRUE)
}
