#' summary.skag0011Fun1: addition to summary()
#'
#' @param object Return form MLRModInfo()
#' @param ... additional arguments affecting the summary produced
#'
#' @return Command line Output
#' @export
#'
#' @examples
#' \dontrun{ summary(MLRModInfo(QUALITY ~ TEMP, PRODQUAL))}
summary.skag0011Fun1 <- function(object, ...){

  if(length(object) == 0){
    cat(red("ERROR: No Information for data\n"))
    return(invisible())
  }

  if(is.na(object$InfMeasures[[1]][1])){
    cat("No found High Influence Values\n")
  }
  else{
    cat("High Influence Data:\n")
    print(object$InfMeasures)
  }
  cat("-----------------------------------------------------\n\n")
  print(object$shapiro)

  cat("-----------------------------------------------------\n\n")
  print(object$anova)

  cat("-----------------------------------------------------\n\n")
  cat("Confidence Interval of Beta Values\n")
  print(object$CI)

  cat("-----------------------------------------------------\n")
  cat("-----------------------------------------------------\n\n")
  cat("'New' Model from Step Function\n")
  print(object$NewModel)

  cat("-----------------------------------------------------\n\n")
  cat("Summary of 'New' Model\n")
  print(summary(lm(object$NewModel)))
  cat("AIC: ")
  cat(paste(AIC(object$NewModel)))

  cat("\n-----------------------------------------------------\n\n")
}
