#' BayesAnalysis: Bayes analysis utilinsing MCMCpack
#'
#' @param model Model to test
#' @param data data for model
#' @param choice choice on how to choose b0
#' @param iter iterations of MCMC
#' @param ... other parameters to use in MCMCregress()
#'
#' @importFrom MCMCpack MCMCregress
#'
#' @return Fancy list
#' @export
#'
#' @examples
#' ## Example 1
#' BayesAnalysis(QUALITY ~ TEMP + PRESSURE, PRODQUAL, choice = "mean", iter = 10000)
BayesAnalysis <- function(model, data, choice = "beta", iter = 10000, ...){
  ##################### CHECK ARGS ###################
  data <- as.data.frame(data)

  ylm <- tryCatch(
    error = function(cnd) 0,
    lm(model, data)
  )

  if(choice != "beta" || choice != "mean" || !is.numeric(choice)){
    cat(red("ERROR: 'choice' should either be a vector of numeric, 'beta', or 'mean'.\n"))
    my_list = list()
    return(invisible(my_list))
  }

  if(is.numeric(ylm)){
    cat(red("ERROR: Could not construct linear model, check 'model' and 'data'.\n"))
    my_list = list()
    return(invisible(my_list))
  }

  if(!is.numeric(iter) || iter <= 0){
    cat(red("ERROR: 'iter' should be numeric larger than 0\n"))
    my_list = list()
    return(invisible(my_list))
  }

  ##############################################################
  TermNames <- c(colnames(ylm$model)[1])
  for(i in 2:ylm$rank){
    TermNames[i] = colnames(ylm$qr[[1]])[i]
  }

  if(choice == "mean"){
    b0 <- c(mean(data[[TermNames[1]]]))
    for(i in 2:ylm$rank){
      b0 = c(b0, mean(model.matrix(ylm)[,i]))
    }
  }

  if(choice == "beta"){
    b0 <- ylm$coefficients
  }

  else{
    b0 = choice
  }

  posterior <- MCMCregress(formula = model, data = data, mcmc = iter,
                           b0 = b0, ...)


  plot(posterior)
  summary(posterior)
}
