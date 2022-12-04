#' MLRBootstrap: Bootstrap analysis of MLR model
#'
#' @param model Model to do bootstrap analysis
#' @param data Data for bootstrap analysis
#' @param iter Iterations of the bootstrap
#' @param alpha Used to create 1-alpha CI and etc
#' @param HistBreaks Breaks in the histogram
#'
#' @importFrom purrr map walk
#'
#' @description This function will run a bootstrap analysis on a passed in model. Specifically, we
#'    grab a number of samples and produce \eqn{\beta} estimates of this sampled data. Occasionally it
#'    is not possible to create such an estimate as \eqn{X'X} is sometimes a singular matrix. The output of
#'    this function allows us to look at the distribution of estimates and compare them to the theoretical
#'    distribution.
#'
#' @return This function will return a page of plots. The plots will show the distribution of the
#'    beta values with lines corresponding to the point estimate and intervals.
#'
#'    A named list of the following information is also supplied:
#' \describe{
#'   \item{beta_iters}{The beta values attained from sampling}
#'   \item{CI_beta}{Theoretical distribution}
#'   \item{qbeta}{Intervals from the sampled data, compare to `CI_beta`}
#'   \item{iter}{Number of iterations to calculate}
#'   \item{alpha}{Used to create a 1-alpha interval}
#'   \item{failedSamples}{A list of the samples in which a singular matrix was produced}
#' }
#'
#'
#' @export
#'
#' @examples
#' ## For PRODQUAL Data
#' t <- MLRBootstrap(QUALITY ~ TEMP + PRESSURE + TEMP:PRESSURE + I(TEMP^2), data = PRODQUAL,
#'  iter = 5000, HistBreaks = 50)
#' t
#'
#' ## For Browser Data | will create singular matricies
#' t3 <- MLRBootstrap(TIME ~ NUMBER + BROWSER, data = BROWSER, iter = 1000)
#' t3
MLRBootstrap <- function(model, data, iter = 5000, alpha = 0.05, HistBreaks = 30) {
  ##################### CHECK ARGS ###################
  data <- as.data.frame(data)

  ylm <- tryCatch(
    error = function(cnd) 0,
    lm(model, data)
  )

  if(is.numeric(ylm)){
    cat(red("ERROR: Could not construct linear model, check 'model' and 'data'."))
    my_list = list()
    return(invisible(my_list))
  }

  if(!is.numeric(iter) || iter <= 0){
    cat(red("ERROR: 'iter' should be numeric larger than 0"))
    my_list = list()
    return(invisible(my_list))
  }

  if(!is.numeric(HistBreaks) || HistBreaks <= 0){
    cat(red("ERROR: 'HistBreaks' should be numeric larger than 0"))
    my_list = list()
    return(invisible(my_list))
  }

  if(!is.numeric(alpha) || alpha > 1 || alpha < 0){
    cat(red("ERROR: 'alpha' should be a numeric between 0 and 1"))
    my_list = list()
    return(invisible(my_list))
  }

  ##################### SETUP ########################
  TermNames <- c(colnames(ylm$model)[1])
  for(i in 2:ylm$rank){
    TermNames[i] = colnames(ylm$qr[[1]])[i]
  }

  y = data[[TermNames[1]]]
  X = model.matrix(ylm)
  n = length(y)
  num = length(X[1,])

  ylm <- lm(y ~ X[,2:num])
  beta <- coef(ylm)

  CI <- confint(ylm, level = 1 - alpha)

  singularModels <- list()

  # Function to find betas from sample
  func <- function(index){

    solve <- tryCatch(
      error = function(cnd) "FAIL",
      solve(t(X[index,]) %*% X[index,])
    )

    if(!is.character(solve)){
      return(coef(lm(y[index] ~ X[index,2:num])))
    }
    else{
      singularModels <<- append(singularModels, list(index))
      return()
    }
  }

  #Random Sample
  index <- map(1:iter, ~ sample(1:n, n, replace = TRUE))

  #vector of betas from sample
  vect <- map(index, func)

  #True number of iterations (in case a matrix was not invertable)
  iter_t = length(unlist(sapply(vect, "[[", 1)))

  hbetas <- matrix(nrow = iter_t, ncol = num)

  #Reorganize data into matrix
  for(i in 1:(num-1)){
    hbetas[,i] = unlist(sapply(vect, "[[", i))
  }

  hbetas[,num] = unlist(sapply(vect,tail, 1))

  qbeta = matrix(nrow = num, ncol = 2)

  ################### PLOTTING  ####################

  ColPcent <- function(x = 0.5, cols = c('blueviolet') ){
    val <- col2rgb(cols)
    newcols <- rgb(val[1,],val[2,],val[3,], maxColorValue = 255, alpha = round((255*x)))
    invisible(newcols)
  }

  # Function to make a histogram of the data
  histMaker <- function(x){
    Names = c("(Intercept)", TermNames[2:length(TermNames)])

    HistDensity <- hist(hbetas[,x], breaks = HistBreaks, plot = FALSE)
    Hdens <- HistDensity$density / max(HistDensity$density)

    hist(hbetas[,x], breaks = HistBreaks, col = ColPcent(Hdens),
         xlab = expression(widehat(beta)[x]),
         main = paste0(Names[x],"\nHistogram of beta ", x-1, " estimate\n",
                       "Point estimate: ", round(beta[x],8)))

    ## Theoretical mean and CI
    abline(v = beta[x], col = "#f79e39", lwd = 2.5)
    abline(v = CI[x,1], col = "red", lwd = 2.5)
    abline(v = CI[x,2], col = "red", lwd = 2.5)

    ## 95% interval of each beta
    qbeta[x,] <<- quantile(hbetas[,x], c(alpha/2, (1-alpha/2)), na.rm=TRUE)

    abline(v = qbeta[x,1], col = "blue", lwd = 2.5)
    abline(v = qbeta[x,2], col = "blue", lwd = 2.5)
  }

  ## Decide Layout
  if(num %% 2 == 0){
    layout(matrix(1:num, nrow = ceiling(num/2), ncol = 2, byrow = TRUE))
  }

  else{
    layout(matrix(c(1:num, num), nrow = ceiling(num/2), ncol = 2, byrow = TRUE))
  }

  # Call the Histogram Making Function with walk from purr
  walk(1:num, histMaker)

  #################### RETURN ####################
  my_list <- list(beta_iters = as.data.frame(hbetas),
                  CI_beta = CI,
                  beta_pts = beta,
                  qbeta = qbeta,
                  iter = iter,
                  alpha = alpha,
                  failedSamples = singularModels
                  )

  return(invisible(my_list))
}
