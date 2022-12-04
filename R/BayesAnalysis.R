#' BayesAnalysis: Bayes analysis utilinsing MCMCpack
#'
#' @param model Model to test
#' @param alpha Used to create 1-alpha CI and etc
#' @param data data for model
#' @param choice choice on how to choose b0
#' @param iter iterations of MCMC
#' @param ... other parameters to use in MCMCregress(). See \link[MCMCpack]{MCMCregress}
#'
#' @importFrom MCMCpack MCMCregress
#' @importFrom gridExtra grid.arrange
#' @importFrom ggmcmc ggs_compare_partial ggs_crosscorrelation ggs_traceplot ggs
#' @importFrom coda crosscorr
#'
#' @description This functions runs a Bayesian analysis on a model sent by the user. We look at
#'    validity of of the sampling (done by Markov Chain Monte Carlo). The calculations are
#'    done in \link[MCMCpack]{MCMCregress}, which utilities a multivariate Gaussian prior on the betas, and
#'    an inverse Gamma prior distribution for the conditional error variance. Any argument for
#'    \link[MCMCpack]{MCMCregress} can also be sent into this function. We also look at any possible
#'    correlation between the samples and variance for the betas.
#'
#'
#' @return This function will return a page of plots. The plots will show the Density
#'    distribution, a comparison of the density distribution when the sampling is partially
#'    done, the trace plots, a HPD plot, and a matrix plot of the correlation (red is negative
#'    correlation and blue is positive).
#'
#'    There is a command line output to give a summary of the posterior distribution.#'
#'
#'    A named list of the following information is also supplied:
#' \describe{
#'   \item{posterior}{The posterior information from the analysis, supplied by `MCMCregress()`}
#'   \item{quantiles}{Quantiles from the posterior distribution}
#'   \item{CI}{Convidence intervals from the theoretical model, compare to `quantiles`}
#'   \item{corMatrix}{Correlation matrix, can be viewed in the plot output too}
#'   \item{postSum}{Object with the information from the summary output}
#' }
#'
#' @export
#'
#' @examples
#' ## Example 1
#' \dontrun{t <- BayesAnalysis(QUALITY ~ TEMP + PRESSURE, PRODQUAL, iter = 10000)}
BayesAnalysis <- function(model, data, alpha = 0.05, choice = "beta", iter = 10000, ...){
  ##################### CHECK ARGS ###################
  data <- as.data.frame(data)

  ylm <- tryCatch(
    error = function(cnd) 0,
    lm(model, data)
  )

  if(is.numeric(ylm)){
    cat(red("ERROR: Could not construct linear model, check 'model' and 'data'.\n"))
    my_list = list()
    return(invisible(my_list))
  }

  if(!is.numeric(alpha) || alpha > 1 || alpha < 0){
    cat(red("ERROR: 'alpha' should be a numeric between 0 and 1"))
    my_list = list()
    return(invisible(my_list))
  }

  if(!is.character(choice)){
    cat(red("ERROR: 'choice' should either be a vector of numeric, 'beta', or 'mean'.\n"))
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

  if(is.character(choice)){
    b0 <- ylm$coefficients
  }

  else{
    b0 <- choice
  }

  posterior <- MCMCregress(formula = model, data = data, mcmc = iter,
                           b0 = b0, ...)

  postNames <- colnames(posterior)

  quants <- list()
  for(i in 1:dim(posterior)[2]){
    quants <- append(quants, list(quantile(posterior[,i], probs = c(alpha/2, 1 - alpha/2))))
  }

  makePlot1 <- function(i){
    p <- (ggplot(as.data.frame(posterior[,i]), aes(x = as.vector(posterior[,i]))) +
            geom_histogram(aes(y = after_stat(density)), bins = 30, colour = 1, fill = "grey") +
            geom_density(color = "red", lwd = 1) +
            geom_vline(xintercept = quants[[i]], col = "blue") +
            labs(x = postNames[i]))

    if(i == 1){
      p <- p + labs(title = "Density Distribution") +
        theme(plot.title = element_text(size=7))
    }

    return(p)
  }
  makePlot2 <- function(i){
    p <- ggs_compare_partial(ggs(posterior), family = c(postNames[i]))
    if(i == 1){
      p <- p + labs(title = "Partial Comparison") +
        theme(plot.title = element_text(size=7))
    }
    return(p)
  }
  makePlot3 <- function(i){
    p <- ggs_traceplot(ggs(posterior), family = c(postNames[i]))
    if(i == dim(posterior)[2] + 1){
      p <- ggs_caterpillar(ggs(posterior))
    }
    if(i == 1){
      p <- p + labs(title = "Trace Plot") +
        theme(plot.title = element_text(size=7))
    }

    if(i == dim(posterior)[2] + 2){
      p <- ggs_crosscorrelation(ggs(posterior)) +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
        labs(title = "CORRELATION") +
        theme(plot.title = element_text(size=7))
    }
    return(p)
  }

  vect1 <- map(1:dim(posterior)[2], makePlot1) #Had to use purrr as ggplot has issues in loops
  vect2 <- map(1:dim(posterior)[2], makePlot2) #Had to use purrr as ggplot has issues in loops
  vect3 <- map(1:(dim(posterior)[2]+2), makePlot3) #Had to use purrr as ggplot has issues in loops

  vect <- c(vect1, vect2, vect3)

  grid.arrange(grobs = as.list(vect),
               layout_matrix = rbind(
                                matrix(1:(3*dim(posterior)[2]), ncol = 3, byrow = FALSE),
                                c(3*dim(posterior)[2]+1, 3*dim(posterior)[2]+1, 3*dim(posterior)[2]+2)
                                )
               )

  names(quants) <- postNames
  PostSum <- summary(posterior)
  print(PostSum)

  #################### RETURN ####################
  my_list <- list(posterior = posterior,
                  quantiles = as.vector(quants),
                  CI = confint(ylm, level = 1 - alpha),
                  corMatrix = crosscorr(posterior),
                  postSum = PostSum
  )

  return(invisible(my_list))
}
#t <- BayesAnalysis(QUALITY ~ TEMP + PRESSURE, PRODQUAL, choice = "mean", iter = 10000)


