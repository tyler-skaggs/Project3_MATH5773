#' MLRModInfo: Gain information about a given model and find potential better models
#'
#' @param model Model to test
#' @param data data for the model to test
#' @param alpha Value for 1 - alpha CI
#'
#' @importFrom ggplot2 ggplot geom_point aes geom_segment geom_hline theme_bw geom_abline
#'    geom_col stat_qq_line stat_qq labs ggtitle scale_color_continuous
#' @importFrom gridExtra grid.arrange
#' @importFrom crayon red
#'
#' @return Fancy List
#' @export
#'
#' @examples
#' ## Example 1
#' ylm <- lm(QUALITY ~ TEMP + PRESSURE + I(TEMP^2) + I(PRESSURE^2) + TEMP:PRESSURE, data = PRODQUAL)
#' t1 <- MLRModInfo(ylm, PRODQUAL, alpha = 0.05)
#' summary(t1)
#'
#' ## Example 2 | From s20x
#' butterfat.fit<-lm(Butterfat~Breed+Age+Breed*Age,data=BUTTERFAT)
#' t2 <- MLRModInfo(butterfat.fit, BUTTERFAT)
#' summary(t2)
#' plot(t2)
MLRModInfo <- function(model, data, alpha = 0.05){
  ##################### CHECK ARGS ###################
  data <- as.data.frame(data)

  ylm <- tryCatch(
    error = function(cnd) 0,
    lm(model, data)
  )

  if(is.numeric(ylm)){
    cat(red("ERROR: Could not construct linear model, check 'model' and 'data'."))
    my_list = list()
    class(my_list) <- "skag0011Fun1"
    return(invisible(my_list))
  }

  ##################### SETUP ########################
  TermNames <- c(colnames(ylm$model)[1])
  for(i in 2:ylm$rank){
    TermNames[i] = colnames(ylm$qr[[1]])[i]
  }

  Ydat <- data[[TermNames[1]]]

  residuals <- resid(ylm)

  ###################### PLOTS ##########################

  g1 <- ggplot(data.frame(Index = 1:length(residuals), residuals), aes(x = Index, y = residuals)) +
    geom_point(aes(color = abs(residuals), size = abs(residuals)), show.legend = FALSE) +
    geom_segment(aes(xend = Index, yend = 0)) +
    scale_color_continuous(low = "green", high = "red") +
    geom_hline(yintercept = 0) +
    theme_bw() + ggtitle("Residuals")

  #Cook's Distance
  g2 <- ggplot(ylm, aes(seq_along(.cooksd), .cooksd)) +
    geom_abline(intercept = qf(0.9, ylm$rank, dim(data)[1] - ylm$rank), slope = 0) +
    geom_col() + ggtitle("Cook's Distance")

  #Normal QQ
  g3 <- ggplot(data, mapping = aes(sample = Ydat)) +
    stat_qq_line() +
    stat_qq() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    ggtitle("QQ-PLOT")

  grid.arrange(g2, g3, g1,
               layout_matrix = rbind(c(1, 2), c(3, 3)))

  # Looking at influence measures
  infl <- influence.measures(ylm)
  ## Index of which item it is (row)
  Index <- which(infl$is.inf == TRUE) %% dim(infl$is.inf)[1]
  ##Finding the type of influence caught (column)
  InfType <- colnames(as.data.frame(infl$is.inf))[ceiling(which(infl$is.inf == TRUE)/dim(infl$is.inf)[1])]

  ylm.step <- step(ylm, scope = butterfat ~ 1, direction = "backward")


  ###################### RETURN and CLASS ##########################
  my_list <- list(InfMeasures = data.frame(Index, InfType)[order(Index, decreasing = FALSE), ],
                  model = model,
                  anova = anova(ylm),
                  resids = residuals,
                  Fval = summary(ylm)$fstatistic,
                  r.sqd = summary(ylm)$adj.r.squared,
                  AIC = AIC(ylm),
                  shapiro = (shapiro.test(Ydat)),
                  CI = confint(ylm, level = 1 - alpha),
                  NewModel = ylm.step,
                  names = TermNames
                  )

  class(my_list) <- "skag0011Fun1"
  return(invisible(my_list))
}

