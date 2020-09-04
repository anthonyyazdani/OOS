#' Repeated CV - GLM
#'
#' @param expr GLM formula.
#' @param df Data.frame.
#' @param family Object : a description of the error distribution.
#' @param thresh If TRUE, an optimal threshold is found based on the 'statistic' criterion.
#' @param statistic A character indicating which statistic to maximize. Default is "Accuracy". You can use c('Sensitivity', 'Specificity', 'Pos Pred Value', 'Neg Pred Value', 'Precision', 'Accuracy', 'Balanced Accuracy', 'F1', 'Detection Rate', 'Detection Prevalence', 'Prevalence', 'Recall', 'Kappa', 'J', 'Dist').
#' @param number The number of folds.
#' @param repeats The number of complete sets of folds to compute.
#'
#' @return Different accuracy measures.
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom caret twoClassSummary
#' @importFrom caret thresholder
#' @importFrom stats family
#' @importFrom stats optimize
#' @importFrom stats formula
#' @importFrom methods is
#' @import formula.tools
#' @export
#'
cv <- function(expr, df, family, thresh = FALSE, statistic = "Accuracy", number = 10, repeats = 10){

  #FUNS#############################
  intercept.model = function(expr){
    if(length(all.vars(formula(expr))) == 1){return(TRUE)}else{return(FALSE)}
  }

  classification.model = function(family){
    if(class(family) == "family"){

      family_chr = family$family
      if(family_chr == "binomial" | family_chr == "quasibinomial"){return(TRUE)}else{return(FALSE)}

    }else{stop("

             Use parentheses to indicate a family. For example use : [binomial()] instead of [binomial].

             The families supported by this function are:

             - binomial()
             - gaussian()
             - Gamma()
             - inverse.gaussian()
             - quasi()
             - quasibinomial()")}
  }

  y.ind = function(expr, df){
    which(names(df) == all.vars(formula(expr))[1])
  }

  y.get = function(expr, df){
    return(df[,y.ind(expr, df)])
  }

  optimized.class.cv.summary = function(model, statistic){
    g <- function(x){as.numeric(thresholder(model, threshold = x, statistics = statistic)[3])}
    TH = optimize(g,c(0,1), maximum = TRUE)$maximum
    return(thresholder(model,
                       threshold = optimize(g,c(0,1), maximum = TRUE)$maximum,
                       statistics = c('Sensitivity', 'Specificity', 'Pos Pred Value', 'Neg Pred Value',
                                      'Precision', 'Accuracy', 'Balanced Accuracy', 'F1', 'Detection Rate',
                                      'Detection Prevalence', 'Prevalence', 'Recall', 'Kappa', 'J', 'Dist'))[-1])
  }

  class.cv.summary = function(model){
    return(thresholder(model,
                       threshold = 0.5,
                       statistics = c('Sensitivity', 'Specificity', 'Pos Pred Value', 'Neg Pred Value',
                                      'Precision', 'Accuracy', 'Balanced Accuracy', 'F1', 'Detection Rate',
                                      'Detection Prevalence', 'Prevalence', 'Recall', 'Kappa', 'J', 'Dist'))[-1])
  }
  ##################################

  #
  if(is(expr, "formula")){}else{stop("'expr' is not a valid formula.")}

  #
  if(intercept.model(expr)){stop("Intercept models are not allowed.")}

  #
  if(classification.model(family)){

    y = y.get(expr, df)
    y.index = y.ind(expr, df)

    #
    if(is.numeric(y) & (sum(y == 1) + sum(y == 0) == length(y))){

      df[,y.index] <- factor(make.names(df[,y.index]), levels = c("X1", "X0"))

    }else{stop("The response variable should be a numeric vector composed of zeros and ones denoting respectively failures and successes.")}

    #
    if(thresh){

      ctrl <- trainControl(method = "repeatedcv", number = number, repeats = repeats, classProbs = TRUE, savePredictions = "all")
      MODEL_ID_which.thresh <- train(expr, data = df, method = "glm", family = family$family, trControl = ctrl)
      return(cbind.data.frame(expr = as.character(expr), "Pos label" = 1, optimized.class.cv.summary(MODEL_ID_which.thresh, statistic)))

    }else{

      ctrl <- trainControl(method = "repeatedcv", number = number, repeats = repeats, classProbs = TRUE, savePredictions = "all")
      MODEL_ID_which.thresh <- train(expr, data = df, method = "glm", family = family$family, trControl = ctrl)
      return(cbind.data.frame(expr = as.character(expr), "Pos label" = 1, class.cv.summary(MODEL_ID_which.thresh)))

    }

  }else{

    ctrl <- trainControl(method = "repeatedcv", number = number, repeats = repeats)
    return(cbind.data.frame(expr = as.character(expr), train(expr, data = df, method = "glm", family = family$family, trControl = ctrl)$results[-1]))

  }

}
