#' List - Repeated CV - GLM
#'
#' @param exprs List of GLM formulas.
#' @param df Data.frame.
#' @param family Object : a description of the error distribution.
#' @param thresh If TRUE, an optimal threshold is found based on the 'statistic' criterion.
#' @param statistic A character indicating which statistic to maximize. Default is "Accuracy". You can use c('Sensitivity', 'Specificity', 'Pos Pred Value', 'Neg Pred Value', 'Precision', 'Accuracy', 'Balanced Accuracy', 'F1', 'Detection Rate', 'Detection Prevalence', 'Prevalence', 'Recall', 'Kappa', 'J', 'Dist').
#' @param number The number of folds.
#' @param repeats The number of complete sets of folds to compute.
#'
#' @return Different accuracy measures for all formulas in 'exprs'.
#' @importFrom pbapply pblapply
#' @importFrom pbapply pboptions
#' @importFrom methods is
#' @export
#'
l.cv = function(exprs, df, family, thresh = FALSE, statistic = "Accuracy", number = 10, repeats = 10){

  #FUNS#############################
  intercept.model = function(expr){
    if(length(all.vars(formula(expr))) == 1){return(TRUE)}else{return(FALSE)}
  }
  ##################################

  #
  if(class(exprs) == "list"){

    #
    if(sum(sapply(exprs, is, class2 = "formula")) == length(exprs)){

      #
      if(sum(sapply(exprs, intercept.model)) == 0){

        pboptions(char = "=")

        return(do.call(rbind.data.frame, pblapply(exprs, OOS::cv, family = family, df = df, thresh = thresh, statistic = statistic, number = number, repeats = number)))

      }else{

        stop("OOS does not support intercept models. At least one of the formulas in 'exprs' is an intercept model.")

      }

    }else{

      stop("Each formula in 'exprs' should be a valid formula.")

    }

  }else{

    stop("'exprs' should be a list.")

  }

}
