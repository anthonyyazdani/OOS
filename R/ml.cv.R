#' Multicore l.cv for heavy tasks
#'
#' @param exprs List of GLM formulas.
#' @param df Data.frame.
#' @param family Object : a description of the error distribution.
#' @param thresh If TRUE, an optimal threshold is found based on the 'statistic' criterion.
#' @param statistic A character indicating which statistic to maximize. Default is "Accuracy". You can use c('Sensitivity', 'Specificity', 'Pos Pred Value', 'Neg Pred Value', 'Precision', 'Accuracy', 'Balanced Accuracy', 'F1', 'Detection Rate', 'Detection Prevalence', 'Prevalence', 'Recall', 'Kappa', 'J', 'Dist').
#' @param number The number of folds.
#' @param repeats The number of complete sets of folds to compute.
#' @param cl Number of CPU cores on the current host.
#'
#' @return Different accuracy measures for all formulas in 'exprs'.
#' @importFrom methods is
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom parallel detectCores
#' @importFrom snow makeCluster
#' @importFrom snow stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @export
#'
ml.cv = function(exprs, df, family, thresh = FALSE, statistic = "Accuracy", number = 10, repeats = 10, cl = detectCores()){

  #FUNS#############################
  intercept.model = function(expr){
    if(length(all.vars(formula(expr))) == 1){return(TRUE)}else{return(FALSE)}
  }

  quiet <- function(x){
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }
  ##################################

  #
  if(class(exprs) == "list"){

    #
    if(sum(sapply(exprs, is, class2 = "formula")) == length(exprs)){

      #
      if(sum(sapply(exprs, intercept.model)) == 0){

        start_time <- Sys.time()
        cl = makeCluster(cl, type="SOCK")
        registerDoSNOW(cl)

        pb <- txtProgressBar(max = length(exprs), char = "=", style = 3, width = 50)
        progress <- function(n) setTxtProgressBar(pb, n)
        opts <- list(progress = progress)

        OUT = foreach(i = 1:length(exprs), .combine = rbind, .options.snow = opts) %dopar%
          {
            OOS::cv(expr = exprs[[i]], df = df, family = family, thresh = thresh, statistic = statistic, number = number, repeats = repeats)
          }

        stopCluster(cl)
        close(pb)
        end_time <- Sys.time()
        cat("elapsed=", round(as.numeric(end_time - start_time), digits = 3), sep = "")
        quiet(.rs.restartR())
        return(OUT)

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
