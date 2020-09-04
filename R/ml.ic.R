#' Multicore l.ic for heavy tasks
#'
#' @param exprs List of GLM formulas.
#' @param df Data.frame.
#' @param family Object : a description of the error distribution.
#' @param penalty numeric specifying the ‘weight’ of the equivalent degrees of freedom. (penalty = 2 is the AIC and penalty = log(n) is the BIC)
#' @param cl Number of CPU cores on the current host.
#'
#' @return The Information Criterion for all formulas in 'exprs'.
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
ml.ic = function(exprs, df, family, penalty = 2, cl = detectCores()){

  #FUNS#############################
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

      start_time <- Sys.time()
      cl = makeCluster(cl, type="SOCK")
      registerDoSNOW(cl)

      pb <- txtProgressBar(max = length(exprs), char = "=", style = 3, width = 50)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)

      OUT = foreach(i = 1:length(exprs), .combine = rbind, .options.snow = opts) %dopar%
        {
          OOS::ic(expr = exprs[[i]], df = df, family = family)
        }

      stopCluster(cl)
      close(pb)
      end_time <- Sys.time()
      cat("elapsed=", round(as.numeric(end_time - start_time), digits = 3), sep = "")
      quiet(.rs.restartR())
      return(OUT)

    }else{

      stop("Each formula in 'exprs' should be a valid formula.")

    }

  }else{

    stop("'exprs' should be a list.")

  }

}
