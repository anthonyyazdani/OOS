#' List - Information criterion
#'
#' @param exprs List of GLM formulas.
#' @param df Data.frame.
#' @param family Object : a description of the error distribution.
#' @param penalty numeric specifying the ‘weight’ of the equivalent degrees of freedom. (penalty = 2 is the AIC and penalty = log(n) is the BIC)
#'
#' @return The Information Criterion for all formulas in 'exprs'.
#' @importFrom pbapply pblapply
#' @importFrom pbapply pboptions
#' @importFrom methods is
#' @export
#'
l.ic = function(exprs, df, family, penalty = 2){

  #
  if(class(exprs) == "list"){

    #
    if(sum(sapply(exprs, is, class2 = "formula")) == length(exprs)){

        pboptions(char = "=")

        return(do.call(rbind.data.frame, pblapply(exprs, OOS::ic, df = df, family = family, penalty = penalty)))

    }else{

      stop("Each formula in 'exprs' should be a valid formula.")

    }

  }else{

    stop("'exprs' should be a list.")

  }

}
