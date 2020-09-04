#' Returning the number of candidate models.
#'
#' @param y Character : indicating the response column name.
#' @param df Data.frame.
#' @param type 'f', 'fs' or 's' denoting respectively first order, first and second order, second order formulas.
#'
#' @return The number of candidate models.
#' @importFrom stats formula
#' @importFrom stats terms
#' @export
#'
h.formulas <- function(y, df, type = "f"){

  if(class(y) == "character"){

    #
    if(type == "f"){
      x = labels(terms(formula(paste(y,"~ .")), data = df))
    }else if(type == "fs"){
      x = labels(terms(formula(paste(y,"~ .^2")), data = df))
    }else if(type == "s"){
      x = labels(terms(formula(paste(y,"~ .^2")), data = df))[-c(1:(dim(df)[2]-1))]
    }else{
      stop("No such type of formula. use 'f', 'fs' or 's'.")
    }

    return((2^length(x)-1))


  }else{stop("y should be a character indicating the response column name.")}

}
