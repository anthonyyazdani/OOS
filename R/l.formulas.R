#' Produce all possible formulas for a given data.frame
#'
#' @param y Character : indicating the response column name.
#' @param df Data.frame.
#' @param type 'f', 'fs' or 's' denoting respectively first order, first and second order, second order formulas.
#'
#' @return All possible formulas for a given data.frame.
#' @importFrom stats formula
#' @importFrom stats terms
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom utils combn
#' @export
#'
l.formulas <- function(y, df, type = "f"){

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

    message(paste(format((2^length(x)-1), scientific=F), "formulas are produced."))

    pb <- txtProgressBar(max = length(x)+2*(2^length(x)-1), char = "=", style = 3, width = 50)
    counter = 0

    a = list()
    for (i in 1:length(x)) {
      a = append(a, combn(1:length(x),i,simplify=FALSE))
      counter <- counter + 1
      setTxtProgressBar(pb, counter)
    }

    b = list()
    for (i in 1:length(a)) {
      b = append(b, paste(paste(y, "~", sep = " "),paste(x[a[[i]]],collapse="+")))
      counter <- counter + 1
      setTxtProgressBar(pb, counter)
    }

    c = list()
    for (i in 1:length(b)) {
      c = append(c, formula(b[[i]]))
      counter <- counter + 1
      setTxtProgressBar(pb, counter)
    }

    return(c)

  }else{stop("y should be a character indicating the response column name.")}
}
