#' Multicore l.formulas for heavy tasks
#'
#' @param y Character : indicating the response column name.
#' @param df Data.frame.
#' @param type 'f', 'fs' or 's' denoting respectively first order, first and second order, second order formulas.
#' @param cl Number of CPU cores on the current host.
#'
#' @return All possible formulas for a given data.frame.
#' @importFrom stats formula
#' @importFrom stats terms
#' @importFrom utils combn
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
ml.formulas = function(y, df, type = "f", cl = detectCores()){

  #FUNS#############################
  x.get.names = function(i,x){
    x[i]
  }

  quiet = function(x){
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }
  ##################################

  if(type == "f"){
    x = labels(terms(formula(paste(y,"~ .")), data = df))
  }else if(type == "fs"){
    x = labels(terms(formula(paste(y,"~ .^2")), data = df))
  }else if(type == "s"){
    x = labels(terms(formula(paste(y,"~ .^2")), data = df))[-c(1:(dim(df)[2]-1))]
  }else{
    stop("No such type of formula. use 'f', 'fs' or 's'.")
  }

  start_time = Sys.time()
  cl = makeCluster(cl, type="SOCK")
  registerDoSNOW(cl)

  pb = txtProgressBar(max = length(x), char = "=", style = 3, width = 50)
  progress = function(n) setTxtProgressBar(pb, n)
  opts = list(progress = progress)

  OUT = foreach(i = 1:length(x), .combine = append, .options.snow = opts) %dopar%
    {
      a = combn(1:length(x),i,simplify=FALSE)
      b = lapply(a, x.get.names, x = x)
      c = lapply(b, paste, collapse = "+")
      d = paste(rep(paste(y, "~", sep = ""), length(c)), c, sep = "")
      return(lapply(d, formula))
    }

  stopCluster(cl)
  close(pb)
  end_time = Sys.time()
  cat("elapsed=", round(as.numeric(end_time - start_time), digits = 3), sep = "")
  quiet(.rs.restartR())
  return(OUT)

}
