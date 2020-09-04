#' Information criterion
#'
#' @param expr GLM formula.
#' @param df Data.frame.
#' @param family Object : a description of the error distribution.
#' @param penalty numeric specifying the ‘weight’ of the equivalent degrees of freedom. (penalty = 2 is the AIC and penalty = log(n) is the BIC)
#'
#' @return The Information Criterion for expr.
#' @importFrom stats family
#' @importFrom stats extractAIC
#' @importFrom stats glm
#' @import formula.tools
#' @export
#'
ic = function(expr, df, family, penalty = 2){

  #
  if(is(expr, "formula")){}else{stop("'expr' is not a valid formula.")}

  #
  OUT = cbind.data.frame(as.character(expr), extractAIC(glm(formula = expr, data = df, family = family), k = penalty)[2])
  names(OUT) = c("expr", paste("IC.penalty.",penalty, sep = ""))
  return(OUT)

}
