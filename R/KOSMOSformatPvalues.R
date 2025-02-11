#' @title Function to format p-values for display in KOSMOS plots
#'
#' @description This function is called in \code{KOSMOSregplot()} to format p-values for display in the little stat stable.
#'
#' @param p_value a numeric value between 0 and 1.
#'
#' @return Returns a \code{expression()}-compatible string starting with \code{=} or \code{<}.
#'
#' @examples
#' KOSMOSformatPvalues(0.0132342)
#' KOSMOSformatPvalues(0.0013234)
#' KOSMOSformatPvalues(0.0001323)
#' KOSMOSformatPvalues(0.0000132)
#' KOSMOSformatPvalues(0.0000013)
#
#' @export

KOSMOSformatPvalues <- function(p_value) {
  if (is.na(p_value)) {
    formatted_value=NaN
  } else if (0.0001 < p_value & p_value < 0.001) {
    closest_larger_decimal <- 10^ceiling(log10(p_value))
    formatted_value=sprintf("< %.3f", closest_larger_decimal)
  } else if (p_value < 0.0001) {
    exponent <- ceiling(log10(p_value))
    formatted_value=bquote(bold("< " * 10^.(exponent)))#bquote(paste("< ",10^.(exponent)))
  } else {
    formatted_value=sprintf("= %.3f", p_value)
  }
  return(formatted_value)
}
