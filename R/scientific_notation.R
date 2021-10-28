#' @title Reformat string as scientific notation
#' @description \code{scientific_notation} is a function that takes a character string and returns a character string reformatted to scientific notation
#'
#' @param l Character string to be formatted into scientific notation
#' @param n Number of digits to include prior to conversion to scientific notation
#'
#' @return A character string containing the input string transformed into scientific notation
#' @export
#'
#' @examples
#' scientific_notation(0.00001, 3)
#'
scientific_notation <- function(l, n) {
  checkmate::assert_numeric(l)
  checkmate::assert_numeric(n)
  parse_scientific <- function(l, n) {
    if (l >= 10^-n) {
      return(l)
    } else {
      # turn in to character string in scientific notation
      l <- format(l, scientific = TRUE)
      # fix 0 formatting
      l <- gsub("0e\\+00", "0", l)
      # quote the part before the exponent to keep all the digits
      l <- gsub("^(.*)e", "'\\1'e", l)
      # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2)
      l <- gsub("e\\+", "e", l)
      # turn the 'e+' into plotmath format
      l <- gsub("e", "%*%10^", l)
      # convert 1x10^ or 1.000x10^ -> 10^
      # l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
      # return this as an expression
      return(l)
    }}
  return(purrr::map_chr(l, parse_scientific, n))
}
