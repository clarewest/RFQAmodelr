#' Say hello
#'
#' Receive a greeting from RFQAmodelr
#'
#' @param name string. Your name
#' @return Returns a greeting, personalised using \code{name} if provided.
#' @examples
#' say_hello()
#' say_hello("Clare")
#' @export
say_hello <- function(name="stranger"){
  message = paste("Hello, ", name, "!", sep="")
  return(message)
}
