#' Summarizing Markov clustering algorithm
#'
#' Summarizing Markov clustering algorithm
#' @param 
#' @keywords 
#' @export
#' @examples

summary.MarkovClustObject <- function(MCO) {
    list(title="Markov clustering algorithm results",
         M=MCO$M,
         nLoops=MCO$nLoops)
}

