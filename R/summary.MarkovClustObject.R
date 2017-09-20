#' Summarizing Markov clustering algorithm
#'
#' Summarizing Markov clustering algorithm
#' @param MCO element of class MarkovClustObject
#' @keywords markov clustering, graph clustering
#' @export
#' @examples
#' A <- rbind(c(1, 1, 0, 1, 0, 1, 0),
#'            c(1, 1, 1, 1, 0, 0, 0),
#'            c(0, 1, 1, 1, 0, 0, 1),
#'            c(1, 1, 1, 1, 1, 0, 0),
#'            c(0, 0, 0, 1, 1, 1, 1),
#'            c(1, 0, 0, 0, 1, 1, 1),
#'            c(0, 0, 1, 0, 1, 1, 1))
#' m <- markovClust(A)
#' summary(m)

summary.MarkovClustObject <- function(MCO) {
    list(title="Markov clustering algorithm results",
         M=MCO$M,
         nLoops=MCO$nLoops)
}

