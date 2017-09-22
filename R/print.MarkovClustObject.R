#' Print Markov clustering algorithm
#'
#' Print Markov clustering algorithm
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
#' markovClust(A)

print.MarkovClustObject <- function(MCO) {
    cat("Markov clustering algorithm\n\n")
    cat("nLoops: ", MCO$nLoops, "\n\n")
    cat("order: ", MCO$ord, "\n\n")
    cat("cluster sizes: \n")
    print(MCO$nums)
    cat("\n")
    cat("contingency table:\n")
    print(MCO$cont)
}

