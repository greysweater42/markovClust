#' Markov clustering function
#'
#' Markov clustering function algorithm
#' @param 
#' @keywords 
#' @export
#' @examples

markovClust <- function(M, r=2.5, eStop=0.001) {
    inflation <- function(M, r) return(M ^ r / rowSums(M ^ r))
    frobeniusNorm <- function(M1, M0) return(sqrt(sum((M1 - M0) ^ 2)))
    for (i in 1:100) {
        Mt1 <- M
        M <- M %*% M
        M <- inflation(M, r)
        if (frobeniusNorm(M, Mt1) < 0.001) break
    }
    M <- structure(round(M), class = c("MarkovClustObject", "matrix"))
    return(M)
}

