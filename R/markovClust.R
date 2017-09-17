#' Markov clustering function
#'
#' Markov clustering function algorithm
#' @param M adjacency matrix
#' @param r inflation parameter
#' @param eStop stop criterium - Frobenius norm of matrix M_t - M_{t-1} 
#' @keywords markov clustering, graph clustering
#' @export
#' @examples blabla


markovClust <- function(M, r, eStop) {
    inflation <- function(M, r) return(M ^ r / rowSums(M ^ r))
    frobeniusNorm <- function(M1, M0) return(sqrt(sum((M1 - M0) ^ 2)))
    for (i in 1:100) {
        Mt1 <- M
        M <- M %*% M
        M <- inflation(M, r)
        if (frobeniusNorm(M, Mt1) < 0.001) break
    }
    return(round(M))
}

