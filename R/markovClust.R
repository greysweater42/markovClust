#' Markov clustering function
#'
#' Markov clustering function algorithm
#' @param M adjacency matrix
#' @param r inflation parameter
#' @param eStop stop criterium - Frobenius norm of matrix M_t - M_{t-1} 
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
#' D <- diag(rowSums(A))
#' M0 <- solve(D) %*% A
#' markovClust(M0, 2.5, 0.001)


markovClust <- function(M, r=2.5, eStop=0.001, nLoops=100) {
    inflation <- function(M, r) return(M ^ r / rowSums(M ^ r))
    frobeniusNorm <- function(M1, M0) return(sqrt(sum((M1 - M0) ^ 2)))
    for (i in 1:nLoops) {
        Mt1 <- M
        M <- M %*% M
        M <- inflation(M, r)
        if (frobeniusNorm(M, Mt1) < 0.001) break
    }
    l <- list(M=round(M), nLoops=i)
    MCO <- structure(l, class = c("MarkovClustObject", "list"))
    return(MCO)
}

