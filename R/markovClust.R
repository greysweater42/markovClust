#' Markov clustering function
#'
#' Markov clustering function algorithm
#' @param M adjacency matrix
#' @param inflation inflation parameter
#' @param expansion expansion parameter
#' @param eStop stop criterium - Frobenius norm of matrix M_t - M_{t-1} 
#' @param max_it maximum number of iterations
#' @keywords markov clustering, graph clustering
#' @import expm
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


markovClust <- function(M, inflation=2.5, expansion=2, eStop=0.001, max_it=100) {
    f_inflation <- function(M, r) return(M ^ r / rowSums(M ^ r))
    f_frobeniusNorm <- function(M1, M0) return(sqrt(sum((M1 - M0) ^ 2)))
    M <- f_inflation(M, 1)  # initial normalization
    for (i in 1:max_it) {
        Mt1 <- M
        M <- M %^% expansion
        M <- f_inflation(M, inflation)
        if (f_frobeniusNorm(M, Mt1) < 0.001) break
    }
    if (i == nLoops) warning("Convergence criterium not met") 
    l <- list(M=round(M), nLoops=i)
    MCO <- structure(l, class = c("MarkovClustObject", "list"))
    return(MCO)
}

