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
#' markovClust(A)


markovClust <- function(M, inflation=2.5, expansion=2, e_stop=0.001, 
                        max_it=100, name) {
    f_inflation <- function(M, r) return(M ^ r / rowSums(M ^ r))
    f_frobeniusNorm <- function(M1, M0) return(sqrt(sum((M1 - M0) ^ 2)))
    M <- f_inflation(M, 1)  # initial normalization
    for (i in 1:max_it) {
        Mt1 <- M
        M <- M %^% expansion
        M <- f_inflation(M, inflation)
        if (f_frobeniusNorm(M, Mt1) < e_stop) break
    }
    if (i == max_it) warning("Stop criterium not met") 
    M <- round(M)  # to do
    poles <- colSums(M) != 0
    ord <- colSums(t(M[, poles]) * 1:sum(poles))
    cont <- if (!missing(name)) table(name, ord) else NULL
    l <- list(M=round(M), nLoops=i, ord=ord, cont=cont)
    MCO <- structure(l, class = c("MarkovClustObject", "list"))
    return(MCO)
}

