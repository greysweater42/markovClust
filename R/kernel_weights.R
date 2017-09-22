#' Gaussian kernel weights
#'
#' Gaussian kernel weighhts
#' @param d data
#' @param phi some parameter, usually 0.5
#' @param k k newarest neighbours
#' @keywords markov clustering, graph clustering
#' @useDynLib markovClust
#' @export
#' @examples 

kernel_weights <- function(d, phi=0.5, k) {
    X <- t(as.matrix(d))
    storage.mode(X) <- "double"
    p <- as.integer(nrow(X))
    n <- as.integer(ncol(X))
    phi <- as.double(phi)
    w <- double(n*(n-1)/2)
    sol <- .C('kernel_weights',X=X,p=p,n=n,phi=phi,w=w)
    A <- matrix(0, n, n)
    A[lower.tri(A)] <- sol$w
    A <- A + t(A) + diag(n)
    if (!missing(k)) {
        for (i in 1:nrow(A)) {
            threshold <- sort(A[i,], decreasing=T)[k+1]
            A[i, A[i,] < threshold] <- 0
        }
        A[!A == t(A)] <- 0
    }
    return(A)
}

