
nbi < - function (A){
    # A is the n x m adjacencny matrix here
    n <- nrow(A)
    m <- ncol(A)
    # You need to calculate the degree of columns to use it as node weight
    Ky <- diag(1/colSums(A))
    Ky[is.infinite(Ky) | is.na(Ky)] <- 0
    kx <- rowSums(A)
    Nx <- 1/(matrix(kx, nrow=n, ncol=n, byrow=TRUE))
    Nx[is.infinite(Nx) | is.na(Nx)] <- 0 
    kx[is.infinite(kx) | is.na(kx)] <- 0 
    
    W <- t(A %*% Ky)
    W <- A %*% W
    W <- Nx * W
    rownames(W) <- rownames(A)
    colnames(W) <- rownames(A)
    R <- W %*% A
    return (R)
}

nbi.mod <- function(A){
    # A is the adjacencny matrix here
    lambda = 0.5
    n <- nrow(A)
    m <- ncol(A)
    Ky <- diag(1/colSums(A))
    Ky[is.infinite(Ky) | is.na(Ky)] <- 0

    kx <- rowSums(A)
    Nx <- 1/(matrix(kx, nrow=n, ncol=n, byrow=TRUE)^(lambda) * 
                          matrix(kx, nrow=n, ncol=n, byrow=FALSE)^(1-lambda))
    Nx[is.infinite(Nx) | is.na(Nx)] <- 0 
    kx[is.infinite(kx) | is.na(kx)] <- 0 

    W <- t(A %*% Ky)
    W <- A %*% W
    W <- Nx * W
    rownames(W) <- rownames(A)
    colnames(W) <- rownames(A)
    R <- W %*% A
    return (R)
}

