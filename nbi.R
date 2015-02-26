
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
    
    # this is the first resource pass from X to Y nodes where the degree information is passed . Look for dimension of adjacency matrix
    W <- t(A %*% Ky)
    # Again here it returns back the resource to the X nodes
    W <- A %*% W
   
    # This is the final scaling which needs to be done you cannot do mulliplication here you are just scaling
    W <- Nx * W
    rownames(W) <- rownames(A)
    colnames(W) <- rownames(A)
    R <- W %*% A
    return (R)
}

nbi.mod <- function(A,lambda = 0.5){
    # A is the adjacencny matrix her
    n <- nrow(A)
    m <- ncol(A)
    # You need to calculate the degree of columns to use it as node weight
    Ky <- diag(1/colSums(A))
    Ky[is.infinite(Ky) | is.na(Ky)] <- 0
    
    kx <- rowSums(A)
    # use lamda for modified matrix
    Nx <- 1/(matrix(kx, nrow=n, ncol=n, byrow=TRUE)^(lambda) * 
                          matrix(kx, nrow=n, ncol=n, byrow=FALSE)^(1-lambda))
    Nx[is.infinite(Nx) | is.na(Nx)] <- 0 
    kx[is.infinite(kx) | is.na(kx)] <- 0 
    
    # this is the first resource pass from X to Y nodes where the degree information is passed . Look for dimension of adjacency matrix
    W <- t(A %*% Ky) 
    # Again here it returns back the resource to the X nodes
    W <- A %*% W 
    # This is the final scaling which needs to be done you cannot do mulliplication here you are just scaling
    W <- Nx * W 
    rownames(W) <- rownames(A)
    colnames(W) <- rownames(A)
    R <- W %*% A
    return (R)
}

