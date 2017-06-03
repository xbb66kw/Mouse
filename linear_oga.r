library(MASS)

OHT <- function(y, X, Kn) {
    n <- nrow(X)
    p <- ncol(X)
    y <- y - mean(y)
    
    for (i in 1:p) {
        X[,i] <- X[,i] - mean(X[,i])
    }
    jhat <- ehat1 <- integer(Kn)
    Xbase <- array(0,c(n,Kn))
    u <- y
    xnorms <- sqrt(colSums(X^2))
    for (k in 1:Kn) {
        
        SSE <- colSums(abs(t(u) %*% X)) / xnorms #colSums changes matrix with one row to a vector.

        if (k > 1) { 
            SSE[jhat[1:(k-1)]] <- 0
        } #Numerically not zero, manually set it to zero.       
        jhat[k] <- which(abs(SSE)==max(abs(SSE)))
        if(k==1) { 
            qk <- X[,jhat[1]] / sqrt( sum(t(X[,jhat[1]])%*%X[,jhat[1]]) )
            Xbase[,1] <- qk
        } else { 
            rq <- X[,jhat[k]]- Xbase[,1:(k-1)] %*% t(Xbase[,1:(k-1)]) %*% X[,jhat[k]] 
            qk <- rq / sqrt( sum(t(rq)%*%rq) )
            Xbase[,k] <- qk
        }
        u <- u - qk %*% t(qk) %*% u

        ehat1[k] <- t(u) %*% u#For HDIC

    }
    
    # OGA + HDIC
    
    HDBIC  <- n * log(ehat1[1:Kn] / (n * ncol(y))) + ncol(y) * (1 : Kn) * log(p) * log(n)
    mHDBIC  <- min(HDBIC)
    j_hdbic <- jhat[1:which(HDBIC==mHDBIC)]
    
    # OGA + HDBIC + Trim  
    j_trim <- jremain <- j_hdbic
    for (k in 1:length(j_hdbic)) {
        j1 <- jremain[-k]
        ehat1 <- array(0, c(n, ncol(y)))
        
        for (i in 1:ncol(y)) {
            ehat1[,i] = y[,i] - mean(y[,i])
        }
        
        if (length(j1) > 0) { 
            ehat1 <- y - X[,j1] %*% ginv(t(X[,j1]) %*% X[,j1]) %*% t(X[,j1]) %*% y
        }              
        
        HDBIC_trim <-  n*log( abs(det(t(ehat1)%*%ehat1)/ncol(y))/n ) + ncol(y)*( length(j_hdbic)-1 )*log(n)*log(p)
        
        if (HDBIC_trim < mHDBIC) {
            j_trim[k] <- 0
        }
    }
    
    j_trim <- j_trim[-which(j_trim==0)]
    
    if (length(j_trim)==0) { 
        j_trim <- j_hdbic
    }
    
    #----------------------------------------------------------------------------------------------
    trim_m <- integer(p)
    trim_m[j_trim] <- 1
    #OGA_HDBIC_Trim=c( sum(trim_m[which(beta0.rowsum!=0)])  , sum(trim_m[-which(beta0.rowsum!=0)])  )
    #----------------------------------
    #return(list( "relevant selected number"= sum(trim_m[which(beta0.rowsum!=0)]) ,  "overfitting number"= sum(trim_m[which(beta0.rowsum==0)])  , jhat      ))
    return(list(jhat, trim_m))
    # relevant selected number : how many variables in relevant index are the selected model
    # overfitting number       : how many variables in irrelevant index are the selected model
}

n <- 100
p <- 200

X <- array(rnorm(p * n), c(n, p))
y <- X[,1:5] %*% c(1,1,2,3,4) + rnorm(n)

OHT(y, X, 20)