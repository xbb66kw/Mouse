rm(list=ls()); # clear all variables
graphics.off() #




#library(optimx)

logistic <- function(X, beta_) {
    lterm <- X %*% beta_
    lterm[lterm < 10.0] <- exp(lterm[lterm < 10.0]) / (1+exp(lterm[lterm < 10.0]))
    lterm[lterm >= 10.0] <- 1.0
    return(lterm)
}


pri_logistic_loss <- function(X, y) {
    n <- 0
    
    if (length(dim(X)) == 0) {
        n <- length(X)
        X <- array(X, c(n, 1))        
    } else {
        n <- length(X[,1])
    }
    
    loss <- function(beta_) {
        pterm <- X %*% beta_
        pterm[pterm < 10.0] <- log(1+exp(pterm[pterm < 10.0]))
        return(c(sum(pterm/n) - (y %*% X %*% beta_)/n))
    }
    
    return(loss)
}



n <- 100
X <- array(rnorm(2 * n), c(n,2))
y <- rnorm(n) 
pri_logistic_loss(X, y)

##'pri_' prefix stands for privately used
pri_logistic_grad_hess <- function(X, y) {
    n <- 0
    
    if (length(dim(X)) == 0) {
        n <- length(X)
        X <- array(X, c(n, 1))        
    } else {
        n <- length(X[,1])
    }
    
    grad <- function(beta_) {        
        return(c(t(X) %*% (logistic(X, beta_) - y) /n))
    }
    
    hess <- function(beta_) {
        return(t(X) %*% diag(c(logistic(X, beta_)) * c((1-logistic(X, beta_)))) %*% X/n)
    }
    
    return(list('grad'=grad, 'hess'=hess))
}


yo <- pri_logistic_grad_hess(X, y)
yo$hess(c(1,2))


pri_hd_information_criterion <- function(ic, loss, k, wn, n, p) {
    val = 0
    
    if (ic == 'HQIC') {
        val = 2.0 * n * loss + 2.0 * k * wn * log(log(n)) * log(p)
    } else if (ic == 'AIC') {
        val = 2.0 * n * loss + 2.0 * k * wn * log(p)
    } else if (ic == 'BIC') {
        val = 2.0 * n * loss + k * wn * log(n) * log(p)
    }
    
    return (val)
}

pri_hd_information_criterion('BIC', 12, 7, 1, 100,1000)

pri_information_criterion <- function(ic, loss, k, n) {
    val = 0
    
    if (ic == 'HQIC') {
        val = 2.0 * n * loss + k * log(log(n))
    } else if (ic == 'AIC') {
        val = 2.0 * n * loss + 2.0 * k
    } else if (ic == 'BIC') {
        val = 2.0 * n * loss + k * log(n)
    }
    
    return (val)
}

pri_information_criterion('BIC', 12, 7, 100)


pri_minimize <- function(loss, x0, method='BFGS', jac, hess, tol) {
    #print(method)
    #print(x0)
    #print(hess)
    #print(solve(hess(x0)))
    #print(x0)
    #obj1 <- optimx(par=x0, fn=loss, gr=jac, hess=hess,   control=list(all.methods=TRUE, save.failures=TRUE, trace=0))
    obj <- optim(par=x0, fn=loss, method="BFGS", gr=jac)
    #obj <- optimx(par=x0, gr=jac, hess=hess, loss, method=method) #control=list(factr=0.0001))gr=jac, hess=hess
    #print(summary(obj))
    #print(obj)
    #print('mini----')
    return (list('coef_'=obj$par, 'value'=obj$value))
}
#debug(optimx)
n <- 1000
X <- array(rnorm(2 * n), c(n,2))
y <- rnorm(n) 
#sample(c(0,1),n, replace=TRUE)


loss_ <- pri_logistic_loss(X, y)

list_functions <- pri_logistic_grad_hess(X, y)
gr <- list_functions$grad
hes <- list_functions$hess

gr(c(1,2))
hes(c(1,2))

pri_minimize(loss_, c(1,2), jac=gr, hess=hes, tol=0.001)




chebyshev_greedy_algorithm_path <- function(X, y, ic='AIC', wn=1.0, fit_intercept=TRUE, kn=3.0, tol=1e-3) {
    n <- 0
    
    if (length(dim(X)) == 0) {
        n <- length(X)
        X <- array(X, c(n, 1))        
    } else {
        n <- length(X[,1])
    }
    
    #now X is a matrix
    if (fit_intercept) {
        X <- cbind(rep(1, n), X)
    }
    
    p <- dim(X)[2]
    
    
    iter_cga <- ceiling(kn * sqrt(n/log(p))) + as.numeric(fit_intercept)
    #iter_cga <- min(iter_cga, p)
    
    
    beta_cga <- array(rep(0, p * iter_cga), c(p, iter_cga)) 
    path_cga <- rep(0, iter_cga)
    hdic_cga <- rep(0, iter_cga)
    loss_path_cga <- rep(0, iter_cga)
    loss_grad <- pri_logistic_grad_hess(X, y)$grad

    
    if (fit_intercept) {
        path_cga[1] = 1
    } else {
        path_cga[1] = which(abs(loss_grad(beta_cga[,1])) == max(abs(loss_grad(beta_cga[,1]))))[1]
    }
    
    loss_cga <- pri_logistic_loss(X[,path_cga[1]], y)
    loss_grad_hess <- pri_logistic_grad_hess(X[,path_cga[1]], y)
    
    
    x0 <- 0.0
    
    obj <- pri_minimize(loss_cga, x0, jac=loss_grad_hess$grad, hess=loss_grad_hess$hess, tol=tol)
    #print(obj)
    beta_cga[path_cga[1], 1] <- obj$coef_
    loss_path_cga[1] <- obj$value
    
    
    hdic_cga[1] <- pri_hd_information_criterion(ic, obj$value, 1, wn, n, p)


    #The other rounds
    for (k in 2:iter_cga) {
        loss_grad_abs <- abs(loss_grad(beta_cga[,k-1]))
        loss_grad_abs[path_cga[1:(k-1)]] <- -1
        
        path_cga[k] <- which(loss_grad_abs == max(loss_grad_abs))[1]

        
        loss_cga <- pri_logistic_loss(X[,path_cga[1:k]], y)
        loss_grad_hess <- pri_logistic_grad_hess(X[,path_cga[1:k]], y)
        
        x0 <- beta_cga[path_cga[1:k], k-1]#k-dimensional
        
        obj <- pri_minimize(loss_cga, x0, jac=loss_grad_hess$grad, hess=loss_grad_hess$hess, tol=tol)

        #print(obj)
        
        beta_cga[path_cga[1:k], k] <- obj$coef_#k-dimensional
        loss_path_cga[k] <- obj$value
        
        hdic_cga[k] <- pri_hd_information_criterion(ic, obj$value, k, wn, n, p)
        
    }
    
    return(list('beta_cga'=beta_cga, 'path_cga'=path_cga, 'hdic_cga'=hdic_cga, 'iter_cga'=iter_cga, 'loss_path_cga'=loss_path_cga))    
}


n <- 1000
X <- array(rnorm(2 * n), c(n,2))
y <- sample(c(0,1), n, replace=TRUE) 
 
chebyshev_greedy_algorithm_path(X, y)#, fit_intercept= FALSE)

#debug(chebyshev_greedy_algorithm_path)
    
pri_cga_hdic_trim <- function(X, y, ic, wn, fit_intercept, kn, tol, trimming) {
    n <- 0
    
    if (length(dim(X)) == 0) {
        n <- length(X)
        X <- array(X, c(n, 1))        
    } else {
        n <- length(X[,1])
    }
    
    #now X is a matrix
    
    
    p <- dim(X)[2]
    beta_hat <- 0
    
    if(fit_intercept) { 
        beta_hat <- rep(0, p+1)
    } else {
        beta_hat <- rep(0, p)
    }
    
    obj <- chebyshev_greedy_algorithm_path(X, y, ic=ic,  wn=wn, fit_intercept=fit_intercept, kn=kn, tol=tol)
    
    if (fit_intercept) {
        X <- cbind(rep(1, n), X)
    }

    beta_cga <- obj$beta_cga
    path_cga <- obj$path_cga
    hdic_cga <- obj$hdic_cga
    iter_cga <- obj$iter_cga
    loss_path_cga <- obj$loss_path_cga
    

    
    if(fit_intercept) {
        X <- cbind(rep(1,n), X)
    }
    
    k_hdic <- which(hdic_cga == min(hdic_cga))
  
    model <- path_cga[1:k_hdic]
    
    model_trim_list <- c()
    model_size <- length(model)
    
    if(model_size > 1 & trimming) {
        for(k in (as.integer(fit_intercept)+1):k_hdic) {
            model_trim_k <- model[-k]
            
            loss_cga <- pri_logistic_loss(X[,model_trim_k], y)
            loss_grad_hess <- pri_logistic_grad_hess(X[,model_trim_k], y)
            
            
            x0 <- beta_cga[model_trim_k, k_hdic]###working
            
            obj <- pri_minimize(loss_cga, x0, jac=loss_grad_hess$grad, hess=loss_grad_hess$hess, tol=tol)
            
            hdic_trim <- pri_hd_information_criterion(ic, obj$value, k_hdic, wn, n, p)
            
            if(hdic_trim < hdic_cga[k_hdic]) {
                model_size <- model_size - 1
                model_trim_list <- c(model_trim_list, k)
            }       
        }
    }
    
    if (!is.null(model_trim_list)) {
        model_trim <- model[-model_trim_list]
    } else {
        model_trim <- model
    }

    #print(hdic_cga)
    #print(model_trim_list)
    #print(model_trim)
    #print('-------305')
    loss <- 0
    if (model_size < length(model)) {
        loss_cga <- pro_logistic_loss(X[,model_trim], y)
        loss_grad_hess <- pri_logistic_grad_hess(X[,model_trim], y)
        x0 <- beta_cga[model_trim, k_hdic]
        
        obj <- pri_minimize(loss_cga, x0, jac=loss_grad_hess$grad, hess=loss_grad_hess$hess, tol=tol)
        
        beta_hat[model_trim] <- obj$coef_
        loss <- obj$value
    } else {
        beta_hat[model] <- beta_cga[model, k_hdic]
        loss <- loss_path_cga[k_hdic]
    }
    
    intercept <- 0
    intercept_cga <- 0
    coef_cga <- 0
    coef_ <- 0
    if(fit_intercept) {
        intercept <- beta_hat[1]
        intercept_cga <- beta_cga[1,]
        coef_ <- beta_hat[-1]
        coef_cga <- beta_cga[-1,]
        hdic_cga <- hdic_cga[-1]
        path_cga <- path_cga[-1] - 1
        model_trim <- model_trim[-1] - 1
        iter_cga <- iter_cga - 1
    } else {
        intercept <- rep(0)
        intercept_cga <- rep(0, iter_cga)
        coef_cga <- beta_cga
        coef_ <- beta_hat
    }

    coef_ <- coef_[model_trim]
    return(list('intercept'=intercept, 'coef_'=coef_, 'model_trim'=model_trim, 'loss'=loss, 'path_cga'=path_cga, 'intercept_cga'=intercept_cga, 'coef_cga'=coef_cga, 'hdic_cga'=hdic_cga, 'iter_cga'=iter_cga))
}
n <- 100
X <- array(rnorm(2 * n), c(n,2))
y <- rnorm(n) 
sample(c(0,1), n, replace=TRUE) 



pri_cga_hdic_trim(X, y, ic='HQIC', 1, TRUE, 3, 0.001, TRUE)

Logistic.fit <- function(X, y, fit_intercept=TRUE) {
    
    return(pri_cga_hdic_trim(X, y, ic='AIC', wn=1.0, fit_intercept=fit_intercept, kn=3.0, tol=0.001, trimming=TRUE))
    
}


n <- 100
b <- 50
X <- array(rbinom(n * b, 1, 0.5), c(n,b))
X <- array(rnorm(n * b), c(n,b))
#y <- sample(c(0,1), n, replace=TRUE)
#X <- cbind(1, X)
beta_ <- c(-7.7, 7.8)#, 7.0)
p <- 1 / (1 + exp(-X[,2:3] %*% beta_))

y <- rbinom(n, 1, p)
#print(y)
result <- Logistic.fit(X, y)
print(result$coef_)
print(result$model_trim)
print(result$path_cga)

H <- pri_logistic_grad_hess(X[,1:2], y)$hess
G <- pri_logistic_grad_hess(X[,1:2], y)$gr
print(X[,1:2])
print(y)
H(beta_)
G(beta_)
beta_


X <- tunning_set_larger[!is.na(rowSums(tunning_set_larger)),]
y <- y_tunning_larger[!is.na(rowSums(tunning_set_larger))]
X[X != 0] <- 1
result3 <- Logistic.fit(X, y)

X1 <- tunning_set_larger[!is.na(rowSums(tunning_set_larger)),][1:40,]
y1 <- y_tunning_larger[!is.na(rowSums(tunning_set_larger))][1:40]
X1[X1 != 0] <- 1
result1 <- Logistic.fit(X1, y1)

X2 <- tunning_set_larger[!is.na(rowSums(tunning_set_larger)),][41:80,]
y2 <- y_tunning_larger[!is.na(rowSums(tunning_set_larger))][41:80]
X2[X2 != 0] <- 1
result2 <- Logistic.fit(X2, y2)


##Solution:


X_predict <- prediction_set
X_predict[X_predict != 0] <- 1
beta_ <- c(result3$intercept, result3$coef_)
reref <- rep(NA, length(prediction_set[,1]))
for (t_ in 1:length(prediction_set[,1])) {
    reref[t_] <- logistic(c(1, X_predict[t_,result3$model]), beta_) >= 0.5
}
sum(reref - y_predict == 1 | reref - y_predict == -1, na.rm=TRUE) / sum(!is.na(reref))





X_predict <- prediction_set[!is.na(rowSums(prediction_set)),]
y <- y_predict[!is.na(rowSums(prediction_set))]
X_predict[X_predict != 0] <- 1

fit_lasso_qr <- rq.fit.lasso(cbind(1, X_predict), y, tau = 0.10, lambda = 1, beta = .9995, eps = 1e-06)
fdfdf <- rep(NA, length(X_predict[,1]))
for (t_ in 1:length(X_predict[,1])) {
    fdfdf[t_] <- logistic(c(1, X_predict[t_,]), fit_lasso_qr$coeff) >= 0.5
}


fitter <- rep(4, length(prediction_set[,1]))
fitter[!is.na(rowSums(prediction_set))] <- fdfdf

reref[fitter == 1] <- 1


sum(reref - y_predict == 1 | reref - y_predict == -1, na.rm=TRUE) / sum(!is.na(reref))
#reref[rowSums(false_predictor) <= length(false_prediction_neurons) * para_negative] <- 0#######
#reref[prediction_aux_information] <- 1
#sum(reref - y_predict == 1 | reref - y_predict == -1, na.rm=TRUE) / sum(!is.na(reref))

