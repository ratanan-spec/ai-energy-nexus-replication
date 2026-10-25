run_discount_tvp <- function(Y, dates, lambda = PROJECT$lambda, kappa = PROJECT$kappa,
                             H = PROJECT$fevd_horizon, init_window = PROJECT$init_window,
                             bound = PROJECT$stability_bound) {
  Y <- as.matrix(Y); K <- ncol(Y); node_names <- colnames(Y)
  X0 <- cbind(1, Y[1:(init_window-1), , drop=FALSE]); Y0 <- Y[2:init_window, , drop=FALSE]
  B <- qr.solve(X0, Y0); E0 <- Y0-X0%*%B; Sigma <- crossprod(E0)/nrow(E0)
  P <- lapply(seq_len(K), function(i) solve(crossprod(X0)+diag(1e-8,ncol(X0)))*Sigma[i,i])
  gf <- function(A,S) {
    Phi <- vector("list",H); Phi[[1]] <- diag(K)
    if(H>1) for(h in 2:H) Phi[[h]] <- Phi[[h-1]]%*%A
    th <- matrix(0,K,K)
    for(i in 1:K) for(j in 1:K) {
      num <- sum(vapply(Phi,function(ph)(ph[i,,drop=FALSE]%*%S[,j,drop=FALSE])^2,numeric(1)))/S[j,j]
      den <- sum(vapply(Phi,function(ph)(ph[i,,drop=FALSE]%*%S%*%t(ph[i,,drop=FALSE])),numeric(1)))
      th[i,j] <- num/den
    }
    th/rowSums(th)
  }
  ne <- nrow(Y)-init_window; arr <- array(NA_real_,c(ne,K,K),dimnames=list(NULL,node_names,node_names))
  tci <- numeric(ne); net <- matrix(NA_real_,ne,K,dimnames=list(NULL,node_names)); resc <- logical(ne)
  for(s in 1:ne) {
    t <- init_window+s; x <- c(1,Y[t-1,]); e <- Y[t,]-drop(x%*%B)
    for(i in 1:K) {pp<-P[[i]]/lambda; g<-drop(pp%*%x)/as.numeric(1+t(x)%*%pp%*%x);
      B[,i]<-B[,i]+g*e[i]; P[[i]]<-pp-tcrossprod(g,x)%*%pp}
    Sigma<-kappa*Sigma+(1-kappa)*tcrossprod(e); Sigma<-(Sigma+t(Sigma))/2+diag(1e-10,K)
    A<-t(B[-1,,drop=FALSE]); rho<-max(Mod(eigen(A,only.values=TRUE)$values))
    if(rho>=bound){A<-A*(bound/rho);resc[s]<-TRUE}
    th<-gf(A,Sigma);arr[s,,]<-th;off<-th;diag(off)<-0
    tci[s]<-100*sum(off)/K;net[s,]<-100*(colSums(off)-rowSums(off))
  }
  list(Date=as.Date(dates[(init_window+1):nrow(Y)]),TCI=tci,NET=net,theta=arr,
       average_theta=apply(arr,c(2,3),mean),rescaled=sum(resc),lambda=lambda,H=H)
}
