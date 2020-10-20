# Bernmix stuff
#require(extraDistr)
#require(pheatmap)

compute_expectation <- function(X, p){
  apply(apply(X, 1, extraDistr::dbern, p=p, log=TRUE), 2, sum)
}


log_normalize <- function(x){
  x = x - min(x)
  x = exp(x)
  
  x/sum(x)
}


weighted_mean <- function(x, w){
  sum(x*w)/sum(w)
}


bic <- function(LL, n_params, N, P, k){
  log(N)*(k*P) - 2*LL
}


initialize_parameters <- function(X, k){
  P <- dim(X)[2]
  
  matrix(stats::runif(k*P), nrow=k, ncol=P)
}


bernoulli_mixture <- function(X, k, EM_steps = 25){
  
  P<- dim(X)[2]
  N <- dim(X)[1]
  
  params <- initialize_parameters(X, k)
  colnames(params) <- colnames(X)
  
  cluster_assignments <- matrix(nrow=N, ncol=k)
  likelihoods <- matrix(nrow=N, ncol=k)
  
  LL <- c()
  
  for(step in 1:EM_steps){
    #for(i in 1:k){
    #  likelihoods[,i] <- compute_expectation(X, p=params[i,])
    #}
    
    likelihoods <- bernLikeC(X, params)
    
    cluster_assignments <- t(apply(likelihoods, 1, log_normalize))
    cluster_assignments <- pmin(cluster_assignments, .9999)
    cluster_assignments <- pmax(cluster_assignments, .0001)
    
    
    #Maximization
    for(i in 1:k){
      params[i,] <- apply(X, 2, weighted_mean, w=cluster_assignments[,i])
    }
    
    LL[step] <- sum(likelihoods*cluster_assignments)
  }
  
  
  
  bm <- list(
    parameters = params,
    cluster_probabilities = cluster_assignments,
    cluster_assignments = apply(cluster_assignments, 1, which.max),
    mixture_distribution = apply(cluster_assignments, 2, sum)/sum(cluster_assignments),
    BIC = bic(LL[step], n_params, N, P, k),
    LL = LL[step],
    convergence = LL,
    k = k
  )
  
  class(bm) <- "BernoulliMixture"
  
  return(bm)
}


plot.BernoulliMixture <- function(x){
  params <- x$parameters
  pheatmap::pheatmap(t(params),
                     cluster_rows = FALSE,
                     cluster_cols = FALSE,
                     col = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"))(25), 
                     labels_col = paste0(round(x$mixture_distribution,2)*100,'%'),
                     angle_col = 0
  )
}
  
X <- surveys[,c("coronaSimilarFlu", "coronaOnlyElderly","youngInvulnerable","elderlyMoreRisk","coronaAllHospitalize","coronaKillsMost","ethnicitySpreadsCovid","allSpreadCovid",
                "nonNativesSpreadCovid","asymptomaticSpread","onlySickSpread","infectFromAnimal")]

X2 <- as.matrix(X*1)



bmix <- bernoulli_mixture(X2, 6, EM_steps = 30)
bmix$convergence

plot(bmix)

bmix$convergence
  