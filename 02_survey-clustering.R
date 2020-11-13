#install.packages('pheatmap')
source(here::here('functions', 'bernoulli-mixture-funs.R'))
Rcpp::sourceCpp(here::here('functions', 'bernLikeC.cpp'))

library(dplyr)

#Extract binary vars from survey results
X <- surveys[,c("coronaSimilarFlu", "coronaOnlyElderly","youngInvulnerable","elderlyMoreRisk","coronaAllHospitalize","coronaKillsMost","ethnicitySpreadsCovid","allSpreadCovid",
                "nonNativesSpreadCovid","asymptomaticSpread","onlySickSpread","infectFromAnimal")]

#Convert to numeric
X2 <- as.matrix(X*1)



set.seed(345)

##3-4 best
bmix <- bernoulli_mixture(X2, 3, EM_steps = 100)
bmix$convergence

plot(bmix, main="Clusters")

#Cluster 1 = suspicious
#Cluster 2 = 
#Cluster 3 = scientific


#Save the cluster profiles
pheatmap::pheatmap(t(bmix$parameters),
                   cluster_rows = FALSE,
                   cluster_cols = FALSE,
                   col = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,"Blues"))(25), 
                   labels_col = paste0(round(bmix$mixture_distribution,2)*100,'%'),
                   angle_col = 0,
                   main = 'Cluster Profiles'
                   
)

png(filename = here::here('images', 'cluster-summaries.png'))
dev.off()

##Add back to the data
surveys$belief_clusterid <- bmix$cluster_assignments

surveys <- 
  surveys %>%
  mutate(
    cluster = case_when(
      belief_clusterid == 1 ~ 'Sick Only',
      belief_clusterid == 2 ~ 'Suspicious',
      belief_clusterid == 3 ~ 'Scientific',
      TRUE ~ 'OTHER'
      
    )
  )

#cleanup
rm(X, X2, bmix)
