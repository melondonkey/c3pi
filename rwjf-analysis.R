library(tsne)
#install.packages('factoextra')
#library(factoextra)

#t1 <- tsne(rwjf_M)


rwjf_std <- (rwjf_M - apply(rwjf_M, 2, mean))/apply(rwjf_M, 2, sd) 


p1 <- princomp(rwjf_std)

plot(p1$sdev)

cor(rwjf_M)