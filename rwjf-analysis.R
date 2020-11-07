library(tsne)
#install.packages('factoextra')
#library(factoextra)

t1 <- tsne(rwjf_M)


rwjf_std <- scale(rwjf_M)


p1 <- princomp(rwjf_std)

screeplot(p1)



plot(x=t1[,1], y=t1[,2])

pca_data <- p1$scores

combined <- cbind.data.frame(rwjf3[,c(1:7)], rwjf_M, pca_data)

View(cor(combined[,-c(1:7)]))

## What did we learn? 
# 1st principal component .91 correlation with Poor or Fair Health and Frequent Physical distress - neg wealth and ed
# 2nd principal component Rural/urban divide (high correlation with % rural and low with housing problems)
# 3rd pc - uninsured children, uninsured, uninsured adults, hispanic, food access; negative cor with air pollution and insufficient sleep
# 4th pc - 