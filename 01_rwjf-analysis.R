

#Standardize matrix for PCA
rwjf_std <- scale(rwjf_M)

p1 <- princomp(rwjf_std)


screeplot(p1)

pca_data <- p1$scores

#Create a data frame that combines the principal components and the original variables
rwjf_pc <- cbind.data.frame(rwjf_df[,c(1:7)], rwjf_M, pca_data)

#View(cor(rwjf_pc[,-c(1:7)]))

## What did we learn? 
# 1st principal component .91 correlation with Poor or Fair Health and Frequent Physical distress - neg wealth and ed
# 2nd principal component Rural/urban divide (high correlation with % rural and low with housing problems)
# 3rd pc - uninsured children, uninsured, uninsured adults, hispanic, food access; negative cor with air pollution and insufficient sleep
# 4th pc - 


#cleanup
rm(p1, rwjf_std, rwjf_M, pca_data, rwjf_df)

surveys_sdoh <-
  surveys %>%
  left_join (fips_to_zip3, by=c('zipcodePrefix'='zip3')) %>%
  left_join(rwjf_pc, by=c('fips_code'='fipscode'))

