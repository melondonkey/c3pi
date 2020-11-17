if (!require(xgboost)) install.packages('xgboost')

library(xgboost)
library(dplyr)

# Filter unmatchable records like responses from Puerto Rico
data <- surveys_sdoh %>%
  filter(!is.na(fips_code)) %>%
  filter(!(fips_code %in% c('72013','72113', '72127')))



# Test if adding features improves predictive performance, starting with a baseline of basic demographics

#The basics
X0 <- model.matrix(~  age + gender + ethnicity + education + religion - 1, 
                       data = data)

#Basic + most variation in RWJF
X1 <- model.matrix(~  age + gender + ethnicity + education + religion - 1 + Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5, 
                   data = data)

#Basics + Political beliefs
X2 <- model.matrix(~  age + gender + ethnicity + education + religion + religiosity + politicalBelief + politicalParty  +
                     trumpApproval- 1, 
                   data = data)

#All of the above
X3 <- model.matrix(~  age + gender + ethnicity + education + religion + 
                           religiosity + politicalBelief + politicalParty  + trumpApproval +
                           Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 - 1, 
                         data = data)

#Kitchen sink
X4 <- model.matrix( ~ - 1 +
                      age + gender + ethnicity + education + religion + 
                      religiosity + politicalBelief + politicalParty  + trumpApproval +
                      v001_rawvalue + #Premature death raw value
                      v002_rawvalue + #Poor or fair health raw value
                      v003_rawvalue + #Uninsured adults raw value
                      v004_rawvalue + #Primary care physicians raw value
                      v005_rawvalue + #Preventable hospital stays raw value
                      v007_rawvalue + #Diabetes monitoring raw value
                      v009_rawvalue + #Adult smoking raw value
                      v011_rawvalue + #Adult obesity raw value
                      v014_rawvalue + #Teen births raw value
                      v021_rawvalue + #High school graduation raw value
                      v023_rawvalue + #Unemployment raw value
                      v024_rawvalue + #Children in poverty raw value
                      v036_rawvalue + #Poor physical health days raw value
                      v037_rawvalue + #Low birthweight raw value
                      v039_rawvalue + #Motor vehicle crash deaths raw value
                      v042_rawvalue + #Poor mental health days raw value
                      v043_rawvalue + #Violent crime raw value
                      v044_rawvalue + #Income inequality raw value
                      v045_rawvalue + #Sexually transmitted infections raw value
                      v049_rawvalue + #Excessive drinking raw value
                      v050_rawvalue + #Mammography screening raw value
                      v051_rawvalue + #Population raw value
                      v052_rawvalue + #% below 18 years of age raw value
                      v053_rawvalue + #% 65 and older raw value
                      v054_rawvalue + #% Non-Hispanic African American raw value
                      v055_rawvalue + #% American Indian and Alaskan Native raw value
                      v056_rawvalue + #% Hispanic raw value
                      v057_rawvalue + #% Females raw value
                      v058_rawvalue + #% Rural raw value
                      v059_rawvalue + #% not proficient in English raw value
                      v060_rawvalue + #Diabetes  + # raw value
                      v062_rawvalue + #Mental health providers raw value
                      v063_rawvalue + #Median household income raw value
                      v065_rawvalue + #Children eligible for free or reduced price lunch raw value
                      v067_rawvalue + #Driving alone to work raw value
                      v069_rawvalue + #Some college raw value
                      v070_rawvalue + #Physical inactivity raw value
                      v080_rawvalue + #% Native Hawaiian/Other Pacific Islander raw value
                      v081_rawvalue + #% Asian raw value
                      v082_rawvalue + #Children in single-parent households raw value
                      v083_rawvalue + #Limited access to healthy foods raw value
                      v085_rawvalue + #Uninsured raw value
                      v086_rawvalue + #Health care costs raw value
                      v088_rawvalue + #Dentists raw value
                      v122_rawvalue + #Uninsured children raw value
                      v124_rawvalue + #Drinking water violations raw value
                      v125_rawvalue + #Air pollution - particulate matter raw value
                      v126_rawvalue + #% Non-Hispanic white raw value
                      v127_rawvalue + #Premature age-adjusted mortality raw value
                      v131_rawvalue + #Other primary care providers raw value
                      v132_rawvalue + #Access to exercise opportunities raw value
                      v133_rawvalue + #Food environment index raw value
                      v134_rawvalue + #Alcohol-impaired driving deaths raw value
                      v135_rawvalue + #Injury deaths raw value
                      v136_rawvalue + #Severe housing problems raw value
                      v137_rawvalue + #Long commute - driving alone raw value
                      v139_rawvalue + #Food insecurity raw value
                      v140_rawvalue + #Social associations raw value
                      v142_rawvalue + #Residential segregation - non-white/white raw value
                      v143_rawvalue + #Insufficient sleep raw value
                      v144_rawvalue + #Frequent physical distress raw value
                      v145_rawvalue + #Frequent mental distress raw value
                      v146_rawvalue , #Drug overdose deaths - modeled raw value
                      
##Location data attributes
 #                     hospitalIcuBeds +
#  hospitalStaffedBeds +
#  latestUnemploymentRate,
                    data=data
                      )

#stuff we can match to census data
X5 <- model.matrix(~  age + gender + ethnicity + education  - 1, 
                   data = data)


 # Record AUC of each variable set - CV test
## Drivers of supsicious #1B998B
# X0 = .712
# X1 = .709
# X2 = .734
# X4 = .726

# Drivers of scientific #C70039
#X0 = .772
#X1 = .771
#X2 = .807
#X4 =  .800
#X5 = .763

# Drivers of skeptic #009FFD

#X4 = .643

y <- data$skeptical

X <- X4


xgb1 <-
  xgb.cv(
    nround = 100,
    nfold=10,
    params = list(objective='binary:logistic', max_depth=4),
    data = X,
    label = y,
    metrics='auc'
  )

best_iter <- max(xgb1$evaluation_log$test_auc_mean)
best_round <- which(xgb1$evaluation_log$test_auc_mean == best_iter)

xgb_model <- 
  xgboost(
    nrounds = best_round,
    params = list(objective='binary:logistic', max_depth=4),
    data = X,
    label = y
  )

importance <- xgb.importance(feature_names = colnames(X), model = xgb_model) 
#View(importance)

#Plot for writeup.  Manually changed the dataset
importance %>%
  arrange(-Gain) %>%
  mutate(
    rank = row_number()
  ) %>%
  filter(rank <=20) %>%
  ggplot( aes(x=reorder(Feature, Gain), y=Gain)) + geom_point(size=2, color='#009FFD') + coord_flip() +
  xlab("Feature") + ylab("Gain (Feature Importance)") +
  theme_minimal(base_size=14) + 
  ggtitle('Top 20 Drivers of Skeptical Profile')



