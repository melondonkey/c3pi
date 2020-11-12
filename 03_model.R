
library(brms)

option(mc.cores=4)
options(buildtools.check = function(action) TRUE)

unique(surveys$cluster)

surveys <-
  surveys %>%
  mutate(
    suspicious = ifelse(cluster=="Suspicious", 1, 0),
    age = 2020 - birthYear2020 
  )

mean(surveys$suspicious)


mod1 <- brm(suspicious ~ s(age) + (1|education) + (1|ethnicity) + (1|gender)  ,
            family="bernoulli",
            data=surveys)