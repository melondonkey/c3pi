
library(brms)
library(lme4)

options(mc.cores=4)
options(buildtools.check = function(action) TRUE)

unique(surveys$cluster)

surveys <-
  surveys %>%
  mutate(
    suspicious = ifelse(cluster=="Suspicious", 1, 0),
    age = 2020 - birthYear2020 
  )

mean(surveys$suspicious)


mod1 <- brm(suspicious ~ s(age) + (1|education) + (1|ethnicity) + (1|gender) + (1|religion) + s(religiosity) ,
            family="bernoulli",
            data=surveys)

mod2 <- glmer(suspicious ~  (1|education) + (1|ethnicity) + (1|gender) + (1|religion) + (1|ethnicity:education),
              data=surveys,
              family="binomial"
              )


summary(mod2)

ranef(mod2)


surveys %>%
  group_by(cluster) %>%
  summarize(
    avg = mean(coronavirusIntent_Mask, na.rm=TRUE)
  )