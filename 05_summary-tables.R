## Summary tables

library(tidyr)
library(dplyr)
library(ggplot2)

se_prop <- function(x){
  p = mean(x)
  q = 1 - p
  n = length(x)
  sqrt(p*q/n)
}

se <- function(x){
  sd(x, na.rm=TRUE)/sqrt(sum(!is.na(x)))
}

summary_table <- 
  surveys_sdoh %>%
  mutate(
    white = ifelse(ethnicity=='white',1,0),
    male = ifelse(gender=='male',1,0)
  ) %>%
  group_by(cluster) %>%
  summarize(
    avg_age = mean(age),
    se_age = se(age),
    avg_white = mean(white),
    se_white = se_prop(white),
    avg_male = mean(male),
    se_male = se_prop(male),
    avg_trumpApproval = mean(trumpApproval),
    se_trumpApproval = se(trumpApproval),
    avg_political = mean(politicalParty),
    se_political = se(politicalParty),
    avg_religiosity = mean(religiosity),
    se_religiosity = se(religiosity),
    avg_mask = mean(coronavirusIntent_Mask, na.rm=TRUE),
    se_mask = se(coronavirusIntent_Mask),
    avg_socialdist = mean(coronavirusIntent_SixFeet, na.rm=TRUE),
    se_socialdist = se(coronavirusIntent_SixFeet)
  )

summary2 <- 
summary_table %>%
  pivot_longer(cols = -one_of('cluster'),
               names_to = c('metric', 'variable'),
               names_sep = "_") 

summary3 <-
  summary2 %>%
  pivot_wider(id_cols = c('cluster', 'variable'),
              names_from= c('metric')) %>%
  mutate(
    lower = avg - se,
    upper = avg + se
  )


myvar <- 'socialdist'
summary3 %>%
  filter(variable==myvar) %>%
  ggplot(aes(x=cluster, y=avg, color=cluster)) + geom_point(size=3) +
  geom_pointrange(aes(ymin=lower, ymax=upper), size=2) +
  coord_flip() +
  scale_color_manual(values = c('#C70039','#009FFD','#1B998B')) +
  theme_minimal(base_size=15)  +
  theme(legend.position = 'none') +
  xlab('') + 
  ylab('Average Intent to Maintain 6 Feet Distance')

ggsave(here::here('images', paste0(myvar, '.png')), width=7, height=2)
  

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
