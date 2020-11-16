if (!require(extraDistr)) install.packages('extraDistr')
if (!require(here)) install.packages('here')
if (!require(httr)) install.packages('httr')
if (!require(jsonlite)) install.packages('jsonlite')
if (!require(dplyr)) install.packages('dplyr')
if (!require(readr)) install.packages('readr')
if (!require(tidyverse)) install.packages('tidyverse')

library(httr)
library(jsonlite)
library(here)
library(extraDistr)
source(here::here('example-files', 'c3aidatalake.R'))
library(dplyr)
library(readr)

##Pull survey data
surveys <- 
  fetch(
  "surveydata",
  list(
    spec = list(
      limit = -1
    )
  ),
  get_all = TRUE
)


#surveys <-
#  surveys %>%
#  mutate(
#    census_race = case_when(
#      ethnicity == "white" ~ "White alone",
#      ethnicity == "black" ~ "Black or African American alone",
#      ethnicity == "asian" ~ "Asian alone",
#      ethnicity == "hispanic-latino"
#    )
#  )



#Census data
#census <- 
#  fetch(
#    "populationdata",
#    list(
#      spec = list(
#        limit = 100,
#        filter= 'contains(id, "_UnitedStates" ) && origin == "United States Census"'
#      )
#    ),
#    get_all = FALSE
#  )

#Pull all the counties
locations <- 
  fetch(
    "outbreaklocation",
    list(
      spec = list(
        limit = -1,
        filter= 'contains(id, "_UnitedStates" ) && locationType == "county"' ,
        include = 'this, LaborDetail.laborForce'
      )
    ),
    get_all = TRUE
  )


### RWJF Pull and cleanse ###

# Pull Robert Wood Johnson Foundation county health rankings
rwjf_link <- "https://www.countyhealthrankings.org/sites/default/files/analytic_data2018_0.csv" 

rwjf <- read_csv(url(rwjf_link), skip = 1)

rwjf_reference <- read_csv(url(rwjf_link), n_max = 1)
rwjf_reference <- data.frame(t(rwjf_reference))
rwjf_reference$description <- rownames(rwjf_reference)
colnames(rwjf_reference)[1] <- "variable"

# Get down to the raw metrics
rwjf2 <-
  rwjf %>%
  select(!contains(c('_cilow', 'cihigh', 'numerator', 'denominator', 'other_data'))) %>%
  filter(state != 'US') %>%
  filter(countycode != '000')

# Select columns with less than 20% missing
rwjf3 <- rwjf2[,apply(is.na(rwjf2), 2, mean) < .2]


#Impute missing values with the column mean and create matrix format
rwjf_M <- as.matrix(rwjf3[,8:dim(rwjf3)[2]])
colmeans <- apply(rwjf_M, 2, mean, na.rm=TRUE)
missing_elements <- which(is.na(rwjf_M), arr.ind = FALSE)
rwjf_M[missing_elements] <- as.vector(colmeans[which(is.na(rwjf_M), arr.ind = TRUE)[,2]])

rwjf_df <- rwjf3

rwjf_reference <-
  rwjf_reference %>%
  filter(variable %in% colnames(rwjf_df))

#Cleanup
rm(rwjf, rwjf2, rwjf3)



### ZIP to FIPS from HUD https://www.huduser.gov/portal/datasets/usps_crosswalk.html 
zip_fips <- read_csv('ZIP_COUNTY_092020.csv')

# Select most highest residential pct for match
zip_fips2 <- 
  zip_fips %>% 
  group_by(ZIP) %>%
  arrange(ZIP, -RES_RATIO) %>%
  mutate(
    rownum = row_number(),
    zip3 = as.numeric(substr(ZIP,1,3)), #numeric to match zip column in surveys
    fips_code = as.character(COUNTY)
  ) %>%
  filter(rownum == 1) %>%
  ungroup()


locations2 <- locations[locations$fips != "NULL",] #Remove invalid FIPS
locations2$fips2 <- unlist(locations2$fips) #unlist the fips column
locations2$fips_code <- as.character(formatC(as.numeric(locations2$fips2), digits=5, width=5, flag="0")) #formatting

fips_to_zip3 <- 
  locations2 %>%
  select(-one_of('location', 'fips')) %>%
  #  select(fips_code, latestTotalPopulation) %>%
  left_join(zip_fips2 %>% select(zip3, fips_code) %>% distinct(), by=c("fips_code")) %>%
  group_by(zip3) %>%
  arrange(zip3, -latestTotalPopulation) %>%
  mutate(
    rownum = row_number()
  ) %>%
  filter(rownum==1)

rm(locations2, zip_fips, zip_fips2)


