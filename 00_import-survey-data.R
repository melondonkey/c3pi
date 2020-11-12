library(httr)
library(jsonlite)
library(here)
library(extraDistr)
source(here::here('example-files', 'c3aidatalake.R'))
library(dplyr)

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


surveys <-
  surveys %>%
  mutate(
    census_race = case_when(
      ethnicity == "white" ~ "White alone",
      ethnicity == "black" ~ "Black or African American alone",
      ethnicity == "asian" ~ "Asian alone",
      ethnicity == "hispanic-latino"
    )
  )



#Census data -- too big
census <- 
  fetch(
    "populationdata",
    list(
      spec = list(
        limit = 100,
        filter= 'contains(id, "_UnitedStates" ) && origin == "United States Census"'
      )
    ),
    get_all = FALSE
  )

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

#Cleanup
rm(rwjf, rwjf2, rwjf3)


