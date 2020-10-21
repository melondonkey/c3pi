library(httr)
library(jsonlite)
library(here)
library(extraDistr)
source(here::here('example-files', 'c3aidatalake.R'))


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


#Census data -- too big
#census <- 
#  fetch(
#    "populationdata",
#    list(
#      spec = list(
#        limit = -1,
#        filter= 'contains(id, "_UnitedStates" ) && origin == "United States Census" '
#      )
#    ),
#    get_all = TRUE
#  )


locations <- 
  fetch(
    "outbreaklocation",
    list(
      spec = list(
        limit = 200,
        filter= 'contains(id, "_UnitedStates" ) && locationType == "county"' #,
 #       include = 'this, LaborDetail.laborForce'
      )
    ),
    get_all = FALSE
  )

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


#Impute missing values with the column mean
rwjf_M <- as.matrix(rwjf3[,8:dim(rwjf3)[2]])
colmeans <- apply(rwjf_M, 2, mean, na.rm=TRUE)
missing_elements <- which(is.na(rwjf_M), arr.ind = FALSE)
rwjf_M[missing_elements] <- as.vector(colmeans[which(is.na(rwjf_M), arr.ind = TRUE)[,2]])


