library(httr)
library(jsonlite)
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
        filter= 'contains(id, "_UnitedStates" )'
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



#readr::write_csv(rover, here::here('survey_data.csv'))