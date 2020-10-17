library(httr)

library(jsonlite)

source(here::here('example-files', 'c3aidatalake.R'))

surveys <- read_data_json('surveydata', 'fetch', 'id')

survey_data <- POST("https://api.c3.ai/covid/api/1/surveydata/fetch",
                    accept("application/json"))


test <- fromJSON(rawToChar(survey_data$content))


df <- test$objs

##Do it their way
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


census <- 
  fetch(
    "populationdata",
    list(
      spec = list(
        limit = -1,
        filter= 'contains(id, "_UnitedStates" ) && origin == "United States Census" '
      )
    ),
    get_all = TRUE
  )


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

#readr::write_csv(rover, here::here('survey_data.csv'))