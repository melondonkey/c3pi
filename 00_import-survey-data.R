library(httr)

library(jsonlite)


surveys <- read_data_json('surveydata', 'fetch', 'id')

survey_data <- POST("https://api.c3.ai/covid/api/1/surveydata/fetch",
                    accept("application/json"))


test <- fromJSON(rawToChar(survey_data$content))


df <- test$objs

