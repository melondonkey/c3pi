census <- 
  fetch(
    "populationdata",
    list(
      spec = list(
        limit = -1,
        filter= 'contains(id, "Cobb" ) && origin == "United States Census"'
      )
    ),
    get_all = TRUE
  )

#this census data does not have breakdowns by race
library(tidycensus)

vars <- load_variables(2015, "acs5")

varsfull <- load_variables(2010, "sf1")

varsfull2 <- 
  varsfull %>%
  filter(stringr::str_detect(concept, 'SEX BY AGE'))