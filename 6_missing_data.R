

# Missing data
# January 2024
# Borg DN

# Load data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE',
         status == '0',
         total_time <7380) # remove an erroneous data point

d$swim[d$swim == 0] <- NA # change 0 to NA
d$run[d$run == 0] <- NA # change 0 to NA
d$bike[d$bike == 0] <- NA # change 0 to NA

dsub <-
  d %>%
  select(
    year,
    program_id,
    event,
    athlete_id,
    sex,
    age,
    sport_category,
    swim,
    run,
    bike,
    water_temperature,
    wet_suits,
    air_temperature
  )

# Columns with missing values
sapply(dsub, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

# Missings
vis_miss(dsub)
gg_miss_upset(dsub, nsets = 20)
gg_miss_fct(x = dsub, fct = year)



#### End

