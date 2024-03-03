

# Load data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names()

# Numbers
(records_extracted = nrow(d))

event_modified_to_duathlon = d %>% filter(event_modified_to_duathlon == 'TRUE')
table(event_modified_to_duathlon$event)
table(event_modified_to_duathlon$program_id)

(records_minus_duathlon = records_extracted - nrow(event_modified_to_duathlon))

# Did not finish, etc
df = d %>% filter(event_modified_to_duathlon == 'FALSE')
table(df$status)

length(unique(df$event))

df2 = df %>% filter(status == '0') %>%
  drop_na(sport_category)
nrow(df2)

# No swim result
df2 %>% filter(swim==0) %>%
  nrow(.)

dswim = df2 %>% filter(swim>0)
nrow(dswim)
dswim %>% select(water_temperature, sport_category, age, wet_suits, sex) %>%
  gg_miss_upset() # missing
dswim %>% select(water_temperature, sport_category, age, wet_suits, sex) %>%
  sapply(., function(x) sum(is.na(x))) %>%
  .[which(.>0)]

water_miss = cbind(dswim,
                 dswim %>% select(water_temperature) %>% rename(water_temp_missing = water_temperature) %>% is.na()) %>%
  filter(water_temp_missing == 'TRUE')
table(water_miss$event); length(unique(water_miss$event))
table(water_miss$program_id); length(unique(water_miss$program_id))

dswim %>% drop_na(water_temperature, sex, year_s, sport_category, wet_suits) %>% # complete cases
  nrow(.)

dswimming <- dswim %>%
  drop_na(water_temperature, sex, year_s, sport_category, wet_suits)

length(unique(dswimming$program_id))
length(unique(dswimming$event))

# No run and/or bike result
df2$bike[df2$bike == 0] <- NA
df2$run[df2$run == 0] <- NA
df2$swim[df2$swim == 0] <- NA

df2 %>% select(bike,run) %>% gg_miss_upset()

d_non_water = df2 %>% filter(run>0, bike>0) %>%
  filter(total_time <123*60)
d_non_water %>% select(air_temperature, sport_category) %>% vis_miss() # missing
d_non_water %>% select(air_temperature, sport_category) %>%
  sapply(., function(x) sum(is.na(x))) %>%
  .[which(.>0)]

air_miss = cbind(d_non_water,
                 d_non_water %>% select(air_temperature) %>% rename(air_temperature_missing = air_temperature) %>% is.na()) %>%
  filter(air_temperature_missing == 'TRUE')
table(air_miss$event); length(unique(air_miss$event))
table(air_miss$program_id); length(unique(air_miss$program_id))

d_non_water %>% drop_na(air_temperature, sex, swim, year_s, sport_category) %>% # complete cases
  nrow(.)

dcombined <- d_non_water %>% drop_na(air_temperature, sex, year_s, sport_category) # complete cases
dcombined %>% gg_miss_upset()


# Minus 1 athlete with a race result of 124.13 minutes


#### End




