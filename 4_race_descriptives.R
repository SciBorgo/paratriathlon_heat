

# Load data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE')

# Swim
d %>% filter(swim>0) %>%
  mutate(swim = swim/60) %>%
  group_by(sex) %>%
  summarise(mu = mean(swim),
            sigma = sd(swim))

d %>% filter(swim>0) %>%
  group_by(sex) %>%
  summarise(mu = mean(swim_speed, na.rm = T),
            sigma = sd(swim_speed, na.rm = T))

# Bike
d %>% filter(bike>0) %>%
  mutate(bike = bike/60) %>%
  group_by(sex) %>%
  summarise(mu = mean(bike),
            sigma = sd(bike))

d %>% filter(bike>0) %>%
  group_by(sex) %>%
  summarise(mu = mean(bike_speed, na.rm = T),
            sigma = sd(bike_speed, na.rm = T))


# Run
d %>% filter(run>0) %>%
  mutate(run = run/60) %>%
  group_by(sex) %>%
  summarise(mu = mean(run),
            sigma = sd(run))

d %>% filter(run>0) %>%
  group_by(sex) %>%
  summarise(mu = mean(run_speed, na.rm = T),
            sigma = sd(run_speed, na.rm = T))


# Total race time
d %>% filter(total_time>0) %>%
  mutate(total_time = total_time/60) %>%
  group_by(sex) %>%
  summarise(mu = mean(total_time),
            sigma = sd(total_time))


#### End

