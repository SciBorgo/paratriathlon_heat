

# Race locations and years

# Load data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE')

# Events
table(d$event)

# Competition
d_comp <-
  d %>%
  group_by(event,year,location,event_type) %>%
  summarise(n=n())

d_comp %>%
  group_by(year) %>%
  summarise(n=n()) %>%
  arrange(year)

d_comp %>%
  group_by(location) %>%
  summarise(n=n()) %>%
  arrange(-n) %>%
  View()

d_comp %>%
  group_by(event_type) %>%
  summarise(n=n()) %>%
  arrange(-n) 


#### End

