

# Load data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names()

# Exclude duathlon events
event_modified_to_duathlon = d %>% filter(event_modified_to_duathlon == 'TRUE')

df = d %>% filter(event_modified_to_duathlon == 'FALSE')
nrow(df)

# Unique athletes
length(unique(df$athlete_id))

# Gender
df %>% group_by(athlete_id, sex) %>%
  summarise(n=n()) %>%
  select(-n) -> gender

gender %>% group_by(sex) %>%
  summarise(n=n())

# Age
d %>%
  group_by(athlete_id, year, sex) %>%
  summarise(age = median(age)) -> age

age %>%
  group_by(year, sex) %>%
  mutate(sex = recode_factor(sex,
                             'Men' = 'Age of male athletes', 'Women' = 'Age of female athletes')) %>%
  summarise(mu = mean(age),
            sigma = sd(age)) %>%
  arrange(sex)


#### End