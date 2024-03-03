

# Age, gender, classification analysis
d <- readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE')

# "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"

hist(d$age, breaks = 50)

d %>%
  group_by(sex, sport_category, athlete_id) %>%
  summarise(n=n()) -> o

# Plot of age
d %>%
  group_by(athlete_id, year, sex) %>%
  summarise(age = median(age)) -> age

age %>%
  group_by(year, sex) %>%
  rename(Sex = sex) %>%
  mutate(Sex = recode_factor(Sex,
                             'Men' = 'Male', 'Women' = 'Female')) %>%
  summarise(mu = mean(age),
            sigma = sd(age)) %>%
  ggplot()+
  geom_pointrange(aes(x = year,
                      y = mu,
                      ymin = mu-sigma,
                      ymax = mu+sigma,
                      colour = Sex,
                      shape = Sex),
                  position = position_dodge2(width = 0.3))+
  geom_line(aes(x = year,
                y = mu,
                group = Sex,
                colour = Sex),
            position = position_dodge2(width = 0.3))+
  #facet_wrap(~'Athlete age')+
  theme_classic()+
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())+
  scale_colour_manual(values = c("#9ECAE1","#08306B"))+
  scale_shape_manual(values = c(19,15))+
  scale_x_continuous(n.breaks = 6)+
  scale_y_continuous(n.breaks = 6)+
  labs(x = 'Year',
       y = 'Age (years)') -> plotA; plotA


# Plot of gender
d %>%
  group_by(year, sex, athlete_id) %>%
  summarise(n=n()) -> gender

gender %>%
  group_by(year,sex) %>%
  summarise(n=n()) %>%
  mutate(Sex = recode_factor(sex, 'Men' = 'Male', 'Women' = 'Female')) %>%
  ggplot(aes(x = year,
             y = n,
             fill = Sex))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())+
  labs(x = 'Year',
       y = 'Number of unique athletes')+
  scale_fill_manual(values = c("#9ECAE1","#08306B"))+
  scale_x_continuous(n.breaks = 7) -> plotB; plotB

# Proportion of women
gender %>%
  group_by(year,sex) %>%
  summarise(n=n()) -> plot

plot %>%
  filter(sex == 'Women') %>%
  rename(women = n) %>%
  select(-sex) %>%
  left_join(plot %>% group_by(year) %>% summarise(total_n = sum(n)),
            by = 'year') %>%
  mutate(prop = women/total_n) %>%
  ggplot(aes(x = year,
             y = prop))+
  geom_line(colour = '#08306B')+
  geom_point(size = 2.5, colour = '#08306B')+ #, shape = 21, fill = 'white'
  theme_classic()+
  theme(panel.grid = element_blank())+
  scale_x_continuous(n.breaks = 7)+
  scale_y_continuous(labels = scales::percent, limits = c(0.2,0.7))+
  #facet_grid(~'Proportion of female athletes')+
  labs(x = 'Year',
       y = 'Proportion of females (%)') -> plotC; plotC

# Plot of events
d %>%
  group_by(year, event) %>%
  summarise(n=n()) -> events

events %>%
  group_by(year) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = year,
             y = n))+
  geom_bar(stat = "identity",
           fill = 'black')+ # "#08306B"
  theme_classic()+
  theme(panel.grid = element_blank())+
  labs(x = 'Year',
       y = 'Number of events')+
  scale_x_continuous(n.breaks = 7)+
  scale_y_continuous(breaks = seq(0,12, by = 2), limits = c(0,12)) -> plotD; plotD

# Save panel plot
patchwork = (plotD+plotB)/(plotC+plotA)
patchwork + plot_annotation(tag_levels = 'A')

ggsave(file = 'figure0.png',
       dpi = 900,
       width = 6.75,
       height = 6.75)


#### End



