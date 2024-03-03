

# Exploratory plot of time against temperature
# August 2023

# Load swim data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE',
         status == '0',
         swim>0) %>%
  mutate(y = swim/60,
         sport_category = recode_factor(sport_category,
                                        'H1' = '1',
                                        'H2' = '2'))

d %>%
  drop_na(wet_suits,sport_category) %>%
  mutate(Class = sport_category) %>%
  ggplot(aes(x = water_temperature,
             y = swim/60))+
  geom_point(shape = 3, aes(colour = Class))+
  facet_grid(~sex)+
  theme_classic()+
  scale_color_manual(values = c('#9ECAE1','#08306B'))+
  labs(x = 'Water temperature (°C)',
       y = 'Swim time\n(min)')+
  xlim(14.5,31.5) -> p1; p1


# Load combined cycle and run data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  mutate(y = (bike+run)/60,
         swim = swim/60,
         total_time = total_time/60,
         sport_category = recode_factor(sport_category,
                                        'H1' = '1',
                                        'H2' = '2')) %>%
  filter(event_modified_to_duathlon == 'FALSE',
         status == '0',
         total_time <123) # Total time less than 120 (+3 min)

d <- d %>% filter(run >0, bike >0)

d %>%
  drop_na(sport_category) %>%
  mutate(Class = sport_category) %>%
  ggplot(aes(x = air_temperature,
             y = y))+
  geom_point(shape = 3, aes(colour = Class))+
  facet_grid(~sex)+
  theme_classic()+
  scale_color_manual(values = c('#9ECAE1','#08306B'))+
  labs(x = 'Air temperature (°C)',
       y = 'Handcycle and push time\n(min)')+
  xlim(13,35)+
  ylim(38,90) -> p2; p2


# Panel
patchwork = (p1)/(p2)
patchwork + plot_annotation(tag_levels = 'A')

ggsave(file = 'supplement2_figure_eda.png',
       dpi = 900,
       width = 6.5,
       height = 6)



#### Same plot as above with loess fits for sport class
# Load swim data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE',
         status == '0',
         swim>0) %>%
  mutate(y = swim/60,
         sport_category = recode_factor(sport_category,
                                        'H1' = '1',
                                        'H2' = '2'))

d %>%
  drop_na(wet_suits,sport_category) %>%
  mutate(Class = sport_category) %>%
  ggplot(aes(x = water_temperature,
             y = swim/60))+
  geom_point(shape = 3, aes(colour = Class))+
  facet_grid(~sex)+
  theme_classic()+
  scale_color_manual(values = c('darkorange3','#08306B'))+
  labs(x = 'Water temperature (°C)',
       y = 'Swim time\n(min)')+
  xlim(14.5,31.5) +
  geom_smooth(aes(group = sport_category,
                  colour = sport_category),
              method = 'loess') -> p3; p3


# Load combined cycle and run data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  mutate(y = (bike+run)/60,
         swim = swim/60,
         total_time = total_time/60,
         sport_category = recode_factor(sport_category,
                                        'H1' = '1',
                                        'H2' = '2')) %>%
  filter(event_modified_to_duathlon == 'FALSE',
         status == '0',
         total_time <123) # Total time less than 120 (+3 min)

d <- d %>% filter(run >0, bike >0)

d %>%
  drop_na(sport_category) %>%
  mutate(Class = sport_category) %>%
  ggplot(aes(x = air_temperature,
             y = y))+
  geom_point(shape = 3, aes(colour = Class))+
  facet_grid(~sex)+
  theme_classic()+
  scale_color_manual(values = c('darkorange3','#08306B'))+
  labs(x = 'Air temperature (°C)',
       y = 'Handcycle and push time\n(min)')+
  xlim(13,35)+
  ylim(38,90)+
  geom_smooth(aes(group = sport_category,
                  colour = sport_category),
              method = 'lm',
              formula = y ~ poly(x, degree = 2)) -> p4; p4


# Panel
patchwork = (p3)/(p4)
patchwork + plot_annotation(tag_levels = 'A')

ggsave(file = 'supplement3_figure_eda.png',
       dpi = 900,
       width = 6.5,
       height = 6)



#### End

