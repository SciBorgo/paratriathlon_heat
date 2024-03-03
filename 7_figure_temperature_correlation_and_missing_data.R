

# Temperature figure
# May 2023

# Load data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE',
         air_temperature >0,
         water_temperature >0) %>%
  mutate(year_new = year-2000,
         year_new = paste("'", year_new, sep = ""),
         location_new = recode_factor(location,
                                  'Yokohama' = 'Yokohama',
                                  'Gold Coast' = 'GC',
                                  'Besancon' = 'Besancon',
                                  'Altafulla' = 'Alt',
                                  'Iseo Franciacorta' = 'Iseo-Franciacorta',
                                  'Magog' = 'Mag',
                                  'Edmonton' = 'Edm',
                                  'Rotterdam' = 'Rot',
                                  'Sarasota' = 'Sar',
                                  'Devonport' = 'Dev',
                                  'Aguilas' = 'Agu',
                                  'Eton Dorney' = 'ED',
                                  'Sarasota-Bradenton' = 'Sar',
                                  'Funchal' = 'Fun',
                                  'Milan' = 'Mil',
                                  'Banyoles' = 'Ban',
                                  'Alanya' = 'Alanya',
                                  'Montreal' = 'Mon',
                                  'Tokyo' = 'Tokyo',
                                  'Leeds' = 'Lee',
                                  'Coruna' = 'AC',
                                  'Alhandra' = 'Alh',
                                  'Abu Dhabi' = 'Abu Dhabi',
                                  'Swansea' = 'Swa',
                                  'Lausanne' = 'Lausanne'),
         label = paste(year_new, location_new, sep = " "),
         plot_group = recode_factor(label,
                                  "'17 Alt" = '5',
                                  "'17 Edm" = '5',
                                  "'17 GC"= '1',
                                  "'17 Iseo-Franciacorta"= '2',
                                  "'17 Mag"= '5',
                                  "'17 Rot"= '5',
                                  "'17 Yokohama"= '4',
                                  "'18 Agu"= '1',
                                  "'18 Dev"= '4',
                                  "'18 ED"= '4',
                                  "'18 GC"= '5',
                                  "'18 Iseo-Franciacorta"= '2',
                                  "'18 Lausanne"= '4',
                                  "'18 Mag"= '5',
                                  "'18 Yokohama"= '5',
                                  "'19 Alanya"= '2',
                                  "'19 Ban"= '5',
                                  "'19 Dev"= '5',
                                  "'19 Lausanne"= '2',
                                  "'19 Mag"= '5',
                                  "'19 Mil"= '4',
                                  "'19 Mon"= '4',
                                  "'19 Yokohama"= '4',
                                  "'21 AC"= '5',
                                  "'21 Abu Dhabi"= '2',
                                  "'21 Alanya"= '2',
                                  "'21 Alh"= '5',
                                  "'21 Besancon"= '4',
                                  "'21 Lee"= '1',
                                  "'21 Yokohama"= '4',
                                  "'22 AC"= '5',
                                  "'22 Abu Dhabi"= '2',
                                  "'22 Alanya"= '2',
                                  "'22 Alh"= '5',
                                  "'22 Besancon"= '2',
                                  "'22 Mon"= '1',
                                  "'22 Swa"= '4',
                                  "'23 Abu Dhabi"= '4',
                                  "'23 Dev"= '5',
                                  "'22 Swa"= '1',
                                  "'23 Abu Dhabi"= '1',
                                  "'23 Dev"= '5',
                                  "'21 Tokyo"= '2'))
                                  
table(d$plot_group)


  

# table(d$location_new)
# table(d$plot_group)


# Locations, temp versus temp
d %>%
  ggplot(aes(x = water_temperature,
             y = air_temperature,
             label = label))+
  geom_abline(intercept = 0, slope = 1, colour = 'gray80', size = 0.25,
              linetype = 'longdash')+
  geom_point(colour = '#08306B')+
  theme_classic(base_size = 10)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(size = 10))+
  labs(x = 'Water temperature (°C)',
       y = 'Air temperature (°C)')+
  geom_text(data = d %>% filter(plot_group == '2'),
            hjust = -0.1,
            vjust = -0.25,
            size = 2.35,
            check_overlap = F,
            colour = 'gray40')+
  geom_text(data = d %>% filter(plot_group == '3'),
            hjust = 1.25,
            vjust = 1.7,
            size = 2.35,
            check_overlap = F,
            colour = 'gray40')+
  scale_x_continuous(limits = c(14,34.25), breaks = seq(12, 36, by = 2))+
  scale_y_continuous(limits = c(12.5,34.25), breaks = seq(12, 36, by = 2)) -> p1; p1


# Location plot
# Load data
d = readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE')

dl <-
  d %>%
  group_by(event, location) %>%
  summarise(n=n())

dmissingloc <-
  d %>%
  group_by(event, location, air_temperature) %>%
  summarise(n=n()) %>%
  subset(is.na(air_temperature))

miss_dat <-
  dmissingloc %>%
  select(location) %>%
  group_by(location) %>%
  summarise(missing=n())

# "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"

dl %>%
  select(-n) %>%
  group_by(location) %>%
  summarise(n=n()) %>%
  arrange(-n) %>%
  left_join(miss_dat, by = 'location',
            keep = T) %>%
  mutate(n = as.integer(n),
         missing = ifelse(is.na(missing), 0, missing),
         location.y = NULL,
         present = n-missing,
         n = NULL) %>%
  rename(location = location.x) %>%
  pivot_longer(cols = c(missing:present),
               values_to = 'count',
               names_to = 'Temperature data') %>%
  ggplot(aes(x = reorder(location, -count),
             y = count,
             fill = `Temperature data`))+
  geom_bar(stat = 'identity')+
  theme_classic(base_size = 10)+
  theme(panel.grid = element_blank(),
        axis.text.x=element_text(angle=45, hjust=1),
        strip.text = element_text(size = 10))+
  #facet_grid(~'Event locations')+
  scale_y_continuous(limits = c(0,6.25), n.breaks = 7)+
  labs(x = 'Location',
       y = 'Number of races')+
  scale_fill_manual(values = c("#9ECAE1","#08306B"),
                    name = 'Temp data') -> p2; p2




# Save panel plot
patchwork = (p1)/(p2)
patchwork + plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(1,0.4))

ggsave(file = 'figure1.png',
       dpi = 900,
       width = 5.5,
       height = 6.5)




#### End




