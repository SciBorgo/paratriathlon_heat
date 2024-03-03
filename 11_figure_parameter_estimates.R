

# Figure: Parameter estimates
# January 2024
# Borg DN

# "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"

# Swim model
load("/Users/david/Dropbox/Research projects/Project - Paratriathlon heat/3_Data analysis/fit_swim.RData")

posterior <- as.matrix(fit)

dplot <-
  posterior %>%
  as.data.frame() %>%
  mutate(`Water temp–natural spline component 1` = b_nswater_temperatureknotsEQc22261,
         `Water temp–natural spline component 2` = b_nswater_temperatureknotsEQc22262,
         `Water temp–natural spline component 3` = b_nswater_temperatureknotsEQc22263,
         `Sex–women` = b_sexWomen,
         `Sport class–2` = b_sport_categoryH2,
         `Age` = b_age_s,
         `Wet suit–mandatory` = b_wet_suitsMandatory,
         `Wet suit–not allowed` = b_wet_suitsNotallowed) %>%
  select('Water temp–natural spline component 1',
         'Water temp–natural spline component 2',
         'Water temp–natural spline component 3',
         'Age',
         'Sex–women',
         'Sport class–2',
         'Wet suit–mandatory',
         'Wet suit–not allowed') %>%
  pivot_longer(names_to = 'parameter',
               values_to = 'effect',
               cols = everything()) %>%
  mutate(parameter = factor(parameter, levels = c('Water temp–natural spline component 1',
                                                  'Water temp–natural spline component 2',
                                                  'Water temp–natural spline component 3',
                                                  'Age',
                                                  'Sex–women',
                                                  'Sport class–2',
                                                  'Wet suit–mandatory',
                                                  'Wet suit–not allowed')))

dplot %>%
  ggplot()+
  stat_pointinterval(aes(x = effect,
                   y = parameter),
               .width = c(0.66,0.95),
               colour = '#08519C')+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(n.breaks = 7)+
  geom_vline(xintercept = 0, colour = 'black', size = 0.5, linetype = 5)+
  #coord_cartesian(xlim = c(-0.5,0.7))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  facet_grid(~'Swim model')+
  labs(x = 'Effect (logit)',
       y = 'Parameter\n') -> p1; p1



# Handcycle and push time model
load("/Users/david/Dropbox/Research projects/Project - Paratriathlon heat/3_Data analysis/fit_bikerun.RData")

posterior <- as.matrix(fit)

dplot <-
  posterior %>%
  as.data.frame() %>%
  mutate(`Air temp–linear component` = b_polyair_temperaturedegreeEQ21,
         `Air temp–cubic component` = b_polyair_temperaturedegreeEQ22,
         `Sex–women` = b_sexWomen,
         `Sport class–2` = b_sport_categoryH2,
         `Age` = b_age_s,
         `Swim time` = b_swim_s,
         `Swim time by sex–women` = `b_sexWomen:swim_s`) %>%
  select('Air temp–linear component',
         'Air temp–cubic component',
         'Age',
         'Sex–women',
         'Sport class–2',
         'Swim time',
         'Swim time by sex–women') %>%
  pivot_longer(names_to = 'parameter',
               values_to = 'effect',
               cols = everything()) %>%
  mutate(parameter = factor(parameter, levels = c('Air temp–linear component',
                                                  'Air temp–cubic component',
                                                  'Age',
                                                  'Sex–women',
                                                  'Sport class–2',
                                                  'Swim time',
                                                  'Swim time by sex–women')))

dplot %>%
  ggplot()+
  stat_pointinterval(aes(x = effect,
                         y = parameter),
                     .width = c(0.66,0.95),
                     colour = '#08519C')+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(n.breaks = 7)+
  geom_vline(xintercept = 0, colour = 'black', size = 0.5, linetype = 5)+
  #coord_cartesian(xlim = c(-0.5,0.7))+
  theme_classic()+
  theme(panel.grid = element_blank())+
  facet_grid(~'Combined handcycle and push model')+
  labs(x = 'Effect (logit)',
       y = 'Parameter\n') -> p2; p2




# Panel
patchwork = (p1)/(p2)
patchwork + plot_annotation(tag_levels = 'A')

ggsave(file = 'figure_parameters.png',
       dpi = 900,
       width = 6,
       height = 6)


#### End
