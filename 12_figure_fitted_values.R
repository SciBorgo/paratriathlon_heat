

# Figure: fitted values
# January 2024
# Borg DN

# "#9ECAE1","#08306B"

# Swim segment performance
load("/Users/david/Dropbox/Research projects/Project - Paratriathlon heat/3_Data analysis/fit_swim.RData")

conditions <- make_conditions(fit, vars = 'sex')

plot_95 <-
  conditional_effects(fit, conditions = conditions, effect = 'water_temperature', prob = 0.95)[1] %>%
  as.data.frame() %>%
  rename(
    x = water_temperature.water_temperature,
    y = water_temperature.estimate__,
    lower = water_temperature.lower__,
    upper = water_temperature.upper__,
    sex = water_temperature.cond__)

plot_95 %>%
  mutate(sex = recode_factor(sex, 'sex = Men' = 'Men', 'sex = Women' = 'Women')) %>%
  ggplot()+
  geom_ribbon(aes(x = x,
                  y = y/60,
                  ymin = (lower)/60,
                  ymax = (upper)/60,
                  fill = sex),
              alpha = 0.65)+ #,fill = '#08519C'
  geom_line(aes(x = x,
                y = y/60),
            colour = 'black',
            size = 1)+
  facet_wrap(~sex)+
  theme_classic()+
  labs(x = 'Water temperature (°C)',
       y = 'Swim time (min)')+
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        strip.background = element_rect(fill = "white"))+
  scale_y_continuous(breaks = seq(0,25, by = 2))+
  scale_x_continuous(breaks = seq(0,40, by = 2))+
  coord_cartesian(ylim = c(10,18.5))+
  scale_fill_manual(values = c("#9ECAE1","#08306B"))+
  guides(fill = 'none') -> p1;p1



# Combined cycle and run time
load("/Users/david/Dropbox/Research projects/Project - Paratriathlon heat/3_Data analysis/fit_bikerun.RData")

conditions <- make_conditions(fit, vars = 'sex')

plot_95 <-
  conditional_effects(fit, conditions = conditions, effect = 'air_temperature', prob = 0.95)[1] %>%
  as.data.frame() %>%
  rename(x = air_temperature.air_temperature,
         y = air_temperature.estimate__,
         lower = air_temperature.lower__,
         upper = air_temperature.upper__,
         sex = air_temperature.cond__)

plot_95 %>%
  mutate(sex = recode_factor(sex, 'sex = Men' = 'Men', 'sex = Women' = 'Women')) %>%
  ggplot(aes(x = x,
             y = y/60))+
  geom_ribbon(aes(ymin = (lower)/60,
                  ymax = (upper)/60,
                  fill = sex),
              alpha = 0.65)+
  geom_line(aes(x = x,
                y = y/60),
            colour = 'black',
            size = 1)+
  facet_wrap(~sex)+
  theme_classic()+
  labs(x = 'Air temperature (°C)',
       y = 'Combined handcycle and push time\n(min)')+
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        strip.background = element_rect(fill = "white"))+
  scale_y_continuous(breaks = seq(30,80, by = 5))+
  scale_x_continuous(breaks = seq(10,40, by = 4))+
  coord_cartesian(ylim = c(50,72))+
  scale_fill_manual(values = c("#9ECAE1","#08306B"))+
  guides(fill = 'none') -> p2;p2



# Panel
patchwork = (p1)/(p2)
patchwork + plot_annotation(tag_levels = 'A')

ggsave(file = 'figure_fitted.png',
       dpi = 900,
       width = 6.5,
       height = 7)


#### End







