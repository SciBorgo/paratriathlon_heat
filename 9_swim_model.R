

# Water analysis
# January 2024
# Borg DN

# Load data
d <- readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE',
         status == '0',
         swim>0) %>% # remove missing swim times
  mutate(y = swim/60)

# Water temperature and swim time
d %>%
  group_by(sex, water_temperature) %>%
  summarise(mu = mean(swim)/60) %>%
  ggplot(aes(x = water_temperature,
             y = mu))+
  geom_point(shape = 3)+
  #geom_line()+
  facet_grid(~sex)+
  ylim(0,30)+
  geom_smooth(method = 'loess')

d %>%
  drop_na(wet_suits) %>%
  mutate(swim = swim/60,
         `Wet suit allowed` = wet_suits) %>%
  filter(swim <40,
         swim >0) %>%
  ggplot(aes(x = water_temperature,
             y = swim,
             colour = `Wet suit allowed`))+
  geom_point()+
  facet_wrap(~sex)+
  geom_smooth(aes(group = sport_category),
              method = 'loess',
              colour = 'black')+
  theme_bw()+
  labs(x = 'Water temperature',
       y = 'Swim time (minutes)')+
  theme(panel.grid = element_blank(),
        legend.position = 'bottom')+
  scale_colour_viridis_d(option = 'A', begin = 0.1, end = 0.8)

# Standardize age
d$age_s = d$age-mean(d$age, na.rm = T)

# Look at data
length(unique(d$event))
length(unique(d$program_id))
table(d$water_temperature)

# Variables for model
dsub <- d %>%
  select(swim,
         water_temperature,
         age_s,
         sex,
         sport_category,
         wet_suits,
         program_id,
         athlete_id)

# Impute missing data
d %>% dplyr::select(year, wet_suits) %>%
  filter(year != '2020') %>%
  gg_miss_fct(fct = year)

sapply(dsub, function(x) sum(is.na(x))) %>% # Columns with missing values
  .[which(.>0)]

imp_datasets <- mice(dsub, m = 5, method = "rf", seed = 123)
imp_datasets$loggedEvents

stripplot(imp_datasets, water_temperature, pch = 19, xlab = "Imputation number") # look at where data is imputed to

assign_var2 <- function(water_temperature) {
  if (water_temperature < 18) {
    return("Mandatory")
  } else if (water_temperature > 24.5) {
    return("Not allowed")
  } else {
    return("Allowed")
  }
}

# Apply the function to each imputed dataset
imp_datasets <- mice::complete(imp_datasets, "long") %>% 
  mutate(wet_suits_miss = is.na(wet_suits)) # Convert to long format and identify missing wetsuit locations

dsub_wet_miss <- imp_datasets %>% filter(wet_suits_miss == 'TRUE') # select out rows with wet suit data missing
dsub_wet_complete <- imp_datasets %>% filter(wet_suits_miss == 'FALSE') # select out wet suit non missing

dsub_wet_miss$wet_suits <- sapply(dsub_wet_miss$water_temperature, assign_var2)

dsub_reshape <- union(dsub_wet_miss, dsub_wet_complete)
vis_miss(dsub_reshape) # check for missing

list_of_dfs <- split(dsub_reshape, dsub_reshape$.imp)

# Settings
cores_set = 8
iter_set = 10000
chains_set = 4
seed_set = 123

# Model
fit <- brm_multiple(swim ~ ns(water_temperature, knots = c(22,26)) + sex + age_s + sport_category + wet_suits + (1|program_id),
           family = negbinomial(),
           data = list_of_dfs,
           cores = cores_set,
           iter = iter_set,
           chains = chains_set,
           seed = seed_set)


# Check Rhats
round(fit$rhats, 3)

# Predictive check and chains
pp_check(fit, re_formula = NULL, ndraws = 100)
plot(fit)

res <- residuals(fit) %>% as.data.frame()
hist(res$Estimate, breaks = 50)

# Save model
#save(fit, file = "fit_swim.RData")

# Summary
summary(fit)

gather_draws(fit, b_nswater_temperatureknotsEQc22261) %>% mean_qi(.value<0)
gather_draws(fit, b_nswater_temperatureknotsEQc22262) %>% mean_qi(.value<0)
gather_draws(fit, b_nswater_temperatureknotsEQc22263) %>% mean_qi(.value<0)
gather_draws(fit, b_sexWomen) %>% mean_qi(.value>0)
gather_draws(fit, b_age_s) %>% mean_qi(.value>0)
gather_draws(fit, b_sport_categoryH2) %>% mean_qi(.value<0)
gather_draws(fit, b_wet_suitsMandatory) %>% mean_qi(.value>0)
gather_draws(fit, b_wet_suitsNotallowed) %>% mean_qi(.value>0)

posterior <- as.matrix(fit)
color_scheme_set('darkgray')
mcmc_areas(posterior,
           pars = c('b_nswater_temperatureknotsEQc22261',
                    'b_nswater_temperatureknotsEQc22262',
                    'b_nswater_temperatureknotsEQc22263',
                    'b_sexWomen',
                    'b_sport_categoryH2',
                    'b_age_s',
                    'b_wet_suitsMandatory',
                    'b_wet_suitsNotallowed'),
           prob_outer = .95,
           prob = 2/3,
           point_est = 'mean')+
  geom_vline(xintercept = 0,
             colour = 'red')

# Fitted values
#conditional_effects(fit, prob = 0.5)

# Generate plot for paper
conditions <- make_conditions(fit, vars = 'sex')

plot_99 <-
  conditional_effects(fit, conditions = conditions, effect = 'water_temperature', prob = 0.99)[1] %>%
    as.data.frame() %>%
    rename(
      x = water_temperature.water_temperature,
      y = water_temperature.estimate__,
      lower = water_temperature.lower__,
      upper = water_temperature.upper__,
      sex = water_temperature.cond__)

plot_99 %>%
  mutate(sex = recode_factor(sex, 'sex = Men' = 'Men', 'sex = Women' = 'Women')) %>%
  ggplot()+
  geom_ribbon(aes(x = x,
                  y = y/60,
                  ymin = (lower)/60,
                  ymax = (upper)/60),
              alpha = 0.3,
              fill = '#08519C')+
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
  coord_cartesian(ylim = c(8,22))+
  geom_point(data = dsub,
             aes(x = water_temperature,
                 y = swim/60),
             shape = 3,
             size = 1)




# Effects on observed scale
# Sex effect
conditional_effects(fit, effect = 'sex', prob = 0.95)[1] %>%
  data.frame() %>%
  select(md = sex.estimate__)

sex_ef <- emmeans(fit, ~ sex)
k <- regrid(sex_ef)
pairs(k)

# Sport effect
conditional_effects(fit, effect = 'sport_category', prob = 0.95)[1] %>%
  data.frame() %>%
  select(md = sport_category.estimate__)

sport_ef <- emmeans(fit, ~ sport_category)
l <- regrid(sport_ef)
pairs(l)

# Age effect
df <-
  dsub %>%
  filter(water_temperature == '24.4') %>%
  group_by(program_id) %>%
  data_grid(water_temperature,sex,age_s,sport_category,wet_suits) %>%
  add_fitted_draws(fit) %>%
  as.data.frame()

dat <- df %>%
  mutate(age = age_s+mean(d$age, na.rm = T)) %>%
  select(age,.value)

lmod <- lm(.value ~ age, data = dat)

lmod %>% coef() %>%
  getElement('age')

confint(lmod)

# For discussion, time context
dsub %>% filter(sex == 'Men') %>% group_by(position) %>% summarise(mu = mean(total_time)/60,
                                                                   sd = sd(total_time)/60)

#### End



