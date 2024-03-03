

# Air temperature analysis
# January 2024
# Borg DN

# Load data
d <- readRDS('data_paratriathlon.RDS') %>%
  clean_names() %>%
  filter(event_modified_to_duathlon == 'FALSE', # select real tri events
         status == '0', # select finishers only
         total_time <7380) # remove an erroneous data point

d$swim[d$swim == 0] <- NA # change 0 to NA
d$run[d$run == 0] <- NA # change 0 to NA
d$bike[d$bike == 0] <- NA # change 0 to NA

# Columns with missing values
sapply(d, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

d <- d %>%
  drop_na(bike, run) %>% # drop missing bike and missing run
  mutate(y = (bike+run)) # combine bike and run times

# Columns with missing values
sapply(d, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

d$swim_s = center(d$swim, type = "CWC", cluster = d$sex)/60 # centering and covert to minutes from seconds
d$age_s = d$age-mean(d$age)

# Columns with missing values
sapply(d, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

dsub <- d %>%
  select(y,
         air_temperature,
         sex,
         swim_s,
         sport_category,
         age_s,
         program_id)

# Columns with missing values
sapply(dsub, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

# Air temp and total time
d %>%
  ggplot(aes(x = air_temperature,
             y = y,
             colour = sport_category))+
  geom_point()+
  facet_wrap(~sex)+
  geom_smooth(aes(group = sport_category),
              colour = 'red',
              method = 'lm',
              formula = y ~ poly(x, degree = 2))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(breaks = seq(45, 95, by = 10))


# Look at data
hist(dsub$y, breaks = 100)
length(unique(dsub$event))
length(unique(dsub$program_id))
table(dsub$air_temperature)

# impute data
d %>% dplyr::select(year,air_temperature) %>%
  filter(year != '2020') %>%
  gg_miss_fct(fct = year)

imp_datasets <- mice(dsub, m = 5, method = "rf", seed = 123)
imp_datasets$loggedEvents

stripplot(imp_datasets, swim_s, pch = 19, xlab = "Imputation number") # look at where data is imputed to
stripplot(imp_datasets, air_temperature, pch = 19, xlab = "Imputation number") # look at where data is imputed to

# Model
# Settings
cores_set = 8
iter_set = 10000
chains_set = 4
seed_set = 123

fit <- brm_multiple(y ~ poly(air_temperature, degree = 2) + sex + sex*swim_s + sport_category + age_s + (1|program_id),
           family = negbinomial(),
           data = imp_datasets,
           cores = cores_set,
           iter = iter_set,
           chains = chains_set,
           seed = seed_set)

# Save model
save(fit, file = "fit_bikerun.RData")

# Check Rhats
round(fit$rhats, 3)

# Predictive check and chains
pp_check(fit, re_formula = NULL, ndraws = 100)
plot(fit)

res <- residuals(fit) %>% as.data.frame()
hist(res$Estimate, breaks = 50)

# Summary
summary(fit)

gather_draws(fit, b_polyair_temperaturedegreeEQ21) %>% mean_qi(.value>0)
gather_draws(fit, b_polyair_temperaturedegreeEQ22) %>% mean_qi(.value>0)
gather_draws(fit, b_sexWomen) %>% mean_qi(.value>0)
gather_draws(fit, b_swim_s) %>% mean_qi(.value>0)
gather_draws(fit, b_sport_categoryH2) %>% mean_qi(.value<0)
gather_draws(fit, b_age_s) %>% mean_qi(.value<0)
gather_draws(fit, `b_sexWomen:swim_s`) %>% mean_qi(.value<0)

posterior <- as.matrix(fit)
color_scheme_set('darkgray')
mcmc_intervals(posterior,
           pars = c('b_polyair_temperaturedegreeEQ21',
                    'b_polyair_temperaturedegreeEQ22',
                    'b_sexWomen',
                    'b_sport_categoryH2',
                    'b_age_s',
                    'b_swim_s',
                    'b_sexWomen:swim_s'),
           prob_outer = .95,
           prob = 2/3,
           point_est = 'mean')+
  geom_vline(xintercept = 0, colour = 'red')

# Fitted values
conditional_effects(fit, prob = 0.5)

# Generate plot for paper
conditions <- make_conditions(fit, vars = 'sex')

plot_99 <-
  conditional_effects(fit, conditions = conditions, effect = 'air_temperature', prob = 0.99)[1] %>%
  as.data.frame() %>%
  rename(x = air_temperature.air_temperature,
         y = air_temperature.estimate__,
         lower = air_temperature.lower__,
         upper = air_temperature.upper__,
         sex = air_temperature.cond__)

plot_99 %>%
  mutate(sex = recode_factor(sex, 'sex = Men' = 'Men', 'sex = Women' = 'Women')) %>%
  ggplot(aes(x = x,
             y = y/60))+
  geom_ribbon(aes(ymin = (lower)/60,
                  ymax = (upper)/60),
              alpha = 0.3,
              fill = '#08519C')+
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
  coord_cartesian(ylim = c(45,75))+
  geom_point(data = dsub,
             aes(x = air_temperature,
                 y = y/60),
             shape = 3,
             colour = 'black')



# Effects on observed scale
sex_ef <-
  conditional_effects(fit, effect = 'sex', prob = 0.95)[1] %>%
  data.frame() %>%
  select(md = sex.estimate__)

md = (sex_ef$md[1]-sex_ef$md[2]); md

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
age_ef <-
  conditional_effects(fit, effect = 'age_s', prob = 0.95)[1] %>%
  data.frame() %>%
  select(age = age_s.age_s,
         y = age_s.estimate__)

age_ef %>%
  ggplot()+
  geom_point(aes(x = age,
                 y = y))

mod <- lm(y ~ age, data = age_ef)

mod %>%
  coef() %>%
  getElement(name = 'age')

# Age effect
df <-
  dsub %>%
  filter(air_temperature == '24') %>%
  group_by(program_id) %>%
  data_grid(air_temperature,sex,swim_s,age_s,sport_category) %>%
  add_fitted_draws(fit) %>%
  as.data.frame()

dat <- df %>%
  mutate(age = age_s+mean(d$age, na.rm = T)) %>%
  select(age,.value)

lmod <- lm(mu ~ age, data = dat)

lmod %>% coef() %>%
  getElement('age')

confint(lmod)




#### End


