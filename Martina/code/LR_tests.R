
p_load(broom)

# plotting LR relationship 
an_df %>% 
  ggplot(aes(x = diagage,
             y = d_dmode_n)) +
  geom_jitter(height = 0.05,
              alpha = 0.1) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)
  theme_classic()

 
  
model <-glm(d_dmode_n ~ d_grade_lab,
             data = an_df,
             family = "binomial")
summary(model)

odds_ratios <- exp(cbind(OR = coef(model), confint(model)))

tidy_model <- tidy(model)
tidy_model$odds_ratio <- exp(tidy_model$estimate)
tidy_model

conf_intervals <- confint(model)

tidy_model <- tidy_model %>% 
  mutate(conf.low = conf_intervals[, 1],
         conf.high = conf_intervals[, 2],
         odds_ratio = exp(estimate),
         conf.low.odds = exp(conf.low),
         conf.high.odds = exp(conf.high)
         ) %>% 
  select(estimate, std.error, conf.low, conf.high, statistic, p.value, odds_ratio, conf.low.odds, conf.high.odds)

tidy_model


# pipe option for glm 

model <- an_df %>% 
  mutate(d_grade_f = as.factor(d_grade)) %>% 
  glm(formula = d_dmode_n ~ d_grade_f,
            family = "binomial")
summary(model)


df <- an_df %>% 
  mutate(d_grade_f = as.factor(d_grade))

model <-glm(d_dmode_n ~ d_grade_f,
            data = df,
            family = "binomial")

summary(model)


# option 2 
tidy_model <- model %>% 
  tidy(exponentiate = TRUE,
       conf.int = TRUE)

tidy_model

an_df %>% tabyl(d_grade)