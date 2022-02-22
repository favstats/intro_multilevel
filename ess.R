
library(tidyverse)
library(essurvey)

ess <- readRDS("data/ess_example") %>% 
  drop_na(income, cntry, education)

cntry_dat <- haven::read_spss("data/ESSMD-2020-cntry_F1.sav")

ess <- ess %>% 
  left_join(cntry_dat %>% 
              select(cntry, gini_coef = c_gini_2018, lt_unemployment_rate = c_loun_pc_act_2018, net_migration = c_cnmigratrt_2018))

saveRDS(ess, file = "data/ess_example")

example_data <- palmerpenguins::penguins %>% 
  select(Group = species, x = bill_length_mm, y = flipper_length_mm)  %>% 
  mutate(x = (80 - x) / 5,
         y = (300 - y) / 15) %>%
  bind_rows(palmerpenguins::penguins %>% 
              mutate(Group = paste0(species, "B")) %>% 
              select(Group, x = bill_length_mm, y = flipper_length_mm)  %>% 
              mutate(x = (85 - x) / 5,
                     y = (305 - y) / 15)) %>% 
  group_by(Group) %>% 
  mutate(y_rev = rev(y)) %>% 
  ungroup(Group) %>% 
  # arrange(desc(y)) %>% 
  mutate(y = ifelse(Group == "Gentoo", y + 2, y)) %>% 
  mutate(y = ifelse(Group == "GentooB", y - 2.5, y)) %>% 
  mutate(y = ifelse(Group == "AdelieB", y + 2, y)) %>% 
  mutate(y = ifelse(Group == "Adelie", y - 1, y)) %>% 
  mutate(y = ifelse(Group == "Chinstrap", y + 2, y)) %>%
  mutate(y = ifelse(Group == "ChinstrapB", y - 2, y)) %>%
  group_by(Group) %>% 
  mutate(y = ifelse(Group %in% c("ChinstrapB", "AdelieB", "Gentoo"), rev(y), y)) %>% 
  ungroup(Group) %>% 
  mutate(x = ifelse(Group %in% c("GentooB"), x-2, x)) %>% 
  mutate(x = ifelse(Group %in% c("Gentoo"), x+2, x))

saveRDS(example_data, file = "data/example_data.rds")

# wave9 <- essurvey::download_rounds(9, ess_email = "fabio.votta@gmail.com")

ess9 <- haven::read_dta("ESS9/ESS9e03_1.dta", encoding = 'latin1')

sub <- ess9 %>%
  # dplyr::select(netusoft, stfgov) %>% 
  filter(cntry %in% c("IT", "NL", "DE", "CH", "PL", "LV")) 

ess9 %>%
  count(edulvlb, sort = T) %>% View
  # dplyr::select(psu, ppltrst, trstplt, trstprt) %>% 
  # dplyr::select(netusoft, stfgov) %>% 
  # filter(cntry %in% c("IT", "NL", "DE", "CH", "PL", "LV")) %>%
  ggplot(aes(edulvlb, hinctnta)) +
  geom_jitter() +
  # geom_smooth(method = "lm") +
  ggpubr::stat_cor() 

modd <- lm(stfgov ~ netusoft, data = sub)



sjPlot::plot_model(modd, type = "std") +
  ylim(0,0.1)

modd2 <- lme4::lmer(stfgov ~ netusoft + (1|cntry), data = sub)

sjPlot::plot_model(modd2, type = "std") +
  ylim(-0.1, 0.1)

ranef(modd2)

sjPlot::plot_model(modd2, type = "re") 




# see group coefficients
model_coefs <- coef(modd2)$cntry %>% 
  rename(Intercept = `(Intercept)`, Slope = netusoft) %>% 
  rownames_to_column("cntry")

# see coefficients
model_coefs



sleep_groups_rani <- left_join(sub, model_coefs, by = "cntry")

model_coef_plot <- ggplot(data = sleep_groups_rani, 
                          mapping = aes(x = netusoft, 
                                        y = stfgov, 
                                        colour = cntry)
) +
  geom_jitter(na.rm = T, alpha = 0.5) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope,
                  colour = cntry
  ),
  size = 1.5
  ) +
  # scale_y_continuous(limits = c(180, 1020)) +
  # scale_x_continuous(breaks = seq(1:10) - 1) +
  theme(legend.position = "top")

# see the plot
model_coef_plot











# # fit random intercepts model
model <- lme4::lmer(stfgov ~ netusoft + (netusoft | cntry), data = sub)

# see cntry coefficients
model_coefs <- coef(model)$cntry %>% 
  rename(Intercept = `(Intercept)`, Slope = netusoft) %>% 
  rownames_to_column("cntry")

# see coefficients
model_coefs

sleep_groups_rani <- left_join(ess9, model_coefs, by = "cntry")


model_coef_plot <- ggplot(data = sleep_groups_rani, 
                          mapping = aes(x = netusoft, 
                                        y = stfgov, 
                                        colour = cntry)
) +
  # geom_jitter(na.rm = T, alpha = 0.5) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope,
                  colour = cntry
  ),
  size = 1.5
  ) +
  scale_y_continuous(limits = c(3, 7)) +
  scale_x_continuous(limits = c(1,5)) +
  theme(legend.position = "top")

# see the plot
model_coef_plot

model_coefs %>% 
  arrange(desc(Slope))


model_coefs %>% 
  ggplot(aes(Intercept, Slope)) +
  geom_point() +
  geom_smooth(method = "lm")




wvs <- haven::read_spss("data/WVS_Cross-National_Wave_7_sav_v2_0.sav")


wvs %>% 
  select(Q275, Q288)%>%
  ggplot(aes(Q275, Q288)) +
  # geom_jitter() +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor() 



modd <- lm(Q288 ~ Q275, data = wvs)

icc(modd2)
sjstats::icc(modd2)
  
modd2 <- lme4::lmer(Q288 ~ Q275 + (Q275|B_COUNTRY_ALPHA), data = wvs)

sjPlot::plot_model(modd, type = "std")  +
  ylim(0.25, 0.35)

sjPlot::plot_model(modd2, type = "std") +
  ylim(0.25, 0.35)
  

coef(modd2)$B_COUNTRY_ALPHA %>% 
  janitor::clean_names() %>% 
  ggplot(aes(intercept, q275)) +
  geom_point() +
  geom_smooth(method = "lm")




# see group coefficients
model_coefs <- coef(modd2)$B_COUNTRY_ALPHA %>% 
  rename(Intercept = `(Intercept)`, Slope = Q275) %>% 
  rownames_to_column("B_COUNTRY_ALPHA")

# see coefficients
model_coefs



sleep_groups_rani <- left_join(wvs, model_coefs, by = "B_COUNTRY_ALPHA")

model_coef_plot <- ggplot(data = sleep_groups_rani, 
                          mapping = aes(x = Q275, 
                                        y = Q288, 
                                        colour = B_COUNTRY_ALPHA)
) +
  # geom_jitter(na.rm = T, alpha = 0.5) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope,
                  colour = B_COUNTRY_ALPHA
  ),
  size = 1.5
  ) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_x_continuous(limits = c(0, 8)) +
  theme(legend.position = "none")

# see the plot
model_coef_plot


wvs_small <- wvs %>% 
  select(education = Q275, income = Q288, cntry = B_COUNTRY_ALPHA) %>% 
  drop_na(education, income) %>% 
  group_by(cntry) %>% 
  sample_n(100)

write_rds(wvs_small, file = "slides/data/wvs_small.rds")


wvs %>% 
  select(education = Q275, income = Q288, cntry = B_COUNTRY_ALPHA) %>% 
  mutate(cntry = fct_reorder(cntry, income, .fun = median)) %>% 
  ggplot(aes(cntry, income)) +
  geom_boxplot() +
  coord_flip()

# edulvlb, hinctnta
ess9 %>% 
  filter(hinctnta < 1000) %>% 
  # select(education = Q275, income = Q288, cntry = B_COUNTRY_ALPHA) %>% 
  mutate(cntry = fct_reorder(cntry, hinctnta, .fun = median)) %>% 
  ggplot(aes(cntry, hinctnta)) +
  geom_boxplot() +
  coord_flip()


ess_inc <- ess9 %>% 
  filter(hinctnta < 1000) %>% 
  mutate(income = hinctnta) %>% 
  mutate(education = case_when(
    str_detect(sjmisc::to_label(edulvlb), "ISCED 1|ISCED level 1") ~ 1,
    str_detect(sjmisc::to_label(edulvlb), "ISCED 2") ~ 2,
    str_detect(sjmisc::to_label(edulvlb), "ISCED 3") ~ 3,
    str_detect(sjmisc::to_label(edulvlb), "ISCED 4") ~ 4,
    str_detect(sjmisc::to_label(edulvlb), "ISCED 5") ~ 5,
    str_detect(sjmisc::to_label(edulvlb), "ISCED 6") ~ 6,
    T ~ NA_real_
  ))

saveRDS(ess_inc, file = "data/ess_inc.rds")

# ess_inc %>% count(edulvlb, education)

modd <- lm(income ~ education, data = ess_inc)

# icc(modd2)
# sjstats::icc(modd2)

modd2 <- lme4::lmer(income ~ education + (education|cntry), data = ess_inc)

sjPlot::plot_model(modd, type = "std")  #+
  # ylim(0.25, 0.35)

sjPlot::plot_model(modd2, type = "std")# +
  # ylim(0.25, 0.35)


coef(modd2)$cntry %>% 
  janitor::clean_names() %>% 
  ggplot(aes(intercept, education)) +
  geom_point() +
  geom_smooth(method = "lm")




# see group coefficients
model_coefs <- coef(modd2)$B_COUNTRY_ALPHA %>% 
  rename(Intercept = `(Intercept)`, Slope = Q275) %>% 
  rownames_to_column("B_COUNTRY_ALPHA")

# see coefficients
model_coefs



sleep_groups_rani <- left_join(wvs, model_coefs, by = "B_COUNTRY_ALPHA")

model_coef_plot <- ggplot(data = sleep_groups_rani, 
                          mapping = aes(x = Q275, 
                                        y = Q288, 
                                        colour = B_COUNTRY_ALPHA)
) +
  # geom_jitter(na.rm = T, alpha = 0.5) +
  geom_abline(aes(intercept = Intercept, 
                  slope = Slope,
                  colour = B_COUNTRY_ALPHA
  ),
  size = 1.5
  ) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_x_continuous(limits = c(0, 8)) +
  theme(legend.position = "none")

# see the plot
model_coef_plot


wvs_small <- wvs %>% 
  select(education = Q275, income = Q288, cntry = B_COUNTRY_ALPHA) %>% 
  drop_na(education, income) %>% 
  group_by(cntry) %>% 
  sample_n(100)

write_rds(wvs_small, file = "slides/data/wvs_small.rds")


wvs %>% 
  select(education = Q275, income = Q288, cntry = B_COUNTRY_ALPHA) %>% 
  mutate(cntry = fct_reorder(cntry, income, .fun = median)) %>% 
  ggplot(aes(cntry, income)) +
  geom_boxplot() +
  coord_flip()


