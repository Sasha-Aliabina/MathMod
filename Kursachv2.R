library("tidyverse") 
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
tbl=read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl=tbl[-1,] 
tbl 
tbl=tbl[tbl$DOY > 222 & tbl$DOY < 349,] 
tbl=tbl[tbl$daytime == TRUE,] 
glimpse(tbl) 
tbl = select(tbl, -(roll)) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
sapply(tbl,is.numeric) 
tbl_numeric = tbl [,sapply (tbl,is.numeric) ] 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ] 
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
cor_td 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep="")) 
formula
mod=lm(formula, data = tbl)
anova(mod)
summary(mod)
formula1 = as.formula(paste("co2_flux ~ DOY + H + qc_LE + rand_err_H + LE + 
                            rand_err_co2_flux + h2o_flux + sonic_temperature + air_density +
                            air_molar_volume + RH + VPD + T. + un_H + un_co2_flux + mean_value + w.ts_cov"))
                            
mod2=lm(formula1, data = tbl)
anova(mod2)
summary(mod2)
formula2 = as.formula(paste("co2_flux ~ H + h2o_flux + T. + un_H + un_co2_flux"))
mod3=lm(formula2, data = tbl)
anova(mod3)
summary(mod3)
mod4 = lm(co2_flux ~ (H + h2o_flux + T. + un_H + un_co2_flux) ^2, data = tbl)
mod4
anova(mod4)
summary(mod4)
mod5 = lm (co2_flux ~ H + h2o_flux + T. + un_H + un_co2_flux + H:T. + H:un_H +
             h2o_flux:T. + T.:un_H + T.:un_co2_flux, data= tbl)                 
mod5      
anova(mod5)
summary(mod5)