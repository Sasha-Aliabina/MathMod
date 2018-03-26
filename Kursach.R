library("tidyverse")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr") 
library("ggplot2")
library("dplyr") # включаем библиотеки
tbl = read_csv("eddypro.csv",  skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) # читаем файл, вносим изменения
tbl = tbl[-1,] # Удаляем первую строку
tbl
glimpse(tbl) # Информация по столбцам
tbl = select(tbl, -(roll)) # Удаляем ненужные переменные
tbl = tbl [ ,c(-6, -7, -9, -10, -12, -13, -15, -16, -18, -19, -21, -22, -77:-130)] # Удаляем ненужные переменные
tbl 
tbl = tbl %>% mutate_if(is.character, factor) # Преобразуем повторяющиеся значения в факторы
names(tbl) = names(tbl) %>% # Исправляем проблему со знаками в переменных
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
glimpse(tbl) # Информация по столбцам
tbl=tbl[tbl$DOY > 222 & tbl$DOY < 349,] # Выбираем осеннее время
tbl
tbl=tbl[tbl$daytime == TRUE,] # Выбираем дневное время 
tbl
sapply(tbl,is.numeric) # Выбираем все переменные типа numeric, чтобы перейти к корелляционному анализу
tbl_numeric = tbl[,sapply(tbl,is.numeric)] # Оставляем только числовые данные
tbl_numeric
cor_tbl = cor(tbl_numeric) # Корреляционный анализ
cor_tbl
cor_tbl = cor(na.omit(tbl_numeric)) # Удаляем строки со значением NA
cor_tbl
cor_tbl = cor(na.omit(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) # Преобразуем матрицу в таблицу
cor_tbl
vars = row.names(cor_tbl)[cor_tbl$co2_flux^2 > .7] %>% na.exclude # Берем только те имена переменных для которых значения коэффициента детерминации было больше 0,7
vars
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep = "")) # Собираем все переменные из вектора с именнами переменных в одну формулу
formula
row_numbers = 1:length(tbl$date) # Делаем непересекающиеся подвыборки
teach = sample(row_numbers, floor(length(tbl$date)*.7))
test = row_numbers[-teach]
teaching_tbl_unq = tbl[teach,]
testing_tbl_unq = tbl[test,]
mod1 = lm(co2_flux~Tau+H+LE+co2_flux+h2o_flux+H_strg+co2_v_minus_adv+h2o_v_minus_adv +co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature+air_temperature+air_pressure+air_density+air_heat_capacity+air_molar_volume+water_vapor_density+e+es+specific_humidity+RH+VPD+Tdew+u_unrot+v_unrot+w_unrot+u_rot+v_rot+w_rot+wind_speed+max_speed+yaw+pitch+u*+TKE+L+_z_minus_d__div_L+bowen_ratio+x_peak+x_offset+x_10%+x_30%+x_50%+x_70%+x_90+un_Tau, data = tbl)  # Линейная модель
summary(mod1) # Общая информация по модели
predict(mod1) # Рассчитываем значение интересующей переменной
resid(mod1) # Остатки или отклонения
coef(mod1) # Получаем коэфициенты модели
anova(mod1) # Проверяем на сколько данные стаистически достоверны
plot(mod1) # Графики
mod2 = lm(co2_flux~H+co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+air_density+u_unrot+v_unrot+w_unrot+yaw+pitch, data=tbl)
summary(mod2) # Общая информация по модели
predict(mod2) # Рассчитываем значение интересующей переменной
resid(mod2) # Остатки или отклонения
coef(mod2) # Получаем коэфициенты модели
anova(mod2) # Проверяем на сколько данные стаистически достоверны
plot(mod2) # Графики
mod3 = lm(co2_flux~H+co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+air_density+w_unrot+yaw+pitch, data=tbl)
summary(mod3) # Общая информация по модели
predict(mod3) # Рассчитываем значение интересующей переменной
resid(mod3) # Остатки или отклонения
coef(mod3) # Получаем коэфициенты модели
anova(mod3) # Проверяем на сколько данные стаистически достоверны
plot(mod3) # Графики
mod4 = lm(co2_flux~H+co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+air_density+yaw+pitch, data=tbl)
summary(mod4) # Общая информация по модели
predict(mod4) # Рассчитываем значение интересующей переменной
resid(mod4) # Остатки или отклонения
coef(mod4) # Получаем коэфициенты модели
anova(mod4) # Проверяем на сколько данные стаистически достоверны
plot(mod4) # Графики
