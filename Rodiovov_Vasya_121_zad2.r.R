#Родионов Василий, ПАЭ-121, Вариант 11
#Задание 2. Создайте модель множественной линейной регрессии 
#ночных потоков паров воды за период 2013 года по данным измерений 
#методом турбулентной пульсации

#проверка рабочей директории
getwd()
#Работа с библиотеками и установкой пакетов
#install.packages("tidyverse")("stringr") ("dplyr") ("ggplot2")

library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")

#считываем файл
eddypro = read.csv("eddypro.csv", skip = 1, na = c ("","NA","-9999","-9999.0"), comment = c("["))

#готовим данные
# Удаляем ненужную пустую первую строку
eddypro = eddypro [-1,]

# Удаляем ненужный пустой столбец "roll"
eddypro = select(eddypro,-(roll))

# Преобразуем в факторы (factor) столбы типа char(символ)
eddypro = eddypro %>% mutate_if(is.character,factor)

#Заменим специальные символы в названии стобцов на допустимые для переменных имена
names(eddypro) = names(eddypro) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]","_")

#Возвратим столбцы таблицы в виде векторов для проверки,посмотрим
glimpse(eddypro)

#Удалим строки в которых содержится NA
eddypro = drop_na(eddypro)


# Отфильтруем данные по заданию только за ночное время
eddypro = filter(eddypro, daytime ==FALSE)

# Получим таблицу, состоящую только из чисел для работы с ней
eddypro_numeric = eddypro[,sapply(eddypro, is.numeric)]

# Получим таблицу, содержащую остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro, is.numeric)]

# Создадим обучающую и тестирующую непересекающиеся выборки с помощью базового функционала
row_numbers = 1:length(eddypro_numeric$h2o_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$h2o_flux)*.7))
test = row_numbers[-teach]

#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]

#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]

#Модель 1 по обучающей выборке
mod1 = lm(h2o_flux ~ (.), data = testing_tbl)
#Информация 
summary(mod1)
#Коэффициенты 
coef(mod1)
#Остатки
resid(mod1)
#Доверительный интервал
confint(mod1)
#Дисперсионный анализ модели
anova(mod1)
#Графическое представление модели
plot(mod1)
#Модель 2
mod2 = lm(DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE+
            rand_err_LE+co2_flux+qc_co2_flux+rand_err_co2_flux+rand_err_h2o_flux+
            H_strg+co2_v.adv+h2o_v.adv+co2_molar_density+co2_mole_fraction+co2_mixing_ratio+
            h2o_molar_density+h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+
            air_temperature+air_pressure+air_density+air_heat_capacity+air_molar_volume+
            water_vapor_density+es+specific_humidity+RH+VPD+u_unrot+v_unrot+w_unrot+u_rot+v_rot+
            w_rot+max_speed+wind_dir+pitch+u.+TKE+X.z.d..L+bowen_ratio+T.+x_peak+x_offset+x_10.+
            x_30.+x_50.+x_70.+x_90.+un_Tau+Tau_scf+un_H+H_scf+un_LE+LE_scf+un_co2_flux+un_h2o_flux+
            w_spikes+ts_spikes+co2_var+w.ts_cov+h2o+co2.1, data = teaching_tbl)

#Информация о модели
summary(mod2)
coef(mod2)
resid(mod2)
confint(mod2)
anova(mod2)
plot(mod2)
#Модель 3
mod3 = lm(DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE+
            rand_err_LE+co2_flux+qc_co2_flux+rand_err_co2_flux+rand_err_h2o_flux+
            H_strg+co2_v.adv+h2o_v.adv+co2_molar_density+co2_mixing_ratio+
            h2o_molar_density+h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+
            air_temperature+air_pressure+air_density+air_heat_capacity+air_molar_volume+
            water_vapor_density+es+specific_humidity+RH+VPD+u_unrot+v_unrot+w_unrot+u_rot+v_rot+
            w_rot+max_speed+wind_dir+pitch+u.+TKE+bowen_ratio+T.+x_peak+x_10.+
            x_30.+x_50.+x_70.+x_90.+un_Tau+Tau_scf+un_H+H_scf+un_LE+LE_scf+un_co2_flux+un_h2o_flux+
            w_spikes+ts_spikes+co2_var+w.ts_cov+h2o+co2.1, data = teaching_tbl)

#Информация о модели 
summary(mod3)
coef(mod3)
resid(mod3)
confint(mod3)
anova(mod3)
plot(mod3)

#Модель 4
mod4 = lm(DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE+
            rand_err_LE+co2_flux+qc_co2_flux+rand_err_co2_flux+rand_err_h2o_flux+
            H_strg+co2_v.adv+h2o_v.adv+co2_molar_density+co2_mixing_ratio+
            h2o_molar_density+h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+
            air_temperature+air_pressure+air_density+air_heat_capacity+air_molar_volume+
            water_vapor_density+es+specific_humidity+RH+VPD+u_unrot+v_unrot+w_unrot+u_rot+v_rot+
            w_rot+max_speed+wind_dir+pitch+u.+TKE+bowen_ratio+T.+x_peak+x_10.+
            x_30.+x_50.+x_70.+x_90.+un_Tau+Tau_scf+un_H+H_scf+un_LE+LE_scf+un_co2_flux+un_h2o_flux+
            w_spikes+ts_spikes+co2_var+w.ts_cov+h2o, data = teaching_tbl)

#Информация о модели 
summary(mod4)
coef(mod4)
resid(mod4)
confint(mod4)
anova(mod4)
plot(mod4)

#Модель 5
mod5 = lm(DOY+file_records+Tau+qc_Tau+rand_err_Tau+H+qc_H+rand_err_H+LE+qc_LE+
            rand_err_LE+co2_flux+qc_co2_flux+rand_err_co2_flux+rand_err_h2o_flux+
            H_strg+co2_v.adv+h2o_v.adv+co2_molar_density+co2_mixing_ratio+
            h2o_molar_density+h2o_mole_fraction+h2o_mixing_ratio+h2o_time_lag+sonic_temperature+
            air_temperature+air_pressure+air_density+air_heat_capacity+air_molar_volume+
            water_vapor_density+es+specific_humidity+RH+VPD+u_unrot+v_unrot+w_unrot+u_rot+
            w_rot+max_speed+wind_dir+pitch+u.+TKE+bowen_ratio+x_peak+x_10.+
            x_30.+x_50.+x_70.+x_90.+un_Tau+Tau_scf+un_H+H_scf+un_LE+LE_scf+un_co2_flux+un_h2o_flux+
            w_spikes+ts_spikes+co2_var+w.ts_cov+h2o, data = teaching_tbl)

#Информация о модели
summary(mod5)
coef(mod5)
resid(mod5)
confint(mod5)
anova(mod5)
plot(mod5)
