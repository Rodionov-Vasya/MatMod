## Родионов Василий ПАЭ-121, вариант 11 -
#для региона 30 рассчить урожайность пшеницы в 2001 году,
#взяв для рассчета средние суммы активных температур за предыдущие 8 лет,
#с 12 блиижайших метеостанций но рассчитав колонку di самостоятельно,
#как долю месяца, когда среднедневные температуры были выше 8 градусов,
#но учитывая, что посев не может начаться раньше середины апреля,
#а вегетация составляет 3 месяца
# Регион 30 - Астраханская область, координаты: 46.3497, 48.0408
# проверяем рабочую дирректорию
rm(list = ls())
getwd()
#устанавливаем пакеты
#install.packages()
library(tidyverse)
library(rnoaa)
library(lubridate)

#Ввод констант для расчета урожайности
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)


#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25

# скачиваем список метеостанций
station_data = ghcnd_stations() 
write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")


# Зададим название вектора и координаты столицы региона:
Astrakhan = data.frame(id = "Astrakhan", latitude = 46.3497, longitude = 48.0408)

Astrakhan_around = meteo_nearby_stations(lat_lon_df = Astrakhan, station_data = station_data,
                                         limit = 12, var = "TAVG",
                                         year_min = 1993, year_max = 2000)

# Создадим таблицу
all_data = tibble()
for (i in 1:12)
{
  # Определим станцию из 12 ближайших:
  Astrakhan_id = Astrakhan_around[["Astrakhan"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = Astrakhan_id,
                          var="TAVG",
                          date_min="1993-01-01",
                          date_max="2000-12-31")
  # Занесем данные в таблицу, объединив их:
  all_data = bind_rows(all_data, data)
}

# Изменения в таблице сохранятся в векторе 
clean_data = all_data %>%
  # Группируем и находим cумму активных температур :
  mutate(year = year(date), month = month(date)) %>%
  mutate(tavg = tavg/10) %>%
  filter(tavg > 5) %>%
  group_by(year, month, id) %>%
  #Находим сумму активных температур, вычисляем di для температур выше 8
  summarize(summ = sum(tavg, na.rm=TRUE), di = length(tavg[tavg>8])/length(tavg) ) %>%
  group_by(month) %>%
  summarize(s = mean(summ, na.rm = TRUE), di= mean(di)) %>%
  #Нужны определенные месяцы - оставим только их
  filter(month>=4 & month <=7) %>%
  # Добавим колонки для расчета:
  mutate (a = af[4:7], b = bf[4:7]) %>%
  mutate (fert = ((a + b * 1.0 * s) * di * Kf) / (Qj * Lj * (100-Ej)) )

#Согласно расчету, урожайность пшеницы
Yield = sum(clean_data$fert); Yield

