# Машенцев Илья – для региона 26 рассчитайте урожайность пшеницы в 2015 году, 
# взяв для рассчета средние суммы активных температур за предыдущие 11 лет, 
# с 30 ближайших метеостанций но убирая из рассчета активных температур дни с температурой выше 25 градусов
# Ставропольский край - 45.046371, 41.969386

# Подключим библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры
lat = 45.046371; lon = 41.969386;

# station_data = ghcnd_stations()
# write.csv(station_data, "station_data.csv")

station_data = read.csv("station_data.csv")
# Получим список метеостанций
stavropol = data.frame(id = "STAVROPOL", latitude = lat,  longitude = lon)
#найдем станции, соответствующие критериям
stavropol_around = meteo_nearby_stations(lat_lon_df = stavropol, station_data = station_data,
                                        limit = 30, var = "TAVG", 
                                        year_min = 2003, year_max = 2014)
# Создадим таблицу
all_data = tibble()
for (i in 1:30)
{
  # Определим станцию
  stavropol_id = stavropol_around[["STAVROPOL"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = stavropol_id,
                          var="TAVG",
                          date_min="2003-01-01",
                          date_max="2014-12-31")
  #объединим данные в таблице
  all_data = bind_rows(all_data, data)
}

# Изменения в таблице сохранятся в векторе clean_data.
clean_data =   all_data %>% 
  # Группируем и находим  cумму активных температур :
  mutate(year = year(date), month = month(date)) %>%
  mutate(tavg = tavg/10) %>%
  #температура активная, но не выше 25
  filter(tavg > 10 && tavg <=25) %>%
  group_by(year, month, id) %>%
  summarize(summ = sum(tavg, na.rm=TRUE)) %>%
  group_by(month) %>%
  summarize(s = mean(summ, na.rm = TRUE)) %>%
  # Добавим колонки для расчета:
  mutate (a = af, b = bf, d = df) %>%
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )

#Согласно расчету, урожайность пшеницы в Ставрополье в 2015 году составила (ц/га):
Yield = sum(clean_data$fert); Yield