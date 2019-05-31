# ������� 1
# ������� ������ � ��� ������� 25 (���������� ����, ������� - �����������)
# ����������� ����������� ������� � 2003 ����, ���� ��� �������� ������� ����� �������� ���������� �� ���������� 12 ���, 
# � ������������ �� ���������� �� 90 �� 180 ��
# ���������� �������: 43.117908, 131.881738

# ��������� ����������:
library(tidyverse)
library(rnoaa)
library(lubridate)

# �������� ������� � ������� ��� �������:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  ����������� ������������� ���
Qj = 1600 # ������������ ������ ��������
Lj = 2.2 #  ����� ������ �������� � �������� ���������
Ej = 25 #   ����������� ��������� ��������

#station_data = ghcnd_stations()
#write.csv(station_data, "station_data.csv")

station_data = read.csv("station_data.csv")
# ������� ������ ������������
vladivostok = data.frame(id = "VLADIVOSTOK", latitude = 43.117908,  longitude = 131.881738)
#������ �������, ��������������� ���������
vladivostok_around = meteo_nearby_stations(lat_lon_df = vladivostok, station_data = station_data,
                                         radius = 180, var = "TAVG", 
                                         year_min = 1991, year_max = 2003)
#�������� �������
all_data = tibble()
#������� � ������� ������ ��� �������, ������� �� �������, ������������� �� ���������� 92 ��
for (i in 4:length(vladivostok_around))
{
  # ��������� �������:
  vladivostok_id = vladivostok_around[["VLADIVOSTOK"]][["id"]][i]
  # �������� ������ ��� �������:
  data = meteo_tidy_ghcnd(stationid = vladivostok_id,
                          var="TAVG",
                          date_min="1991-01-01",
                          date_max="2003-12-31")
  #��������� ������ � �������
  all_data = bind_rows(all_data, data %>%
                         #������� ������� ��� ����������� �� ���� � ������
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         #������ ��������� ������� �������� ����������� �� ������ �� ������ ��� ��� �������
                         summarise (tavg = sum(tavg[tavg>50], na.rm = TRUE)/10 )
  )
}

# ��������� � ������� ���������� � ������� clean_data.
clean_data = all_data %>%
  # ������� ������� month ��� ����������� ������:
  group_by(month) %>%
  # ������ �������� d � c���� �������� ��������� ��� ������ �������:
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  #������� ������ �� ������� � ����������� d
  # ������� ������� ��� �������:
  mutate (a = af, b = bf, d = df) %>%
  # ���������� ����������� ��� ������� ������:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) )
#�������� �������, ����������� ������� � ���������� ���� � 2003 ���� ��������� (�/��):
Yield = sum(clean_data$fert); Yield
