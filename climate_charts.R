library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

head(temp_carbon)


#temp_carbon charts
p<-temp_carbon %>%filter(!is.na(temp_anomaly))%>%ggplot(aes(year,temp_anomaly))+
  geom_point()
p <- p + geom_hline(aes(yintercept = 0), col = "blue")
p<-p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p <- p + geom_line(aes(year,land_anomaly), col = "Red")
p <- p + geom_line(aes(year,ocean_anomaly), col = "blue")
p

#greenhouse gases

head(greenhouse_gases)

greenhouse_gases %>%
  ggplot(aes(year,concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850), col = "blue")+
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


#carbon emmissions
temp_carbon %>%filter(!is.na(carbon_emissions))%>%ggplot(aes(year,carbon_emissions))+
  geom_point()

head(historic_co2)
#cco2
co2_time<-historic_co2 %>%filter(!is.na(co2))%>%ggplot(aes(year,co2,color=source))+
  geom_line()

#cco2 again
historic_co2 %>%filter(!is.na(co2))%>%filter(year>= -800000 & year<=-775000)%>%ggplot(aes(year,co2,color=source))+
  geom_line()

#cco2 again
historic_co2 %>%filter(!is.na(co2))%>%filter(year>= -375000 & year<=-330000)%>%ggplot(aes(year,co2,color=source))+
  geom_line()

#cco2 again
historic_co2 %>%filter(!is.na(co2))%>%filter(year>= -140000 & year<=-120000)%>%ggplot(aes(year,co2,color=source))+
  geom_line()

#cco2 again
historic_co2 %>%filter(!is.na(co2))%>%filter(year>= -3000 & year<=2018)%>%ggplot(aes(year,co2,color=source))+
  geom_line()

