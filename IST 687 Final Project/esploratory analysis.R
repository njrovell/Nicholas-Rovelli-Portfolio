final_data <- read.csv("Sample_Modeling_DF.csv")

test_data <- final_data
test_data <- na.omit(test_data)

library(tidyverse)

ggplot(test_data, aes(x = in.weather_file_city, 
                      y = out.total.energy_consumption)) + 
  geom_bar(stat = "identity", fill = "maroon") + 
  labs(title = "Energy Consumption by City", x = "City", 
       y = "Total Energy Consumption") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test_data, aes(x = in.weather_file_city, fill = in.heating_fuel)) + 
  geom_bar() + 
  labs(title = "Types of Fuels", x = "City") + 
  guides(color = guide_legend(title = "Fuel Type")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test_data, aes(x = as.factor(Day), 
                      y = out.total.energy_consumption)) + 
  geom_line() +
  labs(title = "Energy Consumed by Day of the Month", x = "Day of the Month", 
       y = "Energy Consumed") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(head_data, aes(x = as.factor(Hour), 
                      y = out.total.energy_consumption)) + 
  geom_line() + 
  labs(title = "Energy Consumption by Hour", x = "Hour", 
       y = "Energy Consumption") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test_data, aes(x = Dry.Bulb.Temperature...C., 
                      y = out.total.energy_consumption, 
                      color = in.tenure)) + 
  geom_point() + 
  labs(title = "Energy Consumed in Relation to Temperature and Tenure Status", 
       x = "Dry Bulb Temperatues", y = "Total Energy Consumption") + 
  guides(color = guide_legend(title = "Tenure Status")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test_data, aes(x = as.factor(in.geometry_stories), 
                      y = out.total.energy_consumption)) + 
  geom_boxplot() + 
  labs(title = "Energy Consumption in One and Two Story Buildings", 
       x = "Stories", y = "Energy Consumption") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test_data, aes(x = out.heating_cooling.energy_consumption, 
                      y = out.electrical_appliances.energy_consumption, 
                      color = Hour, 
                      size = Wind.Speed..m.s.)) + 
  geom_point(alpha = .7) + 
  labs(title = "Looking at Energy Consumption of Heating and Cooling and Eletrical Appliences During Hours of the Day and Wind Speed", 
       x = "Heating and Cooling", y = "Eletrical Appliences", fill = "Hours", 
       size = "Wind Speed") + 
  theme(plot.title = element_text(hjust = 0.5)) 