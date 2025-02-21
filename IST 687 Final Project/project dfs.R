final_data <- read.csv("Sample_Modeling_DF.csv")
test_data <- final_data
test_data <- na.omit(test_data)
head_data <- head(test_data, n = 10000)
library(stringr)
library(tidyverse)

summary(final_data$Dry.Bulb.Temperature...C.)

padsummary(test_data)

summary(lm(head_data$out.total.energy_consumption ~ head_data$Dry.Bulb.Temperature...C., 
   head_data$Wind.Speed..m.s., head_data$in.sqft, 
   data = head_data))

head(test_data)

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

ggplot(test_data, aes(x = as.factor(Hour), 
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
  

numeric_columns <- sapply(head_data, is.numeric)

agg_data <- aggregate(. ~ bldg_id + Day , data = head_data, 
                      FUN = function(x) ifelse(is.numeric(x), 
                                               mean(x, na.rm = TRUE), 
                                               unique(x)))



#making the correlation plot
# loading corrplot
#library(corrplot)
# Creating correlation matrix for diamonds dataset
#D <- cor(diamonds[,c(1, 5,6,7,8,9)])
#coorplot(D, method = "circle")


table(test_data$in.cooling_setpoint_offset_magnitude)

test_data$in.cooling_setpoint_offset_magnitude <- 
  str_remove(test_data$in.cooling_setpoint_offset_magnitude, "F")
#removing all fareinheight signs from column

table(test_data$in.geometry_floor_area_bin)

if (test_data$in.geometry_floor_area_bin == "0-1499") {
  test_data$in.geometry_floor_area_bin = 1
} else if (test_data$in.geometry_floor_area_bin == "1500-2499"){
  test_data$in.geometry_floor_area_bin = 2
} else if (test_data$in.geometry_floor_area_bin == "2500-3999"){
  test_data$in.geometry_floor_area_bin = 3
} else {
  test_data$in.geometry_floor_area_bin = 4
}

test_data$in.geometry_floor_area_bin <- factor(test_data$in.geometry_floor_area_bin, levels = c("0-1499", "1500-2499", "2500-3999", "4000+"))

# Use if-else statements to assign numeric values
if ("0-1499" %in% test_data$in.geometry_floor_area_bin) {
  test_data$in.geometry_floor_area_bin = 1
} else if ("1500-2499" %in% test_data$in.geometry_floor_area_bin) {
  test_data$in.geometry_floor_area_bin = 2
} else if ("2500-3999" %in% test_data$in.geometry_floor_area_bin) {
  test_data$in.geometry_floor_area_bin = 3
} else {
  test_data$in.geometry_floor_area_bin = 4
}
#trying to make the different values a single number

table(test_data$in.heating_setpoint_offset_magnitude)

test_data$in.heating_setpoint_offset_magnitude <- 
  str_remove(test_data$in.heating_setpoint_offset_magnitude, "F")
#removing all Fahrenheit letters

table(test_data$in.hot_water_fixtures)

test_data$in.hot_water_fixtures <- 
  str_remove(test_data$in.hot_water_fixtures, "% Usage")
#creating just the number part of the percentages


electricity_data <- 
  data.frame(final_data$out.electricity.ceiling_fan.energy_consumption,
             final_data$out.electricity.clothes_dryer.energy_consumption, 
             final_data$out.electricity.clothes_washer.energy_consumption, 
             final_data$out.electricity.cooling_fans_pumps.energy_consumption, 
             final_data$out.electricity.cooling.energy_consumption, 
             final_data$out.electricity.dishwasher.energy_consumption, 
             final_data$out.electricity.freezer.energy_consumption, 
             final_data$out.electricity.heating_fans_pumps.energy_consumption, 
             final_data$out.electricity.heating_hp_bkup.energy_consumption, 
             final_data$out.electricity.heating.energy_consumption, 
             final_data$out.electricity.hot_tub_heater.energy_consumption, 
             final_data$out.electricity.hot_tub_pump.energy_consumption, 
             final_data$out.electricity.hot_water.energy_consumption, 
             final_data$out.electricity.lighting_exterior.energy_consumption, 
             final_data$out.electricity.lighting_garage.energy_consumption, 
             final_data$out.electricity.lighting_interior.energy_consumption, 
             final_data$out.electricity.mech_vent.energy_consumption, 
             final_data$out.electricity.plug_loads.energy_consumption, 
             final_data$out.electricity.pool_heater.energy_consumption, 
             final_data$out.electricity.pool_pump.energy_consumption, 
             final_data$out.electricity.pv.energy_consumption, 
             final_data$out.electricity.range_oven.energy_consumption, 
             final_data$out.electricity.refrigerator.energy_consumption, 
             final_data$out.electricity.well_pump.energy_consumption)

colnames(electricity_data) <- 
  gsub("final_data.", "", colnames(electricity_data))

gas_data <- 
  data.frame(final_data$out.fuel_oil.heating_hp_bkup.energy_consumption, 
             final_data$out.fuel_oil.heating.energy_consumption, 
             final_data$out.fuel_oil.hot_water.energy_consumption, 
             final_data$out.natural_gas.clothes_dryer.energy_consumption, 
             final_data$out.natural_gas.fireplace.energy_consumption, 
             final_data$out.natural_gas.grill.energy_consumption, 
             final_data$out.natural_gas.heating_hp_bkup.energy_consumption, 
             final_data$out.natural_gas.heating.energy_consumption, 
             final_data$out.natural_gas.hot_tub_heater.energy_consumption, 
             final_data$out.natural_gas.hot_water.energy_consumption, 
             final_data$out.natural_gas.lighting.energy_consumption, 
             final_data$out.natural_gas.pool_heater.energy_consumption, 
             final_data$out.natural_gas.range_oven.energy_consumption, 
             final_data$out.propane.clothes_dryer.energy_consumption, 
             final_data$out.propane.heating_hp_bkup.energy_consumption, 
             final_data$out.propane.heating.energy_consumption, 
             final_data$out.propane.hot_water.energy_consumption, 
             final_data$out.propane.range_oven.energy_consumption)

colnames(gas_data) <- 
  gsub("final_data.", "", colnames(gas_data))

weather_data <- 
  data.frame(final_data$Dry.Bulb.Temperature...C., 
             final_data$Relative.Humidity...., 
             final_data$Wind.Speed..m.s., 
             final_data$Wind.Direction..Deg., 
             final_data$Global.Horizontal.Radiation..W.m2., 
             final_data$Direct.Normal.Radiation..W.m2., 
             final_data$Diffuse.Horizontal.Radiation..W.m2., 
             final_data$in.weather_file_city, 
             final_data$in.weather_file_latitude, 
             final_data$in.weather_file_longitude, 
             final_data$bldg_id)

colnames(weather_data) <- 
  gsub("final_data.", "", colnames(weather_data))

building_data <- 
  data.frame(final_data$bldg_id, 
             final_data$in.bathroom_spot_vent_hour, 
             final_data$in.bedrooms, 
             final_data$in.building_america_climate_zone, 
             final_data$in.ceiling_fan, 
             final_data$in.city, final_data$in.clothes_dryer, 
             final_data$in.clothes_dryer, 
             final_data$in.clothes_washer, 
             final_data$in.cooking_range, 
             final_data$in.cooling_setpoint, 
             final_data$in.cooling_setpoint_has_offset, 
             final_data$in.cooling_setpoint_offset_magnitude, 
             final_data$in.cooling_setpoint_offset_period, 
             final_data$in.county, 
             final_data$in.county_and_puma, 
             final_data$in.dishwasher, 
             final_data$in.ducts, 
             final_data$in.federal_poverty_level, 
             final_data$in.geometry_attic_type, 
             final_data$in.geometry_floor_area, 
             final_data$in.geometry_floor_area_bin, 
             final_data$in.geometry_foundation_type, 
             final_data$in.geometry_garage, 
             final_data$in.geometry_stories, 
             final_data$in.geometry_wall_exterior_finish, 
             final_data$in.geometry_wall_type, 
             final_data$in.has_pv, 
             final_data$in.heating_fuel, 
             final_data$in.heating_setpoint, 
             final_data$in.heating_setpoint_offset_magnitude, 
             final_data$in.heating_setpoint_offset_period, 
             final_data$in.hot_water_fixtures, 
             final_data$in.hvac_cooling_efficiency, 
             final_data$in.hvac_cooling_partial_space_conditioning, 
             final_data$in.hvac_cooling_type, 
             final_data$in.hvac_has_ducts, 
             final_data$in.hvac_has_zonal_electric_heating, 
             final_data$in.hvac_heating_efficiency, 
             final_data$in.hvac_heating_type, 
             final_data$in.hvac_heating_type_and_fuel, 
             final_data$in.income, 
             final_data$in.income_recs_2015, 
             final_data$in.infiltration, 
             final_data$in.income_recs_2020, 
             final_data$in.insulation_ceiling, 
             final_data$in.insulation_floor, 
             final_data$in.insulation_foundation_wall, 
             final_data$in.insulation_rim_joist, 
             final_data$in.insulation_roof, 
             final_data$in.insulation_slab, 
             final_data$in.insulation_wall, 
             final_data$in.lighting, 
             final_data$in.misc_extra_refrigerator, 
             final_data$in.misc_freezer, 
             final_data$in.misc_gas_fireplace, 
             final_data$in.misc_gas_grill, 
             final_data$in.misc_gas_lighting, 
             final_data$in.misc_hot_tub_spa, 
             final_data$in.misc_pool, 
             final_data$in.misc_pool_heater, 
             final_data$in.misc_pool_pump,
             final_data$in.misc_well_pump, 
             final_data$in.occupants, 
             final_data$in.orientation,
             final_data$in.plug_load_diversity, 
             final_data$in.puma, 
             final_data$in.puma_metro_status, 
             final_data$in.pv_orientation, 
             final_data$in.pv_system_size, 
             final_data$in.range_spot_vent_hour, 
             final_data$in.reeds_balancing_area, 
             final_data$in.refrigerator, 
             final_data$in.roof_material, 
             final_data$in.sqft, 
             final_data$in.tenure, 
             final_data$in.usage_level, 
             final_data$in.vacancy_status, 
             final_data$in.vintage, 
             final_data$in.vintage_acs, 
             final_data$in.water_heater_efficiency, 
             final_data$in.water_heater_fuel, 
             final_data$in.weather_file_city, 
             final_data$in.weather_file_latitude, 
             final_data$in.weather_file_longitude, 
             final_data$in.window_areas, 
             final_data$in.windows, 
             final_data$upgrade.clothes_dryer, 
             final_data$upgrade.cooking_range, 
             final_data$upgrade.ducts, 
             final_data$upgrade.geometry_foundation_type, 
             final_data$upgrade.hvac_heating_efficiency, 
             final_data$upgrade.hvac_heating_type, 
             final_data$upgrade.infiltration_reduction,
             final_data$upgrade.insulation_ceiling, 
             final_data$upgrade.insulation_foundation_wall, 
             final_data$upgrade.insulation_roof, 
             final_data$upgrade.insulation_wall, 
             final_data$upgrade.water_heater_efficiency)

colnames(building_data) <- 
  gsub("final_data.", "", colnames(building_data))

bda <- weather_data %>% 
  group_by(weather_data$bldg_id) %>% 
  summarise_all(list(mean))

length(unique(final_data$bldg_id))
nrow(final_data)
final_data <- na.omit(final_data)
any(is.infinite(final_data$in.heating_fuel))
final_data$in.heating_fuel <- as.numeric(final_data$in.heating_fuel)
colnames(final_data)
sum(is.na(final_data))
summary(final_data)
lm(building_data$final_data.in.heating_fuel ~ ., building_data)


length(unique(final_data$out.electricity.clothes_dryer.energy_consumption))
unique_values <- lapply(final_data, unique)
print(unique_values)

count_unique_values <- function(df) {
  # Use the summarise_all function from dplyr to apply a function to all columns
  df %>% 
    summarise_all(~ n_distinct(.))
}

# Example usage:
# Assume df is your dataframe
unique_counts <- count_unique_values(final_columns)
print(unique_counts)

final_columns <- final_data[, -which(unique_counts == 1)]

table(final_columns$in.vacancy_status)

cor(na.omit(as.numeric(final_columns$in.insulation_roof)), 
    na.omit(as.numeric(final_columns$in.insulation_slab)))


weather_df <- read.csv("weather_df.csv")
colnames(weather_df) <- c("date_time", "dry_bulb_temp", "relative_humidity", 
                          "wind_speed", "wind_direction", 
                          "global_horz_radiation", "direct_norm_radiation", 
                          "diffuse_horz_radiation", "county")

final_data