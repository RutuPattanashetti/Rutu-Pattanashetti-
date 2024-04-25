install.packages("tidyverse")
install.packages("plotly")
library(tidyverse)
library(plotly)


unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

unicef_join <- full_join(unicef_indicator_1,unicef_metadata, by = join_by(country , time_period == year))
unicef_join <- full_join(unicef_indicator_1,unicef_metadata , by = c("country","time_period"= "year"))

unicef_join <- unicef_indicator_1 %>%
  full_join(unicef_metadata,by = c("country","time_period"= "year"))

map_world <- map_data("world")

# map 1
map_data_join <- full_join(unicef_join,map_world , by = c ("country" = "region"))

# Plotting the map
ggplot(data = map_data_join, aes(x = long, y = lat, group = group, fill = obs_value)) +
  geom_polygon() +
  scale_fill_viridis_c(option = "C") +  # Color scale
  labs(title = "Global Distribution of Teenage Marriage", fill = "Indicator Value") +
  theme_minimal()
ggplotly()

`#Line chart

filtered_data <- unicef_join %>%
  filter(time_period >= 2012, time_period <= 2020)


 ggplot(filtered_data, aes(x = obs_value, y = `GDP per capita`, group = country)) +
  geom_line() +
  theme_minimal() +
  labs(title = "GDP per Capita Over Time by Country",
       x = "obs_value",
       y = "GDP per Capita")
 
 #scatter plot
 
 # Filter data for the timeframe 2012 to 2020
 filtered_data <- unicef_join %>%
   filter(time_period >= 2012, time_period <= 2020)
 
 # Summarize to find the maximum observed value for each country
 top_countries <- filtered_data %>%
   group_by(country) %>%
   summarise(max_value = max(obs_value, na.rm = TRUE)) %>%
   arrange(desc(max_value)) %>%
   slice_max(n = 10, order_by = max_value)
 
 # Plotting the results
 ggplot(top_countries, aes(x = reorder(country, max_value), y = max_value, fill = country)) +
   geom_col() +
   labs(title = "Top 10 Countries by Maximum Teenage Boys Marriage Raye (2012-2020)",
        x = "Country",
        y = "Maximum % of Teenage Boys Marriage") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
 ggplotly()
 
 
 
 
 # Filter data between 2012 and 2020
 filtered_data <- unicef_join %>%
   filter(time_period >= 2012, time_period <= 2020)
 
 # Compute max obs_value and mean GDP per capita for each country within the period
 country_summaries <- filtered_data %>%
   group_by(country) %>%
   summarise(max_obs_value = max(obs_value, na.rm = TRUE),
             avg_gdp_per_capita = mean(`GDP per Capita`, na.rm = TRUE)) %>%
   ungroup()
 
 # Identify top 10 countries based on max_obs_value
 top_countries <- country_summaries %>%
   slice_max(order_by = max_obs_value, n = 10)
 
 # Filter the original data to include only top countries and relevant time period
 top_countries_data <- filtered_data %>%
   filter(country %in% top_countries$country)
 
#bar chart
 
 marriage_bar_chart <- unicef_join %>%
   mutate(value_bin = cut(obs_value, breaks = seq(0, max(ceiling(obs_value)), by = 2), include.lowest = TRUE)) %>%
   count(value_bin) %>%
   ggplot(aes(x = value_bin, y = n, fill = value_bin)) +
   geom_col(show.legend = FALSE) +  # Hide legend for clarity
   scale_x_discrete(drop = FALSE) +  # Ensure all bins are shown even if some are empty
   theme_minimal() +
   labs(title = "Teenage Boys Marriage Rates Distribution",
        x = "Teenage Marriage Rate (%)",
        y = "Number of Countries") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x labels for readability
 ggplotly(marriage_bar_chart)
 
 #Trends in chart
 trend_chart <- ggplot(unicef_join, aes(x = time_period, y = obs_value)) +
   geom_line(stat = "summary", fun.y = mean) +
   theme_minimal() +
   labs(title = "Trends in Teenage Boys Marriage Rates",
        x = "Time Period",
        y = "Average Teenage Marriage Rate (%)")
 ggplotly(trend_chart)
 