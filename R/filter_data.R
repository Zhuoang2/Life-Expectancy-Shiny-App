filter_gapminder = function(data, selected_continent, selected_country, years_range){
  library(dplyr)
  data %>% filter(continent == selected_continent,
                  country == selected_country,
                  year >= years_range[1],
                  year <= years_range[2])
}