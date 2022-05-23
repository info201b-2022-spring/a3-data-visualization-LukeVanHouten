library("dplyr")
library("ggplot2")
library("maps")
#  Only to make log plot y-axis look nice
library("scales")

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

max_black_jail_incarceration <- incarceration %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  select(year, state, county_name, black_jail_pop)

max_black_prison_incarceration <- incarceration %>%
  filter(black_prison_pop == max(black_prison_pop, na.rm = TRUE)) %>%
  select(year, state, county_name, black_prison_pop)

mean_white_incarceration <- incarceration %>% group_by(state) %>% 
  summarise(mean_white = mean(white_jail_pop, na.rm = TRUE), .groups = "drop")
mean_white_incarceration <- na.omit(mean_white_incarceration)
mean_white_incarceration <- mean_white_incarceration[-1, ]

mean_black_incarceration <- incarceration %>% group_by(state) %>% 
  summarise(mean_black = mean(black_jail_pop, na.rm = TRUE), .groups = "drop")
mean_black_incarceration <- na.omit(mean_black_incarceration)
mean_black_incarceration <- mean_black_incarceration[-1, ]

black_white_incarceration <- incarceration %>% group_by(year) %>% 
  summarise(black_jail = sum(black_jail_pop, na.rm = TRUE), white_jail = sum(white_jail_pop, na.rm = TRUE),
            black_prison = sum(black_prison_pop, na.rm = TRUE), white_prison = sum(white_prison_pop, na.rm = TRUE),
            black_pop = sum(black_pop_15to64, na.rm = TRUE), white_pop = sum(white_pop_15to64, na.rm = TRUE), .groups = "drop")
black_white_incarceration <- filter(black_white_incarceration, black_pop & black_prison > 0)
black_white_incarceration <- mutate(black_white_incarceration, 
                                    black_prison_per_capita = (black_prison / black_pop) * 1000, 
                                    white_prison_per_capita = (white_prison / white_pop) * 1000,
                                    black_jail_per_capita = (black_jail / black_pop) * 1000,
                                    white_jail_per_capita = (white_jail / white_pop) * 1000)

black_prison_incarceration_change <- tail(black_white_incarceration$black_prison, n=1) - black_white_incarceration$black_prison[1]

colors <- c("Black (Prison)" = "red", "Black (Jail)" = "darkred", "White (Prison)" = "blue",
            "White (Jail)" = "steelblue")
min_date <- min(black_white_incarceration$year)
max_date <- max(black_white_incarceration$year)
max_incarceration_per_capita <- ceiling(max(pmax(black_white_incarceration$black_prison_per_capita,
                                      black_white_incarceration$black_jail_per_capita,
                                      black_white_incarceration$white_prison_per_capita,
                                      black_white_incarceration$white_jail_per_capita)))

incarceration_plot <- ggplot(data = black_white_incarceration, aes(x = year)) +
  geom_line(aes(y = black_prison_per_capita, color = "Black (Prison)")) +
  geom_line(aes(y = white_prison_per_capita, color = "White (Prison)")) +
  geom_line(aes(y = black_jail_per_capita, color = "Black (Jail)"), linetype = "twodash") +
  geom_line(aes(y = white_jail_per_capita, color = "White (Jail)"), linetype = "twodash") +
  ggtitle("White vs. Black Incarceration 1990-2016, Per Capita") +
  labs(y = "Incarceration per 1000 people", x = "Years", color = "Race/Type") +
  scale_colour_manual(values = colors) +
  scale_y_continuous(breaks = seq(0, max_incarceration_per_capita, 2)) +
  scale_x_continuous(breaks = seq(min_date, max_date, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Legend help: https://community.rstudio.com/t/adding-manual-legend-to-ggplot2/41651
# Rotating years help: https://www.datanovia.com/en/blog/ggplot-axis-ticks-set-and-rotate-text-labels/

black_pop_incarceration_comparison_plot <- ggplot(data = black_white_incarceration, aes(x = year)) +
  geom_line(aes(y = black_prison + black_jail, color = "Total Black Incarceration")) +
  geom_line(aes(y = black_pop, color = "Black Population")) +
  ggtitle("Black Population vs. Incarceration (Logarithmic)") +
  labs(y = "Black Pop vs. Incarceration", x = "Years", color = "Pop vs. Incarceration") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_x_continuous(breaks = seq(min_date, max_date, 2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Logarithmic help: http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations#display-log-tick-marks

us_map <- map_data("state")
state_names <- state.name
state_names[51] <- "District of Columbia"
state_names <- sort(state_names)
black_white_incarceration_states <- incarceration %>% group_by(state) %>% 
  summarise(black_incarceration = sum(black_prison_pop, na.rm = TRUE) + sum(black_jail_pop, na.rm = TRUE))
black_white_incarceration_states <- select(black_white_incarceration_states, -state)
black_white_incarceration_states <- mutate(black_white_incarceration_states, region = tolower(state_names))
black_white_incarceration_states <- black_white_incarceration_states[, c(2, 1)]
# Sorry Alaska and Hawaii :( (You guys didn't have much data anyways)
black_white_incarceration_states <- black_white_incarceration_states[-c(2, 12), ]
merged_states <- inner_join(us_map, black_white_incarceration_states, by = "region" )
max_black_incarceration_states <- max(merged_states$black_incarceration)
merged_states <- merged_states[merged_states$black_incarceration > 1000, ]

incarceration_map <- ggplot() +
  geom_polygon(data = merged_states, aes(x = long, y = lat, group = group, fill = black_incarceration),
               color= "black", size = 0.2) +
  scale_fill_continuous(name = "Black Incarceration", low = "gold", high = "darkred",
                        limits = c(0, max_black_incarceration_states), 
                        breaks = seq(0, max_black_incarceration_states, 200000)) +
  labs(y = "", x = "(Blank states have insufficient data)") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("State Distribution of Total Black Incarceration 1990-2016") +
  coord_map()
  
# Map help: Reading and https://remiller1450.github.io/s230s19/Intro_maps.html
# Removing axis labels help: https://www.statology.org/remove-axis-labels-ggplot2/