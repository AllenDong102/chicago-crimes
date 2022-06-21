# install.packages("data.table")
library(data.table)
library(dplyr) 
full_data <- fread("/Users/liuyitong/Desktop/chicago_crimes.csv")
# install.packages("ggalluvial")
library(ggalluvial)



# ALLUVIAL PLOT LOCATION & TYPE

data1 <- full_data[1:1000000]
names(data1)[names(data1) == 'Primary Type'] <- 'type'
names(data1)[names(data1) == 'Location Description'] <- 'location'

library(stringr)
data1$location <- str_replace_all(data1$location,
                                  "PARKING LOT/GARAGE\\(NON\\.RESID\\.\\)", 
                                  "PARKING LOT")

top_type <- data1 %>% 
  count(type) %>% 
  top_n(8, n) %>% 
  pull(type)

top_location <- data1 %>% 
  filter(type %in% top_type) %>% 
  count(location) %>% 
  top_n(6, n) %>% 
  pull(location)

filtered_data1 <- data1 %>% 
  filter(type %in% top_type & location %in% top_location)

grouped_data1 <- filtered_data1 %>% group_by(type, location) %>% summarise(Freq = n())  

personal_theme = theme(plot.title = element_text(hjust = 0.5))

ggplot(data = grouped_data1,
       aes(axis1 = type, axis2 = location, y = Freq)) +
  geom_alluvium(aes(fill = location)) +
  geom_stratum() + # could add 'aes(fill = location)' inside bracket
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c('Type of Crime', 'Location of Crime'),
                   expand = c(0.20, 0.05)) +
  scale_fill_viridis_d() +
  theme_void() +
  theme(legend.position = "none") + 
  ggtitle("Summary of Crime Cases in Chicago \nMapping Major Types of Crime to Major Locations") +
  personal_theme




# Stacked Bar Plot - Total Arrested by Type

data6 <- full_data
names(data6)[names(data6) == 'Primary Type'] <- 'type'

top_type <- data6 %>% 
  count(type) %>% 
  top_n(10, n) %>% 
  pull(type)

grouped_data6 <- data6 %>% 
  filter(type %in% top_type) %>% 
  group_by(type, Arrest) %>%
  summarise(Freq = n())  

ggplot(data = grouped_data6, aes(fill=Arrest, y=Freq, x=reorder(type, Freq, sum))) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Crime Arrest By Primary Type") +
  scale_fill_viridis_d() + 
  labs(x = 'Primary Type of Crime', y = 'Frequency of Crime') +
  theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1))




# Proportion Bar Plot - Percentage Arrested by Year

data5 <- full_data %>% 
  group_by(Year, Arrest) %>%
  # filter(Year < 2022) %>% 
  summarise(Freq = n())  

ggplot(data = data5, aes(fill=Arrest, y=Freq, x=Year)) + 
  geom_bar(position="fill", stat="identity") + 
  ggtitle("Proportion of Crime Arrested By Year") +
  scale_y_continuous(breaks = round(seq(0, 1, by = 0.10),1)) +
  scale_fill_viridis_d()




# Proportion Bar Plot - Proportion arrested by hour of day

convert_time <- function(date) {
  hour <- as.numeric(str_sub(date, 12, 13))
  is.am <- (str_sub(date, -2, -1) == 'AM') 
  ifelse(hour == 12, ifelse(is.am, 0, 12), ifelse(is.am, hour, hour + 12))
}

data4 <- full_data %>%
  mutate(Time = convert_time(Date)) %>%
  group_by(Time, Arrest) %>% 
  summarise(Freq = n())  

ggplot(data = data4, aes(fill=Arrest, y=Freq, x=Time)) + 
  geom_bar(position="fill", stat="identity") + 
  ggtitle("Proportion of Crime Arrested By Hour of the Day") +
  scale_x_continuous(breaks = round(seq(0, 23, by = 1),1)) +
  scale_fill_viridis_d() +
  coord_polar() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())






