#Preprocessing part
data <- read.csv("C:/Users/rkt_1/Desktop/EDA final/hotel_bookings.csv")
#checking first few rows of the data
head(data)

# Check for missing values
colSums(is.na(data))

value=0
# Replace missing values with the mean
data$children[is.na(data$children)] <- value

#checking the type of data
str(data)

num_rows <- nrow(data)
num_cols <- ncol(data)

# Print the number of rows and columns
print(paste("Number of rows:", nrow(data)))
print(paste("Number of columns:", ncol(data)))
#---------------------------------------------------------------------
#Analysis part
#how many people came from which country
library(ggplot2)
library(dplyr)
library(viridis)

# Filter the dataset for is_cancelled = 0 and count the number of people by country
country_data <- data %>%
  filter(is_canceled == 0) %>%
  count(country)

# Sort the data by the count in descending order
country_data <- country_data[order(-country_data$n), ]

# Calculate percentages
country_data <- country_data %>%
  mutate(percentage = (n / sum(n)) * 100)

# Create a pie chart with country names and percentages
pie_chart <- ggplot(country_data, aes(x = "", y = n, fill = country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of People with is_cancelled = 0 by Country") +
  scale_fill_manual(values = viridis(nrow(country_data)), name = "Country") +
  theme_minimal()
top_n_labels <- 10  # Specify the number of top labels to display

pie_chart +
  geom_text(data = country_data[1:top_n_labels, ],  # Filter the top N countries
            aes(label = paste(country, " ", sprintf("%.1f%%", percentage))),
            position = position_stack(vjust = 0.4))

#-------------------------------
library(rworldmap)
library(RColorBrewer)

# Filter the dataset for is_cancelled = 0 and count the number of people by country
country_data <- data %>%
  filter(is_canceled == 0) %>%
  count(country)

# Create a world map
map <- getMap()

# Merge country data with map data
merged_data <- joinCountryData2Map(country_data,
                                   joinCode = "ISO3",
                                   nameJoinColumn = "country")

# Define color palette
num_colors <- length(unique(country_data$n))
color_palette <- brewer.pal(num_colors, "YlOrRd")

# Create a color scale for the map
color_scale <- data.frame(n = unique(country_data$n),
                          color = color_palette[1:num_colors])

# Plot the world map with color-coded countries
mapCountryData(merged_data, nameColumnToPlot = "n", catMethod = "fixedWidth",
               colourPalette = color_scale$color)

# Add a legend
addMapLegend(position = "bottomleft", legendWidth = 0.5, legendLabels = color_scale$n,
             col = color_scale$color, title = "Number of Guests")
#-----------------------------------------------------------------------
library(dplyr)

# Filter the data for Resort Hotel
resort_sum <- data %>%
  filter(hotel == "Resort Hotel") %>%
  summarise(sum_adr = sum(adr),
            sum_adults = sum(adults),
            sum_children = sum(children))

# Filter the data for City Hotel
city_sum <- data %>%
  filter(hotel == "City Hotel") %>%
  summarise(sum_adr = sum(adr),
            sum_adults = sum(adults),
            sum_children = sum(children))

# Print the average prices for each hotel type
cat("Average Price for Resort Hotel:", mean((resort_sum$sum_adr)/(resort_sum$sum_adults + resort_sum$sum_children)), "\n")
cat("Average Price for City Hotel:", mean((city_sum$sum_adr)/(city_sum$sum_adults + city_sum$sum_children)), "\n")

library(ggplot2)
library(dplyr)

# Normalize price per night (adr_pp)
data$adr_pp <- data$adr / (data$adults + data$children)

# Filter for actual guests
data_guests <- data %>%
  filter(is_canceled == 0)

# Sort room prices by reserved_room_type
room_prices <- data_guests %>%
  select(hotel, reserved_room_type, adr_pp) %>%
  arrange(reserved_room_type)

# Boxplot
ggplot(room_prices, aes(x = reserved_room_type, y = adr_pp)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  facet_wrap(~ hotel) +
  labs(title = "Price of room for both types of hotel for per night and per person",
       x = "Room type",
       y = "Price [EUR]") +
  ylim(0, 160) +
  theme_minimal()

#--------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
# Normalize price per night (adr_pp)
data$adr_pp <- data$adr / (data$adults + data$children)
# Filter for actual guests and select relevant columns
room_prices_monthly <- data %>%
  filter(is_canceled == 0) %>%
  select(hotel, arrival_date_month, adr_pp) %>%
  arrange(arrival_date_month)

# Define the order of months
ordered_months <- c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November", "December")

# Convert arrival_date_month to an ordered factor with the specified order
room_prices_monthly$arrival_date_month <- factor(room_prices_monthly$arrival_date_month,
                                                 levels = ordered_months,
                                                 ordered = TRUE)

# Bar chart comparing both hotel types
ggplot(room_prices_monthly, aes(x = arrival_date_month, y = adr_pp, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Room price per night and person over the year",
       x = "Month",
       y = "Price [EUR]") +
  scale_fill_manual(values = c("blue", "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#-----------------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Filter the data for Resort Hotel and City Hotel
hotel_data <- data %>%
  filter(hotel %in% c("Resort Hotel", "City Hotel"))

# Group the data by hotel and arrival month, and count the number of occurrences
monthly_counts <- hotel_data %>%
  group_by(hotel, arrival_date_month) %>%
  summarise(count = n())

# Order the months
ordered_months <- c("January", "February", "March", "April", "May", "June",
                    "July", "August", "September", "October", "November", "December")
monthly_counts$arrival_date_month <- factor(monthly_counts$arrival_date_month, levels = ordered_months)

# Line chart to compare busiest months for both hotel types
ggplot(monthly_counts, aes(x = arrival_date_month, y = count, color = hotel, group = hotel)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  labs(title = "Busiest Months for Resort Hotel and City Hotel",
       x = "Month",
       y = "Number of Bookings") +
  scale_color_manual(values = c("blue", "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#-----------------------------------------------------------------


library(dplyr)

# Create a DataFrame with the relevant data for Resort Hotel
rh <- data %>%
  filter(hotel == "Resort Hotel") %>%
  mutate(total_nights = stays_in_weekend_nights + stays_in_week_nights)

num_nights_res <- rh %>%
  count(total_nights) %>%
  pull(total_nights)

num_bookings_res <- rh %>%
  count(total_nights) %>%
  pull(n)

rel_bookings_res <- rh %>%
  count(total_nights) %>%
  mutate(rel_num_bookings = n / sum(n) * 100)

res_nights <- data.frame(hotel = "Resort hotel",
                         num_nights = num_nights_res,
                         rel_num_bookings = rel_bookings_res$rel_num_bookings)

# Create a DataFrame with the relevant data for City Hotel
ch <- data %>%
  filter(hotel == "City Hotel") %>%
  mutate(total_nights = stays_in_weekend_nights + stays_in_week_nights)

num_nights_cty <- ch %>%
  count(total_nights) %>%
  pull(total_nights)

num_bookings_cty <- ch %>%
  count(total_nights) %>%
  pull(n)

rel_bookings_cty <- ch %>%
  count(total_nights) %>%
  mutate(rel_num_bookings = n / sum(n) * 100)

cty_nights <- data.frame(hotel = "City hotel",
                         num_nights = num_nights_cty,
                         rel_num_bookings = rel_bookings_cty$rel_num_bookings)

# Combine the data for both hotels
nights_data <- bind_rows(res_nights, cty_nights)


# Create a bar plot
ggplot(nights_data, aes(x = num_nights, y = rel_num_bookings, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Length of Stay",
       x = "Number of Nights",
       y = "Guests [%]") +
  scale_fill_manual(values = c("orange", "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_cartesian(xlim = c(0, 22))+
  scale_x_continuous(breaks = seq(0, 22, 1))
#----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

# Calculate the count and percentage of bookings per market segment
segment_counts <- data %>%
  count(market_segment) %>%
  mutate(percentage = n / sum(n) * 100)

# Sort the segments by count in descending order
segment_counts <- segment_counts %>%
  arrange(desc(n))

# Create a pie chart
ggplot(segment_counts, aes(x = "", y = n, fill = market_segment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Booking Distribution by Market Segment",
       fill = "Market Segment",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(market_segment, "\n", round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "white",
              size = 3,
              check_overlap = TRUE,
              segment.colour = NA)
#----------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
adr_p=data$adr/(data$adults+data$children)
# Create a bar plot
ggplot(data, aes(x = market_segment, y = adr_p, fill = reserved_room_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = adr_p - sd(adr_p), ymax = adr_p + sd(adr_p)), 
                width = 0.2, 
                position = position_dodge(width = 0.7)) +
  labs(title = "ADR by Market Segment and Room Type",
       x = "Market Segment",
       y = "ADR per person [EUR]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Reserved Room Type"))
#-------------------------------------------------------------------------------
# calculating the total number of cancellations
# Absolute cancelations
total_cancelations <- sum(data$is_canceled)
rh_cancelations <- sum(data[data$hotel == "Resort Hotel", "is_canceled"])
ch_cancelations <- sum(data[data$hotel == "City Hotel", "is_canceled"])

# Relative cancelations as percent
rel_cancel <- total_cancelations / nrow(data) * 100
rh_rel_cancel <- rh_cancelations / sum(data$hotel == "Resort Hotel") * 100
ch_rel_cancel <- ch_cancelations / sum(data$hotel == "City Hotel") * 100

# Print the results
cat(paste("Total bookings canceled:", format(total_cancelations, big.mark = ","), 
          "(", format(rel_cancel, digits = 1), "%)\n"))
cat(paste("Resort hotel bookings canceled:", format(rh_cancelations, big.mark = ","), 
          "(", format(rh_rel_cancel, digits = 1), "%)\n"))
cat(paste("City hotel bookings canceled:", format(ch_cancelations, big.mark = ","), 
          "(", format(ch_rel_cancel, digits = 1), "%)\n"))

#------------
library(ggplot2)
library(dplyr)
library(forcats)

# Create a DataFrame with the relevant data for Resort Hotel
res_book_per_month <- data %>%
  filter(hotel == "Resort Hotel") %>%
  count(arrival_date_month)

res_cancel_per_month <- data %>%
  filter(hotel == "Resort Hotel") %>%
  group_by(arrival_date_month) %>%
  summarise(Cancelations = sum(is_canceled))

res_cancel_data <- data.frame(Hotel = "Resort Hotel",
                              Month = res_book_per_month$arrival_date_month,
                              Bookings = res_book_per_month$n,
                              Cancelations = res_cancel_per_month$Cancelations)

# Create a DataFrame with the relevant data for City Hotel
cty_book_per_month <- data %>%
  filter(hotel == "City Hotel") %>%
  count(arrival_date_month)

cty_cancel_per_month <- data %>%
  filter(hotel == "City Hotel") %>%
  group_by(arrival_date_month) %>%
  summarise(Cancelations = sum(is_canceled))

cty_cancel_data <- data.frame(Hotel = "City Hotel",
                              Month = cty_book_per_month$arrival_date_month,
                              Bookings = cty_book_per_month$n,
                              Cancelations = cty_cancel_per_month$Cancelations)

# Combine the data for both hotels
full_cancel_data <- bind_rows(res_cancel_data, cty_cancel_data)
full_cancel_data$cancel_percent <- full_cancel_data$Cancelations / full_cancel_data$Bookings * 100

# Order the months
ordered_months <- c("January", "February", "March", "April", "May", "June", 
                    "July", "August", "September", "October", "November", "December")
full_cancel_data$Month <- factor(full_cancel_data$Month, levels = ordered_months)

# Plot the cancelations per month
ggplot(full_cancel_data, aes(x = Month, y = cancel_percent, fill = Hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cancelations per Month",
       x = "Month",
       y = "Cancelations [%]") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "green")) +
  guides(fill = guide_legend(title = "Hotel"))
#-------------------------------------------


library(dplyr)

# Select numeric columns (excluding "is_canceled")
numeric_data <- select(data, where(is.numeric), -is_canceled)

# Calculate correlation with "is_canceled" column
cancel_corr <- cor(numeric_data, data$is_canceled)

# Sort correlation values in descending order
sorted_corr <- sort(abs(cancel_corr), decreasing = TRUE)

# Get the corresponding column names
sorted_cols <- colnames(numeric_data)[order(abs(cancel_corr), decreasing = TRUE)]

# Print the column names and sorted correlation values
for (i in 1:length(sorted_corr)) {
  cat(paste(sorted_cols[i], " - ", sorted_corr[i], "\n"))
}
#--------------------------------------------------
# Group by "is_canceled" and calculate value counts of "reservation_status"
reservation_counts <- table(data$is_canceled, data$reservation_status)

# Print the value counts
print(reservation_counts)

#-------------------------------------------------------------------
summary(data)
# Load required libraries
library(ggplot2)
library(dplyr)

# Group data for lead_time and calculate the mean and standard deviation of is_canceled
lead_cancel_data <- data %>%
  group_by(lead_time) %>%
  summarize(mean_cancel = mean(is_canceled), sd_cancel = sd(is_canceled), count = n()) %>%
  filter(count >= 10)

# Plot the data with dots only
ggplot(lead_cancel_data, aes(x = lead_time, y = mean_cancel * 100)) +
  geom_point(color="blue") +
  labs(title = "Effect of lead time on cancelation",
       x = "Lead time",
       y = "Cancelations [%]") +
  theme_minimal()
#-----------------------------------------------------------------------------------------
# Load required libraries
library(ggplot2)
library(dplyr)

# Group data for deposit_type and calculate the mean of is_canceled
deposit_cancel_data <- data %>%
  group_by(deposit_type) %>%
  summarize(mean_cancel = mean(is_canceled))

# Plot the data as a bar plot
ggplot(deposit_cancel_data, aes(x = deposit_type, y = mean_cancel * 100, fill = deposit_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Effect of deposit_type on cancelation",
       x = "Deposit type",
       y = "Cancelations [%]") +
  theme_minimal()
#-----------------------------------------------------------------------------------------
# Load required libraries
library(dplyr)

# Group data for deposit_type and calculate the mean of is_canceled
deposit_mean_data <- data %>%
  group_by(deposit_type) %>%
  summarize(mean_cancel = mean(is_canceled))

# View the resulting data
print(deposit_mean_data)
#------------------------------------------------------------------------------------------
# Load required libraries
library(tidyverse)

# Group data for adr and calculate the mean of is_canceled
adr_cancel_data <- data %>%
  group_by(adr) %>%
  summarize(mean_cancel = mean(is_canceled))

# Create the plot using ggplot2
ggplot(adr_cancel_data, aes(x = adr, y = mean_cancel * 100)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 400) +
  ylim(0, 100) +
  labs(title = "Effect of ADR on cancelation", x = "ADR", y = "Cancelations [%]") +
  theme_minimal()
#--------------------------------------------------------------------------------------------
# Load required libraries
library(tidyverse)
library(ggplot2)
# Group data by meal and calculate the count
meal_counts <- data %>%
  count(meal)

# Create a pie chart using ggplot2
ggplot(meal_counts, aes(x = "", y = n, fill = meal)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Meal Type Preference", x = NULL, y = NULL, fill = "Meal Type") +
  geom_text(aes(label = paste0(round((n/sum(n))*100), "%")), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  theme(legend.position = "right")
