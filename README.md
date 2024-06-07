# Tidy Tuesday {06/06/2024}

##Summary
Makes two plots:
1. Coord_flipped line graph of summarized fat content for different family of cheeses
2. A waffle plot showing a distribution of cheese's made from different animal milk

https://github.com/rfordatascience/tidytuesday/blob/master/data/2024/2024-06-04/readme.md


```r

#loading packages
library(tidyverse)


#dataset
cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

write.csv(cheeses, file = file.path(getwd(), "cheeses.csv"), row.names = FALSE)

get#cleaning up fat_content column, only include cheeses with values 
cheese_fat_only <- cheeses[!grepl("g|-", cheeses$fat_content), ]

cheese_fat_only <- cheese_fat_only[!is.na(cheese_fat_only$fat_content),]

cheese_fat_only$fat_content <- gsub("%", "", cheese_fat_only$fat_content)

cheese_fat_only$fat_content <- as.numeric(cheese_fat_only$fat_content)

cheese_fat_only <- cheese_fat_only[order(cheese_fat_only$fat_content, decreasing = TRUE),]

#averaging based on family of cheeses, if NA exlcuded 
average_fatcontent <- cheese_fat_only %>%
  group_by(family) %>%
  summarise(
    average_fatcontent = mean(fat_content, na.rm = TRUE),
    median_fatcontent = median(fat_content, na.rm = TRUE),
    sd_fatcontent = sd(fat_content, na.rm = TRUE),
    min_fatcontent = min(fat_content, na.rm = TRUE),
    max_fatcontent = max(fat_content, na.rm = TRUE),
    count = n()
  )

average_fatcontent <- average_fatcontent[!is.na(average_fatcontent$family),]

average_fatcontent <- average_fatcontent[order(average_fatcontent$average_fatcontent, 
                                               decreasing = TRUE),]

average_fatcontent$family <- factor(average_fatcontent$family, levels 
                                    = average_fatcontent$family)

#plot
ggplot(average_fatcontent, aes(x = family, y = average_fatcontent, label = round(average_fatcontent, 2))) +
  geom_point(stat = 'identity', fill = "black", size = 10) +
  geom_segment(aes(y = 0,
                   x = family,
                   yend = average_fatcontent,
                   xend = family),
               color = "black", size = 1.0) +
  geom_text(color = "white", size = 3) +
  labs(title = "The Average Fat Content (%) of Cheese Types",
       x = "Family of Cheeses",
       y = "Average Fat Content (%)",
       subtitle = "Source: cheese.com") +
  coord_flip() +
  scale_y_continuous(limits = c(20, 50))


#Waffleplot source: https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html


#Remove rows with multiple types of milk
cheeses_cleaned <- cheeses %>%
  filter(!grepl(",|NA", milk) & !is.na(milk))

# Ensure the sum of milk_table matches total_cells
milk_types <- cheeses_cleaned$milk  # Extract the milk column

# Calculate the frequency of each milk type
milk_table <- table(milk_types)
nrows <- 32
total_cells <- nrows * nrows

# Ensure the sum of milk_table matches total_cells
milk_table_adjusted <- rep(names(milk_table), milk_table)
while (length(milk_table_adjusted) < total_cells) {
  milk_table_adjusted <- c(milk_table_adjusted, sample(names(milk_table), total_cells - length(milk_table_adjusted), replace = TRUE))
}
if (length(milk_table_adjusted) > total_cells) {
  milk_table_adjusted <- milk_table_adjusted[1:total_cells]
}

# Create a dataframe for plotting
df <- expand.grid(y = 1:nrows, x = 1:nrows)
df$category <- factor(milk_table_adjusted, levels = names(milk_table))

desired_order <- c("cow", "goat", "sheep", "buffalo", "camel", "donkey", "moose", "plant-based", "water buffalo", "yak")

# Order df$category according to the desired order
df$category <- factor(df$category, levels = desired_order)


# Plot the data
ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribution of Cheese Types based on the Source of Milk",
       subtitle = "Which Animal’s Milk is Most Cheese Made From?",
       caption = "Source: cheese.com") +
  theme(plot.title = element_text(size = rel(1.2)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "right")

```
