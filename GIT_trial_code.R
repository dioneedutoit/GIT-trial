install.packages("tidyverse")
library("tidyverse")

data <- read.csv("tidy_data.csv")

#to clean column headers, remove spaces and capitals
install.packages("janitor")
library(janitor)
data <- data %>%
  clean_names()

#check if column names have changed
summary(data)

#remove rows with missing data
data <- data %>%
  drop_na()


#change two columns (harvested_non_harvested & sample_site) to factors
data <- data %>%
  mutate(
    sample_site = as.factor(sample_site),
    harvested_non_harvested = as.factor(harvested_non_harvested)
  )




#save new data as csv
write.csv(data, "tidy_data.csv", row.names = FALSE)


#scatter plot showing relationship between stipe length and frond length
ggplot(data, aes(x = stipe_length_cm, y = frond_length_cm, color = harvested_non_harvested)) +
  geom_point() +
  labs(title = "Relationship Between Stipe Length and Frond Length")


#boxplot showing length at different depths
ggplot(data, aes(x = factor(depth_m), y = total_length_cm, fill = factor(depth_m))) +
  geom_boxplot() +
  labs(title = "Total Length Across Depth Levels",
       x = "Depth (m)",
       y = "Total Length (cm)") +
  theme_minimal()

#ttest comparing total length between harvested and non-harvested kelp
t.test(total_length_cm ~ harvested_non_harvested, data = data)
