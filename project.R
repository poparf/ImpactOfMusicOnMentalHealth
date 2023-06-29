print(mxmh_survey_results)


#removing timestamp and permission column
df <- mxmh_survey_results[,-c(1,33)]


#install a special library called "tidyr"
install.packages("tidyr")
library(tidyr)

#function from the library to drop all rows that present N/A in any variable

df_clean <- df %>% drop_na()



df_clean$AgeGroup <- cut(df_clean$Age,
                         breaks = c(10,18,25,35,50, Inf),
                         labels = c("10-17 years", "18-24 years",
                                    "25-34 years", "35-49 years",
                                    "50+ years"),
                         right = FALSE)


library(ggplot2)
# plot the age groups
pl <- ggplot(data = df_clean,aes(x= AgeGroup))
pl <- pl + geom_bar()
pl

install.packages("writexl")
library("writexl")
write_xlsx(df_clean,"Documents\newgroups.xlsx")

require(dplyr)
install.packages("dplyr")
library(dplyr)

g <- df_clean %>%
  group_by(AgeGroup) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))

head(as.data.frame(g))


strata <- c("10-17 years", "18-24 years", "25-34 years", "35-49 years", "50+ years")
proportions <- c(0.214, 0.461, 0.190, 0.080, 0.055)
max_sample_size <- 600
sample_sizes <- proportions * max_sample_size
total_sample_size <- sum(sample_sizes)

sample_sizes <- round(sample_sizes * max_sample_size / total_sample_size)
# RESULT: 13  28  11  5  3

sampled_datav2 <- data.frame()
set.seed(235235234109523)
for (i in 1:length(strata)) {
  stratum_data <- df_filtered[df_filtered$Age == strata[i], ]  # Subset the data for the current stratum
  stratum_sample <- stratum_data[sample(nrow(stratum_data), sample_sizes[i]), ]  # Perform random sampling within the stratum
  sampled_datav2 <- rbind(sampled_data, stratum_sample)  # Append the sampled data to the result
}






install.packages("rsample")
library(rsample)


library(dplyr)

set.seed(372003)
# Subset data for each age group
age_group_10_17 <- df_clean %>% filter(AgeGroup == "10-17 years")
age_group_18_24 <- df_clean %>% filter(AgeGroup == "18-24 years")
age_group_25_34 <- df_clean %>% filter(AgeGroup == "25-34 years")
age_group_35_49 <- df_clean %>% filter(AgeGroup == "35-49 years")
age_group_50_plus <- df_clean %>% filter(AgeGroup == "50+ years")

# Sample from each age group
sampled_age_group_10_17 <- age_group_10_17 %>% sample_n(size = 13)
sampled_age_group_18_24 <- age_group_18_24 %>% sample_n(size = 28)
sampled_age_group_25_34 <- age_group_25_34 %>% sample_n(size = 11)
sampled_age_group_35_49 <- age_group_35_49 %>% sample_n(size = 5)
sampled_age_group_50_plus <- age_group_50_plus %>% sample_n(size = 3)

# Combine sampled data frames
sampled_data <- rbind(sampled_age_group_18_24, sampled_age_group_10_17, 
                      sampled_age_group_25_34, sampled_age_group_35_49, 
                      sampled_age_group_50_plus)

# Sort sampled data frame by row indices
sampled_data <- sampled_data[order(rownames(sampled_data)), ]


print(sampled_data)


head(sampled_data)
# write the second sample as an excel file.
write.csv(sampled_data,"sample2.csv")


df_filtered <- df_clean[df_clean$`Hours per day` <= 10,]

g <- df_filtered %>%
  group_by(AgeGroup) %>%
  summarise(cnt = n()) %>%
  mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  arrange(desc(freq))

g <- df_filtered %>%
  group_by(AgeGroup) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))

head(as.data.frame(g))


set.seed(2666666)
# Subset data for each age group
age_group_18_24 <- df_filtered %>% filter(AgeGroup == "18-24 years")
age_group_10_17 <- df_filtered %>% filter(AgeGroup == "10-17 years")
age_group_25_34 <- df_filtered %>% filter(AgeGroup == "25-34 years")
age_group_35_49 <- df_filtered %>% filter(AgeGroup == "35-49 years")
age_group_50_plus <- df_filtered %>% filter(AgeGroup == "50+ years")

# Sample from each age group
sampled_age_group_18_24 <- age_group_18_24 %>% sample_n(size = 28)
sampled_age_group_10_17 <- age_group_10_17 %>% sample_n(size = 13)
sampled_age_group_25_34 <- age_group_25_34 %>% sample_n(size = 11)
sampled_age_group_35_49 <- age_group_35_49 %>% sample_n(size = 5)
sampled_age_group_50_plus <- age_group_50_plus %>% sample_n(size = 3)

# Combine sampled data frames
sampled_datav2 <- rbind(sampled_age_group_18_24, sampled_age_group_10_17, 
                      sampled_age_group_25_34, sampled_age_group_35_49, 
                      sampled_age_group_50_plus)

# Sort sampled data frame by row indices
sampled_datav2 <- sampled_datav2[order(rownames(sampled_datav2)), ]

write.csv(sampled_datav2,"FirstSample.csv")

write.csv(df_filtered,"DataSet.csv")



# ANOVA #1
#H0: The average number of hours is equal for any platform of streaming.
#H1: The average number of hours is not equal for any platform of streaming.
# data is in df_filtered
# First step:
# Compute descriptive statistics
set.seed(1234)
dplyr::sample_n(df_filtered, 10)

library(dplyr)
group_by(df_filtered, `Primary streaming service`) %>%
  summarise(
    count = n(),
    mean = mean((`Hours per day`), na.rm = TRUE),
    sd = sd((`Hours per day`), na.rm = TRUE)
  )

# ANOVA computation
res.aov <- aov(`Hours per day` ~ `Primary streaming service`, data = df_filtered)
# Summary 
summary(res.aov)


#df$`Frequency [Classical]`
#library(dplyr)df$`Frequency [Video game music]`$ 

group_by(df_filtered, `Frequency [Video game music]`) %>%
  summarise(
    count = n(),
    mean = mean((Depression), na.rm = TRUE),
    sd = sd((Depression), na.rm = TRUE)
  )

# ANOVA computation
res.aov <- aov(Depression ~ `Frequency [Video game music]`, data = df_filtered)
# Summary 
summary(res.aov)

