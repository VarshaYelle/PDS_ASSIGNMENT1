raw_data <- read.csv("C:/Users/Dell/OneDrive/Documents/PDS/Student/data_raw/StudentsPerformance.csv")
clean_data<-na.omit(raw_data[raw_data != "N", ])
write.csv(clean_data,"C:/Users/Dell/OneDrive/Documents/PDS/Student/data_clean/clean_raw_data.csv")

#Loading  clean Data

clean_data<-read.csv2("C:/Users/Dell/OneDrive/Documents/PDS/Student/data_clean/clean_raw_data.csv")
clean_data
library(tidyverse) # metapackage with lots of helpful functions
library(dplyr)
library(ggplot2)
data=read.csv("C:/Users/Dell/OneDrive/Documents/PDS/Student/data_raw/StudentsPerformance.csv")
Summ<-summary(clean_data)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
data %>%
  group_by(race.ethnicity) 
  summarise(mathMean= mean(math.score),readingMean =  mean(reading.score),writingMean = mean(writing.score))%>% 
  ggplot(data = ., aes(x = race.ethnicity, y = mathMean, fill = race.ethnicity))+ geom_bar(stat = "identity")
ggsave("C:/Users/Dell/OneDrive/Documents/PDS/Student/results/race.ethnicity.jpg")
total <- table(data$race.ethnicity)
plot(table(data$race.ethnicity))
ggsave("C:/Users/Dell/OneDrive/Documents/PDS/Student/results/data$race.ethnicity.jpg")
x1 <- data %>% 
  group_by(race.ethnicity) %>%
  filter(math.score > 80) 
table(x1$race.ethnicity)
x1 %>% ggplot() + geom_bar(aes(x1$race.ethnicity,fill = x1$race.ethnicity))
ggsave("C:/Users/Dell/OneDrive/Documents/PDS/Student/results/x1$race.ethnicity.jpg")
data %>% 
  group_by(gender) %>%
  select(math.score,reading.score,writing.score) %>%
  filter(math.score > 80, reading.score > 80, writing.score > 80) %>%
  ggplot(data = ., aes(x = gender, y = math.score, 
                       fill = gender)) + geom_bar(stat = "identity")
ggsave("C:/Users/Dell/OneDrive/Documents/PDS/Student/results/Gender.jpg")
level1 <- data %>% 
  group_by(parental.level.of.education) %>%
  summarise(mathTotal = sum(math.score))

level1 %>% ggplot(data = ., aes(x = parental.level.of.education, y = mathTotal, 
                                fill = parental.level.of.education)) + geom_bar(stat = "identity") + 
  labs(title="Education Level Chart",
       subtitle="Reading Scores") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggsave("C:/Users/Dell/OneDrive/Documents/PDS/Student/results/Education level chart.jpg")

