#Olympics dataset to practice the dpylr and ggplot2 package

#dataset from: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results

setwd("/Volumes/TREKSTOR/Alicia/Dokumente/Studium/Master/Block2/Intro R/Assgnm 1/input")
getwd()

#Load packages
library(dplyr)
library(ggplot2)

#Load data
olympics <- read.csv("athlete_events.csv", stringsAsFactors = FALSE)

#Data rame showing sports per olympic season and sex
disciplines_dplyr <- olympics %>%
  group_by(Sex, Season) %>%
  summarise( n_distinct(Sport)) %>%
  rename(Competitor_Sex = 'Sex', Olympic_Season = 'Season',
         Num_Sports = 'n_distinct(Sport)')

#Dataframe showing all observations of the discipline 'tennis' sorted 
#by year, name and event
olympics_tennis_dplyr <- filter(olympics, Sport == "Tennis")
sorted_tennis_dplyr <- arrange(olympics_tennis_dplyr, desc(Year), Name, Event)
tennis_dplyr <- mutate(sorted_tennis_dplyr, prize = 
                         Medal %in% c("Gold", "Silver", "Bronze"))

#Plot: Number of olympians per individual weight
plot_weight <- ggplot(data = olympics, aes(x = Weight)) +
  geom_histogram(bins= 35, color="black", fill="cornflowerblue") +
  coord_cartesian(xlim = c(30, 214)) +
  scale_y_continuous("Number of olympians") +
  scale_x_continuous("Individual weight") +
  ggtitle("Number of olympians per individual weight") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
plot_weight

#Plot: Height of olympians per sex
plot_height <- ggplot(data = olympics, aes(x = Sex, y = Height, fill =Sex)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("pink1", "lightblue1")) +
  ggtitle("Height of olympians per sex") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
plot_height 

#Plot: Number of olympians per games by sex

winter_games <- olympics %>%
  filter(Season == 'Winter')

summer_games <- olympics %>%
  filter(Season == 'Summer')

plot_nooly_WO <- ggplot(data = winter_games, aes(x = Games, fill = Sex))+
  geom_bar() + 
  labs(y = "Number of olympians") + 
  scale_color_manual(values = c("pink1", "lightblue1"), 
                     aesthetics = c("colour", "fill")) + 
  ggtitle("Number of olympians per winter games by sex")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))

plot_nooly_WO

plot_nooly_SO <- ggplot(data = summer_games, aes(x = Games, fill = Sex))+
  geom_bar() + 
  labs(y = "Number of olympians") + 
  scale_color_manual(values = c("pink1", "lightblue1"), 
                     aesthetics = c("colour", "fill")) + 
  ggtitle("Number of olympians per summer games by sex")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5))

plot_nooly_SO

#Top 20 BMI's per Disciplines 
top_bmi_dplyr <- olympics %>%
  filter(!is.na(Height) ) %>%
  filter(!is.na(Weight) ) %>%
  mutate(Height_m = Height / 100) %>%
  mutate(BMI = Weight / (Height_m^2)) %>%
  group_by(Sport) %>%
  summarise(mean(BMI)) %>%
  rename(Mean_BMI = 'mean(BMI)') %>%
  top_n(20, Mean_BMI) %>%
  arrange(desc(Mean_BMI))

