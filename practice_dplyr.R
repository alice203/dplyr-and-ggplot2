#Practice dplyr basics using the olympics dataset

#dataset from: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results

library('dplyr')

#Load data
olympics <- read.csv("athlete_events.csv", stringsAsFactors = FALSE)

# ---------------------------------- (1) Subsetting observations
### Filter all US-American athletes above 40 y/o
old_athletes <- olympics %>%
  filter(NOC == 'USA') %>%
  filter(Age > 40)

### Filter the first ten observations of the olympics dataframe
top_ten_rows <- slice(olympics, 1:10)

###Randomly select a random fraction of observations
olympics_random <- sample_frac(olympics, 0.7, replace= TRUE)

### Selecting only the team column
# And create dataframe with unique country names
teams  <- select(olympics, contains('Team')) %>%
  distinct()

# And create dataframe with unique National Olympic Committee
noc <- select(olympics, contains('NOC')) %>%
  distinct()

# ---------------------------------- (2) Create new variables

#Create a new variable called 'YearOfBirth' per athlete
olympics <- mutate(olympics, YearOfBirth = Year - Age)

#Create new variable called 'Height (m)'
olympics <-  mutate(olympics, 'Height(m)' = Height/100)

#Create new variable called "Gold"
olympics <- mutate(olympics,)

# ---------------------------------- (3) Group observations
#Summarize medals by team/country and sort by most medals
medal_table <- olympics %>%
  group_by(NOC, Team) %>%
  summarise(n()) %>%
  rename( Medals = 'n()') %>%
  arrange(desc(Medals))

