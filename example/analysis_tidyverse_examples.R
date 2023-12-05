## Script to follow tidyverse course

##### Preliminaries #####

# install.packages("pacman")

packages <- c("dslab",
              "tidyr",
              "dplyr")

pacman::p_load(packages,
               character.only = TRUE,
               install = FALSE) # Change to TRUE to install the necessary packages

##### Data #####

# Data needed
# We will use the dslabs package for the data 
# Data Science labs

data(polls_us_election_2016, package = "dslabs") # load data from package
polls_us_election_2016 <- as_tibble(polls_us_election_2016) # convert to a tibble
data()


##### tidyr verbs ####

# We will use this section to work on four of the most used tidyr verbs


# 1. Pivot longer

longer_dataframe <- polls_us_election_2016 %>%
  pivot_longer(
    cols = c(rawpoll_clinton:adjpoll_mcmullin),
    names_to = c(".value", "candidate"),
    names_pattern = "(rawpoll|adjpoll)_(.*)"
  )

# 2. Pivot wider. 

wider_dataframes <- longer_dataframe %>%
  pivot_wider(
    names_from = candidate,
    values_from = c(rawpoll, adjpoll), 
    values_fn = list
  )

# 3. Separate

separated_data <- polls_us_election_2016 %>%
  separate(enddate, into = c("year", "month", "day"), sep = "-")

# 4. Unite 

united_data <- separated_data %>%
  unite("enddate", c("year", "month", "day"), sep = "-")

#### dplyr verbs ####

# We will use this section to work on five of the most useful dplyr verbs 

# 1. Select 

# Choose variables by names

# Only keep the variables `state, startdate, enddate, pollster, rawpoll_clinton, rawpoll_trump`

polls_us_election_2016 %>%
  select(state,startdate,enddate,pollster,rawpoll_clinton,rawpoll_trump)

# 2. Filter
# Choose observations based on certain conditions. 

# Which polls had a sample size of at least 2,000 people?

polls_us_election_2016 %>%
  filter(samplesize > 2000)

# Which A graded poll with at least 2,000 people had Trump win at least 45% of the vote?

polls_us_election_2016 %>%
  filter(grade == "A" & samplesize > 2000 & rawpoll_trump > 45)


# 3. Arrange 
# reorder rows

# Reorder the dataframe based on the samplesize

polls_us_election_2016 %>% 
  arrange(samplesize)

# 4. Mutate
#Create new variables

#  What was...the combined vote share of Trump and Clinton for each poll?
#  the difference between Trump's raw poll vote share and 538's adjusted vote share?

polls_us_election_2016 %>%
  mutate(trump_clinton_tot = rawpoll_trump + rawpoll_clinton,
         trump_raw_adj_diff = rawpoll_trump - adjpoll_trump) 

# 5. Summarise 
# Collapse data

polls_us_election_2016 %>%
  summarise(max_trump = max(rawpoll_trump)) 

# 6. group_by
# All of the above can be used in combination with group_by() to use function on groups rather than data

# What is the average vote share for Clinton by poll grade?
polls_us_election_2016 %>%
  group_by(grade) %>%
  summarise(mean_vote_clinton = mean(rawpoll_clinton))

##### Data visualization #####

# Three main things to consider in a ggplot 
# data + aes + geom

longer_dataframe %>% # 1. data, you can use the pipe
  filter(state == "District of Columbia") %>%  # we will only filter D.C
  ggplot() + # calling ggplot
  aes(x = enddate, y = rawpoll, color = candidate) + # 2. mapping aes
  geom_line() +  # 3. time of geom
theme_minimal() + # minimal theme (delete extra lines)
  theme(
    legend.position = "bottom",
    text = element_text(size = 10), # Adjust text size here
    axis.text.x = element_text(angle = 45, hjust = 1) # Angle x-axis text for dates
  ) +
  labs( # Define title and labels
    y = "Raw Poll (%)",
    x = "Date of the Poll",
    title = "2016 Elections Polls, D.C."
  )

# Please explore on your own!!! 
# Happy tidying :) 
