#Data from 2019-05-28
#Wine ratings

#Clear workspace and load packages
rm(list=ls(all=TRUE))
install.packages("tidyverse")
library(tidyverse)
library(ggthemes)
library(lubridate)

#Load and inspect data
wine <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

summary(wine)
head(wine)
colnames(wine)

#Take care of NAs
wine <- na.omit(wine)

#################################
#Graph 1
#################################

#Create a subset for California
cali <- wine %>%
  filter(province == "California")

#Filter out my favorite varieties
grapes <- c("Champagne Blend" , "Pinot Grigio" , "Moscato")
cali <- cali %>%
  filter(variety %in% grapes)

#Filter out the price outliar
cali <- cali %>%
  filter(price < 80)

#Create a graph plotting wine flavor vs price
cali %>%
  ggplot(mapping = aes(x = price, y = points), title = "Price versus Points") +
  geom_point(position = "jitter", aes(colour = variety)) +
  geom_smooth() +
  ggtitle("California Wine Flavor Versus Price") +
  xlab("Price (USD)") +
  ylab("Flavor Rating (0-100)")

#################################
#Graph 2
#################################

#Create a rosé subset
rosé <- wine %>%
  filter(variety == "Rosé")

#Create a factor to sort state names alphabetically
rosé$province <- factor(rosé$province)
levels(rosé$province)

#Filter out the crazy price outliar
rosé <- rosé %>%
  filter(price < 1000)

#Create a graph Comparing price of Rose in all four states
rosé %>%
  ggplot(mapping = aes(x = province, y = price, fill = province)) +
  geom_boxplot() +
ggtitle("Winery Rosé Prices By State") +
  xlab("State") +
  ylab("Price (USD)") +
  scale_fill_brewer(palette="Dark2") + 
  theme_classic() +
  theme(legend.position = "none")
