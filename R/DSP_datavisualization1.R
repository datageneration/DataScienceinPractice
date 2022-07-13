## Data Science in Practice Hands-on Workshop: Data visualization
install.packages(c("haven","descr","tidyverse", "png","gifski", "gapminder","gganimate","RColorBrewer"),repos = "http://cran.rstudio.com")
lapply(c("haven","descr","tidyverse", "png","gifski", "gapminder","gganimate","RColorBrewer"), require, character.only = TRUE)

# Create variables composed of random numbers
x <-rnorm(50) 
y = rnorm(x)

x

y
# Plot the points in the plane 
plot(x, y)

# Plot better, using the ggplot2 package 
## Prerequisite: install and load the ggplot2 package

library(ggplot2)
qplot(x,y)

# Plot better better with ggplot2
# x <-rnorm(50) 
# y = rnorm(x)
ggplot(,aes(x,y)) + theme_bw() + geom_point(col="blue")


# Import the TEDS 2016 data in Stata format using the haven package

library(haven)
TEDS_2016 <- haven::read_stata("https://github.com/datageneration/home/blob/master/DataProgramming/data/TEDS_2016.dta?raw=true")

# Prepare to analyze the Party ID variable 
# Assign label to the values (1=KMT, 2=DPP, 3=NP, 4=PFP, 5=TSU, 6=NPP, 7="NA")

TEDS_2016$PartyID <- factor(TEDS_2016$PartyID, labels=c("KMT","DPP","NP","PFP", "TSU", "NPP","NA"))

# Check the variable
attach(TEDS_2016)
head(PartyID)
tail(PartyID)

# Run a frequency table of the Party ID variable using the descr package
library(descr)
freq(TEDS_2016$PartyID)

# Plot the Party ID variable
library(ggplot2)
ggplot(TEDS_2016, aes(PartyID)) + 
  geom_bar()

ggplot2::ggplot(TEDS_2016, aes(PartyID)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Party Support (%)") + 
  xlab("Taiwan Political Parties")

ggplot2::ggplot(TEDS_2016, aes(PartyID)) + 
  geom_bar(aes(y = (..count..)/sum(..count..),fill=PartyID)) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Party Support (%)") + 
  xlab("Taiwan Political Parties") +
  theme_bw()

ggplot2::ggplot(TEDS_2016, aes(PartyID)) + 
  geom_bar(aes(y = (..count..)/sum(..count..),fill=PartyID)) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Party Support (%)") + 
  xlab("Taiwan Political Parties") +
  theme_bw() +
  scale_fill_manual(values=c("steel blue","forestgreen","khaki1","orange","goldenrod","yellow","grey"))

# Create animated chart

my_packages <- c("tidyverse", "png","gifski", "gapminder","gganimate","RColorBrewer")
install.packages(my_packages, repos = "http://cran.rstudio.com")

library(gapminder)
library(gganimate)
library(gifski)
library(png)
library(RColorBrewer)

data("gapminder")
gm = gapminder
# Basic scatter plot object

mapping <- aes(x =gdpPercap, y = lifeExp, 
               size = pop, color = continent,
               frame = year)



# Note: manual color choices.

ggplot(gapminder, mapping = mapping) + # Gather mapping data
  geom_point() + # Add scatter plot layer
  theme_linedraw() + # Add background
  scale_x_log10() + # preprocess X variable
  scale_color_manual(values=c("darkviolet","darkblue","firebrick1","forestgreen","deepskyblue1")) +
  # Control colors
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  # Labels
  geom_text(aes(label=ifelse((country == "China"), "China", ifelse(country=="United States", "United States", ""))),vjust=0,nudge_y = 1,size=6) +
  # Add text labels
  transition_time(year) + # Control animation by year
  ease_aes('linear') # Control easing of aesthetics (constant speed)