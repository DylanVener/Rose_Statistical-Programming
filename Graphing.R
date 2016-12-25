# Read Data
# Grab the data available from online.
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

url <- "http://sites.williams.edu/rdeveaux/files/2014/09/Saratoga.txt"

RealEstate.df <- read_delim(url, delim="\t", col_names=FALSE, skip=1)
colnames(RealEstate.df) <- readLines(url, n=1) %>%
  str_replace_all('(\\")', "") %>%
  str_split("[[:space:]]") %>%
  unlist()
# Clean Data
# Change numeric variables to categorical descriptive variables.
RealEstate.df <- RealEstate.df %>%
  mutate(Fuel.Type = factor(Fuel.Type,
                            labels=c("Gas", "Electric", "Oil")),
         Sewer.Type = factor(Sewer.Type,
                             labels=c("None", "Private", "Public")),
         Heat.Type = factor(Heat.Type,
                            labels=c("Hot Air", "Hot Water", "Electric")),
         Waterfront = factor(Waterfront,
                             labels=c("No", "Yes")),
         Central.Air = factor(Central.Air,
                              labels=c("No", "Yes")),
         New.Construct = factor(New.Construct,
                                labels=c("No", "Yes")))
RealEstate.df$Room.size <- RealEstate.df$Living.Area/RealEstate.df$Bedrooms

ggplot(data=RealEstate.df,mapping=aes(x=Room.size,fill=New.Construct))+
  geom_density(alpha=0.5)+
  labs(x='Average Room Size',y='Density')+
  theme_bw()
# Might retitle legend - change New.Construct to "Type of Home" and No to "Old" and Yes to "New"
# Graph is useful and overall very good - maybe show where the exact typical values fall
geom_text(aes(x=500,y=0.0025,label="Old"))
geom_text(aes(x=750,y=0.0025,label="Old"))

ggplot(data=RealEstate.df,mapping=aes(x=New.Construct,y=Room.size))+
  geom_boxplot(width=0.25)+
  geom_violin(alpha=0.25)+
  labs(x='New Construction?',y='Room Size',fill='')+
  theme_bw()
