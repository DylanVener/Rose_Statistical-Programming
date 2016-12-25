#To run on a different computer, you need the following packages installed, as well as the data sets, which are all publicly available
library('readr')
library('rvest')
library('jsonlite')
library('tibble')
library('dplyr')
library('ggplot2')
library('tidyr')
library('lubridate')

#Code to scrape Target's website, with the input being a valid state abbreviation
grab_stores <- function(state){
  url <-paste(c('http://gam.target.com/store-locator/state-result?lnk=statelisting_stateresult&stateCode=',state,'&stateName=Minnesota'),collapse = '')
  temp.pg <- read_html(url)
  stores.df <- as_data_frame(fromJSON(html_text(html_nodes(temp.pg,'#primaryJsonResponse')))$storeList) 
  return(stores.df)
}

#apply grab_stores function to the list of state abbreviations
stores <- lapply(state.abb,grab_stores)
#bind the returned dataframes into one large dataframe
stores.df <- bind_rows(stores)
#ensuring zipcodes fit correct format
stores.df$zipCode <- strtrim(stores.df$zipCode,5)

#Filtering to remove invalid entries, correctly interpret columns, and convert to a tibble data structure
income.df <- read_csv('~/Statistical Programming/ACS_14_5YR_B19013_with_ann.csv',col_names=c('ID','zipCode','display label','med.inc','Margin of Error'),skip=2) %>%
  filter(med.inc!='-') %>%
  transform(med.inc=as.integer(med.inc))
income.df <- as_tibble(income.df[complete.cases(income.df),])
income.df$target <- ifelse(income.df$zipCode %in% stores.df$zipCode,'Yes','No')

#Density calculations, necessary for the creation of the partially colored density plot
gg <- with(density(income.df$med.inc),tibble(x,y))
gg_target <- with(density(filter(income.df,zipCode %in% stores.df$zipCode)$med.inc),tibble(x,y))
gg_target$target = rep('yes',nrow(gg_target))
gg_not <- with(density(filter(income.df,!(zipCode %in% stores.df$zipCode))$med.inc),tibble(x,y))
gg_not$target = rep('no',nrow(gg_not))

ggplot(data=income.df) + stat_density(aes(x=med.inc,color=target), geom='line',position='dodge')+
  geom_area(data=subset(gg_target,x>31943),aes(x=x,y=y),fill='blue',alpha=0.5) +
  geom_area(data=subset(gg_not,x>31943),aes(x=x,y=y),fill='red',alpha=0.5) +
  labs(x='Median Household Income',y='Density') + theme(legend.position="none")

dates.df <- read_csv('~/Statistical Programming/opendate.csv')[,c('Open Date','City','State')]
#date parsing to R's format
dates.df$`Open Date` <- parse_date(dates.df$`Open Date`,'%m/%d/%Y')
#rounding to nearest year
dates.df$year <- round_date(dates.df$`Open Date`,unit = 'year')


city.df <- read_csv('~/Statistical Programming/city_names_t.csv')
city.df <- gather(city.df,year,pci,`1969-01-01`:`2014-01-01`)
city.df$year <- parse_date(city.df$year)
# Calculating Z score for each city for each year
city.df <- city.df %>% group_by(year) %>% mutate(z.score = scale(pci))

final.df <- inner_join(dates.df,city.df,by=c('City','year'))

ggplot(data=final.df,mapping = aes(x=`Open Date`,y=z.score)) +
  geom_point() + 
  stat_smooth(method="lm") +
  labs(y='Z Score')


ggplot(data=city.df, mapping = aes(x=year,y=pci,by=City)) + 
  geom_line(color='red',alpha=0.5) +
  geom_point(data=final.df,aes(x=year,y=pci),color='blue')
