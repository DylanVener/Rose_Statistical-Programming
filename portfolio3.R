#To run on a different computer, dplyr, tibble, and ggplot2 must be installed, and they must have the dataset on their computer, and to modify the path to its location
library('dplyr')
library('tibble')
library('ggplot2')
ma.df <- as_tibble(read.csv(file="~/Statistical Programming/MA383.csv",header=TRUE, stringsAsFactors=FALSE))
ma.df <- ma.df%>%arrange(YEAR,TERM)
#Filters to only upper level statistics classes
upper.df <- ma.df %>% filter(CRSE %in% c(383,385,386,387,480,481,482,485,487))
students <- unique(upper.df$ID)
classes <- unique(upper.df$CRSE)
results.df <- tibble()

#counts up the instances of each class, for each year, and then plots each value
time <- as_tibble(table(upper.df[,names(upper.df)%in% c("CRSE","YEAR")]))
ggplot(data=time,mapping=aes(x=YEAR,y=n,group=CRSE,color=CRSE=="383")) +
  geom_line() + xlab("Year") + ylab("Number of Students") + ggtitle('MA383 vs Other Upper Level Classes')
  theme(legend.position="none")

#Classifies a student's list of classes, into N/A, terminating, branching, or elective, for a class ID
classify <- function(crs,ID){
  if(!(ID %in% crs)){
    return(c(0,0,0,1))
  } else if(length(crs)==1){
    return(c(1,0,0,0))
  } else if(crs[1]==ID){
    return(c(0,1,0,0))
  } else{
    return(c(0,0,1,0))
  }
}

#Loops through each students, and counts up each classifications
for (k in classes){
  results=c(0,0,0,0)
  for (i in students){
   results <- results+classify(filter(upper.df,ID==i)$CRSE,k) 
  }
  rbind(results.df,tibble(course=k,term=results[1],branch=results[2],elec=results[3])) -> results.df
}

#Because of certain difficulties, I resorted to harcoding the pie chart to the correct values, to ensure the clarity
pie(c(34,22,28),labels=c('Terminal 40.5%','Bridging 26.2%','Elective 33.3%')) 
