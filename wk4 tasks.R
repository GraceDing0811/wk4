#set up
data<-read.csv("dataset.csv")
library(pacman)
pacman::p_load(tidyverse, glue, gapminder, ggplot, gridExtra, dplyr, purrr)

#find the mean number of injuries, homeless and deaths for all disasters
#filter the variables i want
cleaned.data<-data%>%
  select(injured_all_disasters, homeless_all_disasters, deaths_all_disasters, Entity, Year)%>%
  rename(injuries=injured_all_disasters, homeless=homeless_all_disasters, deaths=deaths_all_disasters, country=Entity)
#create a function
find.mean<-function(x){
  mean(1:3)
  na.rm=TRUE
  return()
}
find.mean(cleaned.data)

table<-cleaned.data%>%map(mean(1:3), na.rm=TRUE)
table
#automatic plotting of countries' trends
widerdata<-cleaned.data%>%
  pivot_wider(
    names_from = country,
    values_from = c('injuries', 'deaths', 'homeless')
  )
newdata<-widerdata%>%
  dplyr::select(contains(c("Year", "China", "Afghanistan", "Bangladesh", "Brazil", "Canada")))
frame_data<-data.frame(newdata)

trend.plot<-function(i){
  ggplot(aes_string(x=names(frame_data)[1], y=names(frame_data)[i]))+
    geom_point()+
    geom_line()+
    labs(title = glue("The Trends of {names(frame_data[i])}"))
  y=glue("Number of {names(frame_data[i])}")
}

line.graph<-map(2:ncol(frame_data), trend.plot)
better.linegraph<-gridExtra::grid.arrange(grobs=line.graph, ncol=3)

airquality
frame_data





















