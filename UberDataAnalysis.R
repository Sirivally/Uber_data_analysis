install.packages('ggthemes')
install.packages('scales')
install.packages('lubridate')
install.packages('DT')
install.packages('cowplot')
install.packages('sf')
install.packages('mapview')
install.packages('rnaturalearth')
install.packages('rnaturalearthdata')
install.packages('maps')
install.packages('ggmap')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(lubridate)
library(DT)
library(cowplot)
library(sf)
library(mapview)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(ggmap)


# Extracting data

apr_data=read.csv("apr14.csv")
may_data=read.csv("may14.csv")
jun_data=read.csv("jun14.csv")
jul_data=read.csv("jul14.csv")
aug_data=read.csv("aug14.csv")
sep_data=read.csv("sep14.csv")
Data=rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)
Data

# Visualizing tables
head(Data)
tail(Data)
summary(Data)
str(Data)
unique(Data$Base)  
unique(Data$month)

# Analyzing Data
Data$Date.Time=as.POSIXct(Data$Date.Time,format='%m/%d/%Y %H:%M:%S')

Data$Time=format(as.POSIXct(Data$Date.Time,format='%m/%d/%Y %H:%M:%S'),format='%H:%M:%S')

Data$day=format(day(Data$Date.Time))

Data$weekday=format(wday(Data$Date.Time,label=TRUE))

Data$Date.Time=ymd_hms(Data$Date.Time)

Data$month=format(month(Data$Date.Time,label=TRUE))

Data$Hour=format(hour(hms(Data$Time)))

Data$minute=format(minute(hms(Data$Time)))

Data$second=format(second(hms(Data$Time)))



# visualization


trips_hour=Data %>% group_by(Hour) %>% summarize(Total=n())
datatable(trips_hour)
trips_hour_viz=ggplot(trips_hour,aes(Hour,Total))+
              geom_bar(stat='identity',fill='light green',color='black')+
              theme(plot.background=element_rect(fill='light blue'),
                    panel.background=element_rect(fill='light blue'))+
             ggtitle('Trips by Hour')+scale_y_continuous(labels=comma)
trips_hour_viz

############

trips_month=Data %>% group_by(month) %>% summarize(Total=n())
datatable(trips_month)
trips_month_viz=ggplot(trips_month,aes(month,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
        ggtitle('Trips by Month')+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_month_viz


############

trips_base=Data %>% group_by(Base) %>% summarize(Total=n())
datatable(trips_base)
trips_base_viz=ggplot(trips_base,aes(Base,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips by Bases')+geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_base_viz

plot_grid_1=plot_grid(trips_hour_viz,trips_month_viz,trips_base_viz)
plot_grid_1

############ Base_April

trips_base_apr=Data %>% group_by(Base,month) %>% filter(month=='Apr') %>% 
  summarize(Total=n())
datatable(trips_base_apr)
trips_base_apr_viz=ggplot(trips_base_apr,aes(Base,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips in apr  base')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_base_apr_viz


############ Base_may


trips_base_may=Data %>% group_by(Base,month) %>% filter(month=='May') %>% 
  summarize(Total=n())
datatable(trips_base_may)
trips_base_may_viz=ggplot(trips_base_may,aes(Base,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips in may  base')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_base_may_viz


############ Base_Jun


trips_base_jun=Data %>% group_by(Base,month) %>% filter(month=='Jun') %>% 
  summarize(Total=n())
datatable(trips_base_jun)
trips_base_jun_viz=ggplot(trips_base_jun,aes(Base,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips in jun  base')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_base_jun_viz

############ Base_Jul


trips_base_jul=Data %>% group_by(Base,month) %>% filter(month=='Jul') %>% 
  summarize(Total=n())
datatable(trips_base_jun)
trips_base_jul_viz=ggplot(trips_base_jul,aes(Base,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips in jul  base')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_base_jul_viz

############ Base_Aug


trips_base_aug=Data %>% group_by(Base,month) %>% filter(month=='Aug') %>% 
  summarize(Total=n())
datatable(trips_base_aug)
trips_base_aug_viz=ggplot(trips_base_aug,aes(Base,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips in aug base')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_base_aug_viz

############ Base_Sep


trips_base_sep=Data %>% group_by(Base,month) %>% filter(month=='Sep') %>% 
  summarize(Total=n())
datatable(trips_base_jun)
trips_base_sep_viz=ggplot(trips_base_sep,aes(Base,Total))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips in sep base')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_base_sep_viz

###########

plot_grid_base=plot_grid(trips_base_apr_viz,trips_base_may_viz,trips_base_jun_viz,
                      trips_base_jul_viz,trips_base_aug_viz,trips_base_sep_viz)
plot_grid_base
   
ggplot(Data,aes(Base,fill=month))+geom_bar(position = "Dodge")+
  scale_y_continuous(label=comma)
############ April_weekday

trips_apr_weekday=Data %>% group_by(month,weekday) %>% 
  filter(month=='Apr') %>% summarize(Total=n())
datatable(trips_apr_weekday)
trips_apr_weekday_viz=ggplot(trips_apr_weekday,aes(weekday,Total,fill=month))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips by day in apr')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_apr_weekday_viz

############ May_weekday

trips_may_weekday=Data %>% group_by(month,weekday) %>% 
  filter(month=='May') %>% summarize(Total=n())
datatable(trips_may_weekday)
trips_may_weekday_viz=ggplot(trips_may_weekday,aes(weekday,Total,fill=month))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips by day in may')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_may_weekday_viz

############ June_weekday

trips_jun_weekday=Data %>% group_by(month,weekday) %>% 
  filter(month=='Jun') %>% summarize(Total=n())
datatable(trips_jun_weekday)
trips_jun_weekday_viz=ggplot(trips_jun_weekday,aes(weekday,Total,fill=month))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips by day in jun')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_jun_weekday_viz

############ July_weekday

trips_jul_weekday=Data %>% group_by(month,weekday) %>% 
  filter(month=='Jul') %>% summarize(Total=n())
datatable(trips_jul_weekday)
trips_jul_weekday_viz=ggplot(trips_jul_weekday,aes(weekday,Total,fill=month))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips by day in july')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_jul_weekday_viz

############ August_weekday

trips_aug_weekday=Data %>% group_by(month,weekday) %>% 
  filter(month=='Aug') %>% summarize(Total=n())
datatable(trips_aug_weekday)
trips_aug_weekday_viz=ggplot(trips_aug_weekday,aes(weekday,Total,fill=month))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips by weekday in aug')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_aug_weekday_viz

############ September_weekday

trips_sep_weekday=Data %>% group_by(month,weekday) %>% 
  filter(month=='Sep') %>% summarize(Total=n())
datatable(trips_sep_weekday)
trips_sep_weekday_viz=ggplot(trips_sep_weekday,aes(weekday,Total,fill=month))+
  geom_bar(stat='identity',fill='light green',color='black')+
  theme(plot.background=element_rect(fill='light blue'),
        panel.background=element_rect(fill='light blue'))+
  ggtitle('Trips by weekday in sep')+scale_y_continuous(labels=comma)+
  geom_text(aes(label=signif(Total)),size = 2,vjust=-0.25)
trips_sep_weekday_viz

plot_weekday=plot_grid(trips_apr_weekday_viz,trips_may_weekday_viz,trips_jun_weekday_viz,
          trips_jul_weekday_viz,trips_aug_weekday_viz,trips_sep_weekday_viz)
plot_weekday

ggplot(Data,aes(Base,fill=weekday))+geom_bar(position = "Dodge")+
  scale_y_continuous(label=comma)


#map=get_googlemap(center=c())
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(world) +
  geom_point(Data, aes(lon,lat), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)


summary(Data)

min_lat=39.66
max_lat=42.12
min_long=-74.93
max_long=-72.07


ggplot(Data,aes(Lon,Lat))+
  geom_point(size=1,color='blue')+
  scale_x_continuous(limits=c(min_long,max_long))+
  scale_y_continuous(limits=c(min_lat,max_lat))+
  theme_map()+
  ggtitle("apr-sep New york city uber rides")

install.packages('leaflet')
library(leaflet)
#leaflet() %>% addTiles() %>% 
  
ggplot(Data,aes(Lon,Lat))+
  geom_point(size=1,color='blue')+
  scale_x_continuous(limits=c(min_long,max_long))+
  scale_y_continuous(limits=c(min_lat,max_lat))+
  ggtitle("apr-sep New york city uber rides")

map_data=map_data("world")
View(map_data1)
map_data1=full_join(map_data,Data,by=c('lat','long'))
View(Data)
map_data2=map_data1 %>% filter(!is.na(map_data1$Base))
View(map_data2)



colnames(Data)[2]='lat'
colnames(Data)[3]='long'
head(Data)


locations_sf <- st_as_sf(Data, coords = c("long", "lat"), crs = 4326)
mapview(locations_sf)


bw_map <- get_googlemap(center = c(min_lat, min_long), zoom = 6)




