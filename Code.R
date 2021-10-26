library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(patchwork)
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

#fall in number of participants
data.frame(race)%>%
  select(event,race,country,date,participation,participants)%>%
  mutate(Year=as.Date(date)%>%
  year())%>%
  distinct()%>%
  group_by(Year)%>%
  select(Year,event,country,participants)%>%
  summarise(n=sum(participants))%>%
  arrange(desc(Year))%>%
  data.frame()->yearr

colnames(yearr)<-c("Year","Participants")

ggplot(yearr,aes(x=Year,y=Participants, label=Participants))+
  geom_segment(aes(x=Year,xend=Year,y=0,yend=Participants),colour="#f4f4f8")+
  geom_point(size=12, color="#fe4a49", fill=alpha("#fed766", 0.8), alpha=0.9, shape=21, stroke=2)+
  geom_text(color = "black", fontface="bold",size = 3)+
  scale_x_continuous(breaks = c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(limits=c(0,40000),breaks=c(5000,10000,15000,20000,25000,30000,35000,40000))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "white",face="bold"))+
  labs(title="FALL IN ULTRA TRAIL RUNNING PARTICIPANTS",
       subtitle = "The below data visualization displays the fall in the number of ultra trail runners due to COVID")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5))+
  geom_curve(aes(x = 2020, y = 16500, xend = 2021, yend = 17200),
             curvature = -1, angle = 75,
             color = "white", size = 0.8,
             arrow = arrow(length = unit(0.03, "npc"),
                           type = "closed",
                           ends = "both"))->yearwise

ggsave("yearwise.png",yearwise,width = 12,height = 8)

#fall in number of events
race%>%
  mutate(Year=as.Date(date)%>%
           year())%>%
  distinct()%>%
  group_by(Year)%>%
  count(event)%>%
  summarise(Total=sum(n))%>%
  data.frame()->noofevents

ggplot(noofevents,aes(x=Year,y=Total, label=Total))+
  geom_segment(aes(x=Year,xend=Year,y=0,yend=Total),colour="#f4f4f8")+
  geom_point(size=12, color="#fe4a49", fill=alpha("#fed766", 0.8), alpha=0.9, shape=21, stroke=2)+
  geom_text(color = "black", fontface="bold",size = 3)+
  scale_x_continuous(breaks = c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(limits=c(0,250),breaks=c(50,100,150,200,250))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "white",face="bold"))+
  labs(title="FALL IN NUMBER OF ULTRA TRAIL RUNNING EVENTS CONDUCTED",
       subtitle = "The below data visualization displays the fall in the number of ultra trail running events conducted during COVID")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5))+
  geom_curve(aes(x = 2020, y = 105, xend = 2021, yend = 93),
             curvature = -1, angle = 75,
             color = "white", size = 0.8,
             arrow = arrow(length = unit(0.03, "npc"),
                           type = "closed",
                           ends = "both"))->noevents

ggsave("noevents.png",noevents,width = 12,height = 8)

#country-wise plot
data.frame(race)%>%
  select(event,race,country,date,participation,participants)%>%
  mutate(Year=as.Date(date)%>%
           year())%>%
  distinct()%>%
  filter(Year==2021|Year==2020|Year==2019)%>%
  group_by(country,Year)%>%
  summarise(n=sum(participants))%>%
  data.frame()->countryyear

countryyear[-c(1,25,29,53,58,67,68),]->countryyear

ggplot(countryyear, aes(x=Year, y=n))+
  geom_segment(aes(x=Year,xend=Year,y=0,yend=n),colour="white")+
  geom_point(size=1, color="#fe4a49", fill=alpha("#fed766", 0.8), alpha=0.9, shape=21, stroke=2)+
  facet_wrap(~country,ncol = 8)+
  scale_x_continuous(breaks=c(2019,2020,2021))+
  scale_y_continuous(limits=c(0,10000),breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour="white",fill=NA),
        axis.text = element_text(colour = "white",face="bold"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour = "white",face = "bold",size=12))+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="DIP IN ULTRA TRAIL RUNNERS ACROSS COUNTRIES",
       subtitle = "The below data visualization shows the dip in the number of ultra trail runners during COVID across various countries")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5))->countries

ggsave("countries.png",countries,width = 20,height = 15)

#events - team and solo
data.frame(race)%>%
  select(event,race,country,date,participation,participants)%>%
  mutate(Year=as.Date(date)%>%
           year())%>%
  mutate(participation=recode(participation,"solo"="Solo","Solo"="Solo","team"="Team","relay"="Relay"))%>%
  distinct()%>%
  filter(Year==2021|Year==2020|Year==2019)%>%
  group_by(participation,Year)%>%
  summarise(n=sum(participants))%>%
  arrange(Year,.by_group = TRUE)%>%
  data.frame()->part

part[-1,]->part
#horizontal comparison lollipop
part
part%>%
  select(Year,participation,n)%>%
  spread(participation,n)->part

part[is.na(part)]<-0

ggplot(part) +
  geom_segment(aes(x=Year, xend=Year, y=Solo, yend=Team), color="white") +
  geom_point( aes(x=Year, y=Solo, colour="Solo"), size=3 ) +
  geom_point( aes(x=Year, y=Team, colour="Team"),size=3 ) +
  scale_x_continuous(breaks=c(2019,2020,2021))+
  scale_color_manual(values=c("#fe4a49","#fed766"), guide  = guide_legend())+
  scale_y_continuous(limits=c(0,40000),breaks=c(0,5000,10000,15000,20000,25000,30000,35000,40000))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "white",face="bold"),
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        legend.text = element_text(colour="white",face = "bold",size=12),
        legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour = "white",face = "bold",size=12))+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="FALL IN PARTICIPATION IN TEAM AND SOLO ULTRA TRAIL EVENTS",
       subtitle = "The below data visualization shows the dip in participation in team events during COVID")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5))->teamandsolo

ggsave("teamandsolo.png",teamandsolo,width = 8,height = 6)

#male and female participation
race%>%
  mutate(Year=as.Date(date)%>%
           year())%>%
  left_join(ultra_rankings)%>%
  distinct()%>%
  filter(Year==2021|Year==2020|Year==2019)%>%
  filter(!is.na(gender))%>%
  group_by(gender,Year)%>%
  count(gender)%>%
  arrange(Year,.by_group = TRUE)%>%
  data.frame()%>%
  mutate(gender=recode(gender,"M"="Men","W"="Women"))->gender

gender%>%
  select(Year,gender,n)%>%
  spread(gender,n)->gender

ggplot(gender) +
  geom_segment(aes(x=Year, xend=Year, y=Men, yend=Women), color="white") +
  geom_point( aes(x=Year, y=Men, colour="Men"), size=3 ) +
  geom_point( aes(x=Year, y=Women, colour="Women"),size=3 ) +
  scale_x_continuous(breaks=c(2019,2020,2021))+
  scale_color_manual(values=c("#fe4a49","#fed766"), guide  = guide_legend())+
  scale_y_continuous(limits=c(0,25000),breaks=c(0,5000,10000,15000,20000,25000))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "white",face="bold"),
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        legend.text = element_text(colour="white",face = "bold",size=12),
        legend.title = element_blank(),
        legend.position = "top",
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour = "white",face = "bold",size=12))+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="FALL IN MEN AND WOMEN ULTRA TRAIL RUNNERS",
       subtitle = "The below data visualization shows the dip in men and women ultra trail runners during COVID")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5))->genderc

ggsave("gender.png",genderc,width = 8,height = 6)

#top events conducted
race%>%
  mutate(Year=as.Date(date)%>%
           year())%>%
  filter(Year==2021|Year==2020|Year==2019)%>%
  select(event,race,Year,participants)%>%
  distinct()%>%
  group_by(Year)%>%
  arrange(desc(participants),.by_group = TRUE)%>%
  slice_head(n=5)%>%
  select(Year,Event=event,Participants=participants)%>%
  data.frame()->topevents

ggplot(topevents, aes(Participants, reorder(Event,Participants), label=Participants)) +
  geom_segment(aes(x = 0, y = reorder(Event,Participants), xend = Participants, yend = reorder(Event,Participants)), colour="white") +
  geom_point(size=6, color="#fe4a49", fill=alpha("#fed766", 0.8), alpha=0.9, shape=21, stroke=2) +
  geom_text(colour="black",size=2, fontface="bold")+
  scale_y_discrete() +
  facet_wrap(~ Year, ncol=3,scales = "free_y")+
  scale_x_continuous(limits=c(0,3000),breaks=c(500,1000,1500,2000,2500,3000))+
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour="white",fill=NA),
        axis.text = element_text(colour = "white",face="bold"),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour = "white",face = "bold",size=12))+
  theme(panel.spacing = unit(1, "lines"))+
  labs(title="TOP EVENTS IN ULTRA TRAIL RUNNING DURING COVID",
       subtitle = "The below data visualization shows the five events that saw the highest number of participants during COVID")+
  theme(plot.title = element_text(colour = "white",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "white",size=11,hjust=0.5))->tops

ggsave("tops.png",tops,width = 20,height = 10)

#patchwork plot

yearwise|noevents->p1
p1/tops->p2
p2/countries->p3
teamandsolo|genderc->p4
p3/p4->p5

p5 + plot_annotation(
  title = 'ULTRA TRAIL RUNNING DURING COVID',
  subtitle = 'Participation in ultra trail running falls in 2020 and 2021 due to the coronavirus pandemic',
  caption = 'Data:BjnNowak-Github Repo via Tidy Tuesday | Design: @annapurani93',
  theme = theme(plot.title = element_text(size = 60, hjust=0.5, face="bold",colour = "white"),
                plot.subtitle = element_text(size = 40,hjust=0.5, face="bold",colour = "white"),
                plot.caption = element_text(size = 30, face="bold",colour="white"),
                plot.background = element_rect(fill="black"),
                panel.background = element_rect(fill="black")))->p6

ggsave("plottt1.png",p6,width = 30,height = 40)
ggsave("plottt1.pdf",p6,width = 30,height = 40)
