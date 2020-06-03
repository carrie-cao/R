# SQL and dplyr with baseball examples

library(Rcpp)
library(Lahman)
require('dplyr')
a=Batting%>%
  filter(AB >500& (HR >=50| SO<80))%>%
  select(playerID,HR,yearID)%>%
  arrange(desc(HR))
head(a)
#sum,mean,max,min
Batting%>%
  group_by(playerID)%>%
  summarize(career_HR=sum(HR, na.rm=TRUE))%>%
  arrange(desc(career_HR))

Batting%>%
  group_by(playerID)%>%
  summarize(avgSeason=round(mean(H,na.rm = TRUE),2))%>%
  arrange(desc(avgSeason))

Batting%>%
  group_by(playerID)%>%
  summarize(max_c=max(HR), min_c=min(SO))%>%
  arrange(desc(max_c))

Batting%>%
  group_by(playerID)%>%
  summarize(number=n())
  
Batting%>%
  filter(AB>=400)%>%
  group_by(playerID)%>%
  summarize(min_s=min(SO, na.rm=TRUE))%>%
  filter(min_s <20)%>%
  arrange(desc(min_s))
#mutate
m=Batting%>%
  filter(AB>=400)%>%
  mutate(battingavg=round(H/AB,3))%>%
  select(playerID,battingavg,yearID)%>%
  arrange(desc(battingavg))
head(m)

Batting%>%
  group_by(playerID)%>%
  summarize(career_H=sum(H,na.rm=TRUE), career_AB=sum(AB,na.rm=TRUE))%>%
  filter(career_AB>1000)%>%
  mutate(career_BA=round(career_H/career_AB,3))%>%
  select(playerID,career_BA)%>%
  arrange(desc(career_BA))

Batting%>%
  group_by(playerID)%>%
  summarize(career_H=sum(H,na.rm=TRUE), career_AB=sum(AB,na.rm=TRUE))%>%
  filter(career_AB>1000)%>%
  mutate(career_BA=round(career_H/career_AB,3))%>%
  select(playerID,career_BA)%>%
  arrange(desc(career_BA))

#innerjoin
a=Batting%>%
  filter(playerID=="ruthba01"| playerID=="aaronha01")
a=inner_join(a,Master, by="playerID")
head(a)

a=Batting%>%
  select(playerID, teamID,yearID,HR)
  
a=inner_join(a,Master, by="playerID")%>%
  select(First_name=nameFirst,Last=nameLast,teamID,yearID,HR)
head(a)

#join more than one field
a=Batting%>%
  filter(playerID=="ruthba01")%>%
  select(playerID, teamID,yearID,HR)

a=inner_join(a,Teams, by=c('teamID','yearID'))%>%
  select(playerID,name,teamID,yearID,HR.x)
head(a)

#join 3 tables
a=Batting%>%
  filter(playerID=="ruthba01")%>%
  select(playerID, teamID,yearID,HR)
b=inner_join(a,Master, by='playerID')%>%
  select(nameFirst,nameLast,teamID,yearID,HR)

c=inner_join(b,Teams, by=c('teamID','yearID'))%>%
  select(nameFirst,nameLast,name,yearID,HR.x)
head(c)

#grouping and joining
c=inner_join(Batting, Master,by='playerID')%>%
  group_by(playerID)%>%
  summarize(First_name=nameFirst[1],Last_name=nameLast[1],career_HR=sum(HR,na.rm=TRUE))%>%
  select(First_name,Last_name,career_HR)
head(c)

