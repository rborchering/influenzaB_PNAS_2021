## Process CDC FluView Age-group vs. subtype data

## FluView by age and virus subtype downloaded 190821
## https://gis.cdc.gov/grasp/fluview/flu_by_age_virus.html

## load relevant files
## Load public health lab data aggregated by season (2015/2016 to 2019/2020)
ageSubtype <- read.csv("00-RawData/FluView_current/AgeViewByWeek.csv" 
                       , skip=0
                       , header=T
                       , stringsAsFactors = F)

colnames(ageSubtype)=c("year","week","ageGroup","A_H1","A_unableSubtype","A_H3","A_H1N1_2009","A_notSubtyped","BVic","BYam","B_unspecified","A_H3N2v")

ageSubtype <- ageSubtype %>% as.data.frame() %>%
  mutate(A_H1 = as.numeric(A_H1)
         , A_H1N1_2009 = as.numeric(A_H1N1_2009)
         , A_H3 = as.numeric(A_H3)
         , A_H3N2v = as.numeric(A_H3N2v)
#         , A_unableSubtype = as.numeric(A_unableSubtype)
#         , A_notSubtyped = as.numeric(A_notSubtyped)
         , BVic = as.numeric(BVic)
         , BYam = as.numeric(BYam)
         , B_unspecified = as.numeric(B_unspecified)
         , A_noSubtype = as.numeric(A_unableSubtype)+as.numeric(A_notSubtyped)
  ) 
ageSubtype <- ageSubtype %>% mutate(
  A_unableSubtype = NULL,
  A_notSubtyped = NULL
)


update.season=-1*as.numeric(ageSubtype$week<40)
ageSubtype <- ageSubtype %>% mutate(season=year+update.season)


source("02-Scripts/00-process-demographic.R")
# use the population for the year associated with the season name e.g. 2019 season uses
# CDC projected population size for 2019 (broken down by age groups)
colnames(age.pops)= c("season","ageGroup","pop")
ageSubtype <- merge(ageSubtype,age.pops,by=c("season","ageGroup"))

ageGroups= c("0-4 yr","5-24 yr","25-64 yr","65+ yr") #unique(temp$ageGroup)
names(ageGroups) = c("0-4 years","5-24 years","25-64 years","65+ years")
#ageGroup.labs=c("0-4 years","5-24 years","25-64 years","65+ years")


## season resolution plot all A subtypes and B lineages 
aggSubtypeSeason <- ageSubtype %>% group_by(season, ageGroup, pop) %>% summarize_all(sum) %>% #summarise_each(sum -pop) %>%
  mutate(week=NULL,year=NULL, total = A_H1+A_noSubtype+A_H3+A_H1N1_2009+BVic+BYam+B_unspecified+A_H3N2v) #
temp<-gather(aggSubtypeSeason, key="subtype",value="samples",-c(season,ageGroup,total,pop))  #year
#temp <- right_join(pop.sizes,temp,by=c("year","ageGroup")) # should this be year instad of season??
temp <- temp %>% filter(season >=2010)


## season resolution plot B lineages only
ageLineage <- ageSubtype %>% mutate(A_H1=NULL,A_H1N1_2009=NULL,A_H3=NULL,A_H3N2v=NULL,A_noSubtype=NULL)
ageLineage <- ageLineage %>% mutate(Victoria=BVic,Yamagata=BYam)
ageLineage <- ageLineage %>% mutate(BVic=NULL,BYam=NULL)
aggBSeason <- ageLineage %>% group_by(season, ageGroup, pop) %>% summarize_all(sum) %>% #summarise_each(sum -pop) %>%
  mutate(week=NULL,year=NULL, total = Victoria+Yamagata+B_unspecified) 
tempB<-gather(aggBSeason, key="subtype",value="samples",-c(season,ageGroup,total,pop))  #year
tempB<- tempB %>% filter(season >=2010)


