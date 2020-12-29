## ----IL-ILI,echo=FALSE----------------------------------------------------------------------------
#'
#' Load subtype-specific data from FluView

library(ggplot2)

#load("01-ProcessedData/ILIplus_all_states.Rdata")

state.names=unique(ILIplus$state)
#not.these.states = c("Virgin Islands","Puerto Rico","New York City","District of Columbia")

#these.states=state.names[-which(state.names %in% not.these.states)]

#ILIplus <- ILIplus %>% mutate(index=which(state.names==state))
library("gridExtra")

x.min=min(ILIplus$date.num)
x.max=max(ILIplus$date.num)
year.breaks=seq(2011,2019,by=2)
minor.breaks=seq(2012,2020,by=2)

#ILIplus <- ILIplus %>% arrange(desc(latitude))
#lat.states=unique(ILIplus$state)

#ILIplus <- ILIplus %>% filter(state %in% these.states) %>% arrange(desc(latitude))
#new.state.lat.sort <-ILIplus



totalA <- ggplot(
  ILIplus %>% group_by(state) %>% 
    filter(state %in% these.states) %>% arrange(desc(latitude))
  ,aes(
    x = date.num
    ,y = index_lat
    ,fill = log10(total_a)#total_a/total_specimens#log10(total_a)
  )) + 
  geom_tile()+
#  geom_vline(xintercept=2011:2020, linetype="solid", color="black", size=0.5)+
  scale_x_continuous(limits=c(x.min,x.max),expand=c(0,0),
                     breaks=year.breaks,minor_breaks=minor.breaks)+
  scale_y_discrete(name="state",expand=c(0,0),labels="state.abb")+
#  scale_y_discrete(name="state",expand=c(0,0),breaks="index_lat",labels="state.abb")+
  scale_fill_continuous(breaks=0:3,labels=c(1,10,100,1000))+#
  theme(legend.title=element_blank())+
  ggtitle("number of positive samples type A")

names(state.lat.long)=c("state","state.abb2","latitude","longitude","index_lat")
totalB <- ggplot(
  ILIplus %>% group_by(state) %>% 
    filter(state %in% these.states) 
  ,aes(
    x = date.num
    ,y = index_lat
    ,fill = log10(total_b)#total_a/total_specimens#log10(total_a)
  )) + 
  geom_tile()+
  geom_vline(xintercept=2011:2020, linetype="solid", color="black", size=0.5)+
  scale_x_continuous(limits=c(x.min,x.max),expand=c(0,0),
                     breaks=year.breaks,minor_breaks=minor.breaks)+
 # scale_y_discrete(expand=c(0,0),labels=state.abb)+
  scale_y_continuous(name="state",expand=c(0,0),breaks="index_lat",labels="state.abb2")+
  scale_fill_continuous(breaks=0:3,labels=c(1,10,100,1000))+#
  theme(legend.title=element_blank())+
  ggtitle("number of positive samples type B")


propB <- ggplot(
  ILIplus %>% group_by(state) %>% 
    filter(state %in% these.states)
  ,aes(
    x = date
    ,y = index
    ,fill = total_b/(total_a+total_b)#total_b/total_specimens # log10(total_b)
  )) + 
  geom_tile()+
  geom_vline(xintercept=2011:2020, linetype="solid", color="black", size=1) +
  scale_x_continuous(limits=c(x.min,x.max),expand=c(0,0),
                     breaks=year.breaks,minor_breaks=minor.breaks)+
  scale_y_continuous(name="state",expand=c(0,0),breaks=1:c(length(state.lat.long$latitude)),labels=rev(unique(state.lat.long$state.abb)))+
  theme(legend.title=element_blank())+
  ggtitle("proportion of positive samples type B")

total_specimens <- ggplot(
  ILIplus %>% group_by(state) %>% 
    filter(state %in% these.states)
  ,aes(
    x = date
    ,y = index
    ,fill = log10(total_specimens) # log10(total_b)
  )) + 
  geom_tile()+
  geom_vline(xintercept=2011:2020, linetype="solid", color="black", size=1) +
  scale_x_continuous(limits=c(x.min,x.max),expand=c(0,0),
                     breaks=year.breaks,minor_breaks=minor.breaks)+
  scale_y_continuous(name="state",expand=c(0,0),breaks=1:c(length(state.lat.long$latitude)),labels=rev(unique(state.lat.long$state.abb)))+
  scale_fill_continuous(breaks=0:3,labels=c(1,10,100,1000))+#
  theme(legend.title=element_blank())+
  ggtitle("number of tested samples")


grid.arrange(totalA,totalB,ncol=1)
grid.arrange(propB,total_specimens,ncol=1)



these.states="Georgia"

ggplot(ILIplus %>%
               filter(state %in% these.states)
       ,aes(x=date,y=total_a),group_by(state)) + #c("Arizona","Massachusetts","Illinois","California","Iowa")
  geom_line() +
  geom_line(aes(date,total_b),col='blue')+
  geom_vline(xintercept=2016:2020, linetype="dashed", color="gray", size=1) 
  facet_wrap(vars(state),nrow=3,scales = "free_y") 
  
  
  #eventually group by region instead of alphabetically
  num.states=6
  start.index=38
  these.states = c("Arizona","Massachusetts","Illinois","California","Iowa","Missouri")
  these.states = unique(ILIplus$state)[start.index:(start.index+num.states-1)]
  
  #New Jersey not working
  
  good.states=c("Maryland","Minnesota","Montana","Nebraska","New York","Pennsylvania")[1:num.states]
  #interesting.states=c("Kansas","Missori)
  
  these.states=good.states
  ggplot(ILIplus #%>%
         #        filter(state %in% these.states)
         ,aes(x=date,y=total_specimens),group_by(state)) + #c("Arizona","Massachusetts","Illinois","California","Iowa")
    geom_line() +
    geom_line(aes(date,total_specimens*percent_positive/100),col='blue')+
    geom_vline(xintercept=2011:2020, linetype="dashed", color="gray", size=1) #+
  #  facet_wrap(vars(state),nrow=3,scales = "free_y") 
  
  ggplot(ILIplus,aes(x=date,y=total_specimens,alpha=.0000001),group_by(state)) + #c("Arizona","Massachusetts","Illinois","California","Iowa")
    geom_line() +
    geom_line(aes(date,total_specimens*percent_positive/100,col='blue',alpha=.1))+
    geom_vline(xintercept=2011:2020, linetype="dashed", color="gray", size=1) +
    theme(legend.position = "none")
  
  
  ggplot(ILIplus,aes(x=date,total_specimens*percent_positive/100,col='blue',alpha=.001),group_by(state)) + #c("Arizona","Massachusetts","Illinois","California","Iowa")
    geom_line() +
    geom_line(aes(date,y=total_specimens,alpha=.0000001))+
    geom_vline(xintercept=2011:2020, linetype="dashed", color="gray", size=1) +
    theme(legend.position = "none")
  
  
  ggplot(ILIplus %>%
           filter(state %in% "California")
         #filter(state %in% c("Arizona","Massachusetts","Illinois","California","Iowa","Missouri"))
         ,aes(x=date,y=log10(total_specimens))) + #c("Arizona","Massachusetts","Illinois","California","Iowa")
    geom_line() +
    geom_line(aes(date,log10(total_specimens*percent_positive/100)),col='blue')+
    geom_vline(xintercept=2016:2020, linetype="dashed", color="gray", size=1) +
    facet_wrap(vars(state),nrow=3,scales = "free_y") 
  
  