## deprecated
## xian 2020-05-20 code does not run after package upgrade
## Plotting functions for CDC FluView age-group and subtype data
## FluView by age and virus subtype downloaded 200204
## https://gis.cdc.gov/grasp/fluview/flu_by_age_virus.html

#theme_set(theme_classic(base_size = 14))

#extract legend function
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]] 
  return(legend)}

#' Choose color palette
marker = list(color = brewer.pal(11, "Spectral"))
marker$color <- marker$color[-c(3,7,11)]#-2
font.scale=18


############################################################################################
## Maybe this section should be moved to a data processing file, instead of a plotting file?
############################################################################################
#select column names that correspond to influenza a and b subtype counts
ab_cols=(startsWith(colnames(ageSubtype),"a_") + startsWith(colnames(ageSubtype),"b_"))*(1:length(colnames(ageSubtype)))
subtype.cols = colnames(ageSubtype)[ab_cols]

# find average population size for each age-group in a season
pop.vals <- ageSubtype %>% group_by(season,age_group) %>% summarize_at("pop",mean) 

season.week$start
#c(1:53,c(40:53,1:season.week$end))
aggSubtype <- lapply(c(season.week$start-1,season.week$end),function(x){
  weeks = c(c(40:53),1:x)
  aggSubtype <- ageSubtype %>%
    filter(week_fac %in% weeks) %>%
    group_by(season, age_group) %>% 
    summarize_at(subtype.cols,sum) %>% #summarise_each(sum -pop) %>%
    mutate(total = a_total + b_total,
           a_total= NULL,
           b_total= NULL)
    aggSubtype <- right_join(aggSubtype,pop.vals, by = c("season","age_group")) 
    aggSubtype.wide <- gather(aggSubtype, key="subtype",value="samples",-c(season,age_group,total,pop)) %>% 
      filter(subtype %in% subtype.cols) %>%
      mutate(samples_per_100k = samples*100000/pop)
  }                
)
names(aggSubtype)=c("full","current")

# ## aggregate subtype counts by season resolution
# aggSubtypeSeason <- ageSubtype %>% 
#   group_by(season, age_group) %>% 
#   summarize_at(subtype.cols,sum) %>% #summarise_each(sum -pop) %>%
#   mutate(total = a_total + b_total,
#          a_total= NULL,
#          b_total= NULL) #
# aggSubtypeSeason <- right_join(aggSubtypeSeason,pop.vals, by = c("season","age_group"))
# 
# ## aggregate subtype counts by season resolution up through current available week
# aggSubtypeCurrent <- ageSubtype %>% filter(week_fac %in% c(40:53,1:season.week$end)) %>% 
#   group_by(season, age_group) %>% 
#   summarize_at(subtype.cols,sum) %>% 
#   mutate( total = a_total + b_total,
#           a_total= NULL,
#           b_total= NULL) #
# aggSubtypeCurrent <- right_join(aggSubtypeCurrent,pop.vals, by = c("season","age_group"))
# 
# 
# 
# ## expand aggSubtype data so that subtype is a column
# aggSeason.wide<-gather(aggSubtypeSeason, key="subtype",value="samples",-c(season,age_group,total,pop)) %>% 
#   filter(subtype %in% subtype.cols) %>%
#   mutate(samples_per_100k = samples*100000/pop)
# 
# aggCurrent.wide<-gather(aggSubtypeCurrent, key="subtype",value="samples",-c(season,age_group,total,pop)) %>%
#   filter(subtype %in% subtype.cols) %>%
#   mutate(samples_per_100k = samples*100000/pop,
#          a_total=NULL,
#          b_total=NULL)
# 
############################################################################################


#temp=aggSeason.wide
#temp=aggCurrent.wide
# to look at years before the H1N1 2009 pandemic use filter(season<2008)

age_groups=unique(ageSubtype$age_group)
names(age_groups) = c("0-4 years","5-24 years","25-64 years","65+ years")

# ## function used to generate each panel of the age-group specific data
# ggfun <- function(dat,ageGrp){
#     g <- ggplot(dat%>% filter(age_group==ageGrp), aes(season,samples/total)) # + scale_fill_brewer(palette = "Spectral")
# 
#   g + geom_col(aes(fill=subtype),col="black",width = 1) +
#     scale_fill_manual(values = marker$color) +
#     theme(axis.text.x = element_text(angle=65, vjust=0.6),text = element_text(size=font.scale)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     scale_x_continuous(expand = c(0, 0)) +
#     ylab(ageGrp)+xlab("")
# }

# upper limit for samples per 100,000 comparison between full and current
y.upper = max(
  rbind(aggSubtype[[1]] %>% filter(season>2009),aggSubtype[[2]]%>% filter(season>2009))$samples_per_100k
)

ggfun <- function(dat,temp.prop){
  if(temp.prop==T){
    g <- ggplot(dat, aes(season,samples/total))+ylab("proportion of positive samples")+ 
    scale_y_continuous(expand = c(0, 0))+
      ylab("proportion of positive samples")
    
  }else{
    dat <- dat %>% filter(season>2009)
        g <- ggplot(dat, aes(season,samples_per_100k)) +
        scale_y_continuous(expand = c(0, 0),limits=c(0,1.2*y.upper))+
          ylab("samples per 100,000 population")
  }
  
  g + geom_col(aes(fill=subtype),col="black",width = 1) +
    scale_fill_manual(values = marker$color) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6),text = element_text(size=font.scale),
          legend.position = "none", plot.title = element_text(hjust=0.5)) +
    scale_x_continuous(expand = c(0, 0)) +
    xlab("season")+
    facet_wrap(age_groups,ncol=1)
}

p.agg.current.prop= ggfun(aggSubtype[[2]],temp.prop=T)  +ggtitle(paste0("Early season (through Epiweek ",season.week$end,")"))
p.agg.full.prop = ggfun(aggSubtype[[1]],temp.prop=T) +ggtitle(paste0("Entire season"))
p.agg.current= ggfun(aggSubtype[[2]],temp.prop=F)  +ggtitle(paste0("Early season (through Epiweek ",season.week$end,")"))
p.agg.full = ggfun(aggSubtype[[1]],temp.prop=F)  +ggtitle(paste0("Entire season"))

## TO DO: add legend to below plots
## mylegend <- g_legend(p.agg.current) not working, also need to make this functional so plots are automatically generated in the pdf

#p.agg.prop <- grid.arrange(p.agg.current.prop,p.agg.full.prop,ncol=3,widths=c(2,2,0.75),legend=mylegend)
#p.agg <- grid.arrange(p.agg.current,p.agg.full,ncol=3,widths=c(2,2,0.75),legend=mylegend)

p.agg.prop <- grid.arrange(p.agg.current.prop,p.agg.full.prop,ncol=2,widths=c(2,2))
p.agg <- grid.arrange(p.agg.current,p.agg.full,ncol=2,widths=c(2,2))




#' #########################
#' ## weekly resolution plot
#' #########################
#' 
#' SubtypeWeek <- ageSubtype %>% #group_by(season, age_group) %>% summarise_all(sum) %>%
#'    mutate(total = A_H1+A_H3+A_H1N1_2009+A_noSubtype+BVic+BYam+B_unspecified+A_H3N2v)
#' 
#' 
#' 
#' g + geom_col(aes(fill=subtype),col="black",width = 1) +
#'   scale_fill_manual(values = marker$color) +
#'   theme(axis.text.x = element_text(angle=65, vjust=0.6),text = element_text(size=font.scale)) +
#'   scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
#'   scale_x_continuous(expand = c(0, 0)) +
#'   ylab("proportion of samples")
#' 
#' 
#' # # The year column is correct (has not been altered to reflect the season)
#' # # The season column should also be accurate
#' # # Possibly shouldn't plot using the year column. 
#' # update.season=1*as.numeric(SubtypeWeek$week<40)
#' # SubtypeWeek <- SubtypeWeek %>% mutate(season=season + update.season) #%>% mutate(season=NULL)
#' 
#' #SubtypeWeek <- SubtypeWeek %>% mutate(season=NULL)
#' 
#' #SubtypeWeek <- SubtypeWeek %>% filter(year <= 2020 & week <=32)
#' 
#' SubtypeWeek <- SubtypeWeek %>% mutate(none = if_else(total == 0, 0.5, 0))
#' SubtypeWeek <- SubtypeWeek %>% mutate(total = if_else(none == 0.5, 0.5, total))
#' 
#' temp <- gather(SubtypeWeek, key="subtype",value="samples",-c(week,year,age_group,total,pop))  
#' 
#' # There are some weeks > 53 in the dataset - something to do with the 2009 pandemic?
#' temp <- temp %>% filter(week<54) 
#'  
#' # why is week<10 important?
#' temp <- temp %>% mutate(time = if_else(week < 10, as.numeric(year + week/53)
#'                                        ,as.numeric(year + week/53))) 
#' 
#' temp5=temp %>% filter(age_group=="0-4 yr" & subtype=="A_H1N1_2009") %>% filter(year %in% c(2009,2010))
#' plot(temp5$total)
#' 
#' # something is strange with the time column. Why are there som values greater than 2020?
#' temp %>% filter(year==2019) %>% tail()
#' 
#' #' Sample for 0-4 years old age group 
#' #' Choose color palette
#' marker = list(color = brewer.pal(11, "Spectral"))
#' marker$color <- marker$color[-c(3,7)]#-2
#' font.scale=18
#' #' 
#' g <- ggplot(temp %>% filter(age_group == age_groups[4]) #"0-4 yr"
#'              , aes(time,samples/total)) #+ scale_fill_brewer(palette = "Spectral")
#' 
#' g + geom_col(aes(fill=subtype),col=NA,width = 1) +
#'   scale_fill_manual(values = c(marker$color,"#FFFFFF")) +
#'   theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
#'   scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
#'   scale_x_continuous(limits = c(2010,2020),expand = c(0, 0)) +
#'   ylab("proportion of samples")
#' 
