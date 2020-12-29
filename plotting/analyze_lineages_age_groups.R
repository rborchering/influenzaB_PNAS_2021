## Process CDC FluView Age-group vs. subtype data

## FluView by age and virus subtype downloaded 190821
## https://gis.cdc.gov/grasp/fluview/flu_by_age_virus.html

#' Choose color palette
marker = list(color = brewer.pal(11, "Spectral"))
marker$color <- marker$color[c(8,9,10)]#-2

tempB$ageGroup<-factor(tempB$ageGroup,levels=c("0-4 yr","5-24 yr","25-64 yr","65+ yr"))

## Reformatting B lineage by agegroup plot
p.per.capitaB <- (
  ggplot(tempB) + 
    aes(x=season, y=samples*100000/pop)+ #, group=paste(state,season_fac)) +
    ## timeseries
    #aes(x=week_mod, y=total_b/(total_a+total_b), color=week_mod, group=state) + 
    geom_col(aes(fill=subtype),color='black',width=1) + #, color='grey20')+
    ## show observations
    #geom_point(size=2,alpha=0.1,shape=21, color='transparent')+
    scale_fill_manual('Lineage', labels=c("unspecified", "Victoria", "Yamagata"), values=marker$color) +
    scale_x_continuous(expand=c(0,0),breaks = 2010:2019)+
    scale_y_continuous(expand=c(0,0))+
    #coord_fixed()+
    facet_wrap(~ ageGroup,strip.position = "right",nrow=4)+
    xlab("Season") +
    ylab("Positive Type B samples by age-group (per 100,000 individuals)") +
    gg.theme #theme_bw()
  #gg.xrot
)



