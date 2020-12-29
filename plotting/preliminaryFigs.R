## plotting defaults
## TODO: move to default script
A.color="#1268FF"
B.color="#3268FF"


xlim.ili <- range(ili.clean$date)

## boxplot of proportion by week, split by type
## TO DO: filter by season, have main boxplot show distribution prior to 2019 season and add 2019 sesason separately
p.prop.box <- (
    ## TODO: is logic correct?
    ggplot(pos.type, aes(x=week_fac, y=prop))
    + facet_grid(type~.)
    + geom_boxplot(show.legend=FALSE,alpha=0.3) #colour=A.color,
    + scale_y_continuous(name="Prop pos in each week by state")
    + xlab('Epidemic week')
    + theme_bw()
)


p.sample <- (
    ggplot(ili.wide)
    + aes(x=date, y=state, fill=log10(total)#total_a/total_specimens#log10(total_a)
    )
    + facet_grid(type~., scales='fixed')
    + geom_tile()
    + geom_vline(xintercept=2011:2020, linetype="solid", color="black", size=0.5)
    + gg.scale.date(lim=xlim.ili)
    + scale_fill_continuous('Positive\nsamples',breaks=0:3,labels=c(1,10,100,1000))
    + xlab('')
    + ylab('')
    + theme_bw()
)

## line plot for total positive samples
p.positive.national <- (
  ili.wide %>% 
  group_by(date,type) %>% 
  summarize(nat.total=sum(total,na.rm=T)) %>% 
  ggplot() +
    aes( x = date,
         y = nat.total,
         #linetype = type,
         color = type
    )+
    geom_line()+
    gg.scale.date(lim=xlim.ili, position="top")+
    scale_y_continuous(expand=c(0,0))+
    #scale_y_continuous(expand=c(0,0.1,0,0))+
    scale_color_manual('',values=gg.col.type)+
    xlab('') +
    ylab('Positive\nsamples') +
    ## see plotting/setup.R for defaults
    gg.theme +
    gg.leg.title.blank
)

## just proportion b
p.prop.b <- (
  ggplot(ili.clean) +
  aes( x = date,
        y = state_lat,
        ## 0/0 as NA
        fill = total_b/(total_a+total_b)
        ## 0/0 as 0
        #fill = total_b/(total_a+total_b+1e-6)
  )+
  #facet_grid(hhs ~ ., space='free_y', scales='free_y')+
  geom_tile()+
  #geom_tile(aes(color=NA))+
  geom_vline(xintercept=2011:2020, linetype="solid", color="black", size=1) +
  scale_fill_continuous('Proportion\nType B', breaks=seq(0,10,by=2)/10, guide=gg.colorbar, na.value=gg.col.na) +
  ## add NAs?
  ## https://stackoverflow.com/questions/42365483/add-a-box-for-the-na-values-to-the-ggplot-legend-for-a-continous-map
  #scale_colour_manual(values=NA) +
  #guides(colour=guide_legend("No data", override.aes=list(colour="grey50"))) +
  gg.scale.date(lim=xlim.ili)+
  ## fix
  with(ili.clean, scale_y_discrete(breaks=state_lat,labels=state_abb_lat))+
  #ggtitle("Prop samples positive for Type B") + 
  xlab('Date') +
  #xlab('') +
  #ylab('') +
  ylab('State\n') +
  gg.theme +
  gg.panel.pack
)

## total specimens, log scale
p.specimens <- (
    p.prop.b 
    %+% aes(fill = total_specimens)
      + scale_fill_gradient(name="Tested\nsamples",trans="log", na.value=gg.col.na, breaks=c(10,100,1000),
                            guide=gg.colorbar) #('log10\nSamples')
      #+ ggtitle("number of tested samples")
)


## Find the proportion of states reporting values for positive A samples and positive B ssamples each week
dat.prop.states <- (
  ili.clean %>%
  group_by(date) %>%
  summarize(
    med.prop.b = median(total_b/(total_a+total_b),na.rm = T),
    prop.states=sum(!is.na(total_a) & !is.na(total_b >=0))/length(good.states),
    ## proportion with >0 positive samples
    prop.states.pos=sum(rowSums(cbind(total_a, total_b), na.rm=T)>0)/length(good.states),
    med.prop.plot = (prop.states.pos>0.5)
  ) %>%
  mutate(
    prop.quant = (prop.states.pos>quantile(prop.states.pos, 0.25))
  )
)

## line plot for median proportion b for each week across states
## gray and black lines indicat that the proportion of reporting a value for both total_a and total_b
## was below or >= 80% respectively (Note this is the % out of 'good states')
p.median.prop.b <- (
    dat.prop.states %>%
    ggplot() +
    aes(x = date, y=med.prop.b, fill=med.prop.plot) +
    geom_col() +
    scale_fill_manual(
        # 'Prop states reporting (per week):', 
        # labels =c('<80%', '>= 80%'), 
        'States with\npositive samples', 
        labels =c(parse(text="''<50*'%'"), parse(text="''>=50*'%'")), 
        values=c("grey60","black")
    ) +
    gg.scale.date(lim=xlim.ili)+
    scale_y_continuous(expand = c(0,0))+
    xlab('') +
    ylab('Proportion\n Type B') +
    gg.theme
    #gg.leg.title.blank +
)
