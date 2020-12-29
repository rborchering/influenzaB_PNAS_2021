## filter out the data from this season (will be plotted separately)
## need to chohose cutoff in a more principled way

## gather / reshape: type as factor
pos.type <- ( 
    ili.list$early
    # date.num cutoff will change based on start.week
    # should we display results for week 53?
    %>% filter(week != 53)
    %>% select(state, year, date_num, week_fac, prop_a, prop_b)
    %>% gather(key='type', value='prop', -state:-week_fac)
    ## nice format for type col 
    %>% mutate(type=paste0('Type ', toupper(sub('prop_','', type))))
)

# For each week of the year, find quantiles for the proportion of positive samples of type A and separately
# for the proportion of positive samples of type B 
## use data.table
## for dt[, by=, j= ], by= approx group_by, j= approx summarize()
## j= allows for vector-valued returns
.prob <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

prop.quant <- (
    as.data.table(pos.type)
    ## filter rows
    %.>% .[(date_num <= 2019.7), 
        ## group_by
        by = .(week_fac, type),
        ## summarize
        j = list(
            quant=quantile(prop, probs=.prob, na.rm=T),
            prob=paste0('prob', .prob)
        )
    ]
    ## spread
    %.>% dcast(., ... ~ prob, value.var='quant')
)

## as above, for this year
## just median (summarize works fine)
prop.med.2019 <- (
    pos.type
    %.>% filter(., date_num >= 2019.7) 
    # %.>% browser()
    %.>% group_by(., type, week_fac)
    %.>% summarize(.,
        medianProp = quantile(prop,probs=0.5,na.rm=T)
    )
    %.>% filter(., !is.na(medianProp))
)

pos.type.full <- ( 
    ili.list$full
    # date.num cutoff will change based on start.week
    # should we display results for week 53?
    %>% filter(week != 53)
    %>% select(state, year, date_num, week_fac, prop_a, prop_b)
    %>% gather(key='type', value='prop', -state:-week_fac)
    ## nice format for type col 
    %>% mutate(type=paste0('Type ', toupper(sub('prop_','', type))))
)

prop.quant.pre2019 <- (
    as.data.table(pos.type.full)
    ## filter rows
    %.>% .[(date_num <= 2019.7), 
           ## group_by
           by = .(week_fac, type),
           ## summarize
           j = list(
               quant=quantile(prop, probs=.prob, na.rm=T),
               prob=paste0('prob', .prob)
           )
           ]
    ## spread
    %.>% dcast(., ... ~ prob, value.var='quant')
)
