## Long-form by type 
temp.long <- (
    ili.clean
    %>% select(state, season, date_num, week, percap_a, percap_b)
    %>% gather('type', 'total', -state:-week)
)

## dplyr summarize() fails here for unknown reason
state.season.peaks <- as.data.table(temp.long)[,
    by=.(state,season,type),
    j=list(peak.ref=(
        week[which.max(total)]
        %.>% mk.shift(.)
    ))
]

#plot(peakA.week.dat$week_mod,peakA.week.dat$total_a)
#plot(peakA.week.dat$peakAref,peakA.week.dat$total_a)

.types <- make.names(c('percap_a', 'percap_b'))
## for all pairwise combinations of types
## shift type_dat data to peak week of type_peak
ili.shift <- lapply(.types, function(type_peak) (
    lapply(.types, function(type_dat) (
        ## grab peaks
        subset(state.season.peaks, type==type_peak, select=-type)
        ## join to data
        %>% left_join(
            subset(temp.long, type==type_dat),
            by=c('state', 'season')
        )
        %>% mutate(
            plot.week=mk.shift(week)-peak.ref
        )
    ))
    %>% rbindlist()
    ## combine, already has type col
)) %.>% rbindlist(., idcol='peak.type')

#temp=ili.shift%>% filter(state=="Alabama")
#plot(temp$plot.week,temp$total_a)

# For each week of the year, find quantiles for the proportion of positive samples of type A and separately
# for the proportion of positive samples of type B 
## use data.table
## for dt[, by=, j= ], by= approx group_by, j= approx summarize()
## j= allows for vector-valued returns
.prob <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

## temp object, overwrite
## process all prior seasons & 2019 separately
peak.quant <- list(
    prior=subset(ili.shift, season<2019),
    last=subset(ili.shift, season==2019)
)
## use data.table syntax (summarize won't work)
## filter rows
peak.quant <- lapply(peak.quant, function(dat) (
    dat[
         ## group_by
         by = .(type, peak.type, plot.week),
         ## summarize
         j = list(
           quant=quantile(total, probs=.prob, na.rm=T),
           prob=paste0('prob', .prob)
         )
    ]
    ## spread
    %.>% dcast(., ... ~ prob, value.var='quant')
    %.>% mutate(., 
        ## add col w/reverse factor order
        ftype=factor(type, 
            levels=unique(type), 
            labels=sub('percap_(.)', 'Type \\U\\1', unique(type), perl=T)
        ),
        ftype_rev=factor(ftype, levels=rev(levels(ftype)))
    )
))

## combine into single object
peak.quant.all <- (
    rbindlist(peak.quant, idcol='period')
    %.>% within(., 
        ## Facet labels
        period <- factor(period, 
            levels=c('prior','last'),
            labels=c('2010-2018 seasons', '2019 season')
        )
    )
)
