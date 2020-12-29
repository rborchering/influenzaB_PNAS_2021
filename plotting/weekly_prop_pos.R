## proportion of the cases in the season so far
## See also 02-Scripts/prop.by.type.R

#state.choice="Louisiana"
## Important plot ##
## weekly quantiles over all years split by type
## with 2019 median overlain
p.important <- (
    ggplot(prop.quant)
    + facet_grid(type~.)
    + aes(x=week_fac, group=type, fill=type)
    #+ aes(x=week_fac, group=type, colour=type, fill=type)
    + geom_ribbon(aes(ymin=prob0.1, ymax=prob0.9),alpha=.1)
    + geom_ribbon(aes(ymin=prob0.25, ymax=prob0.75),alpha=0.5)
    ## full median
    + geom_line(color='grey50', alpha=0.8, linetype=1, aes(y=prob0.5))
    ## 2019 median
    + geom_line(
        data=prop.med.2019,
        aes(y=medianProp),
        size=1 #linetype=4
    )
    + scale_fill_manual('', values=gg.col.type)
    + guides(fill=guide_legend('none'))
    + xlab('Week')
    ## FIXME?
    + ylab('Proportion positive (Median + ...Quantiles)')
    + gg.theme
)

p.full.season <- (
  ggplot(prop.quant.pre2019)
  + facet_grid(type~.)
  + aes(x=week_fac, group=type, fill=type)
  #+ aes(x=week_fac, group=type, colour=type, fill=type)
  + geom_ribbon(aes(ymin=prob0.1, ymax=prob0.9),alpha=.1)
  + geom_ribbon(aes(ymin=prob0.25, ymax=prob0.75),alpha=0.5)
  + geom_line(size=1.25, aes(y=prob0.5))
  + scale_fill_manual('', values=gg.col.type)
  + guides(fill=guide_legend('none'))
  + xlab('Week')
  + ylab('Proportion positive (Median + ...Quantiles)')
  + gg.theme
)
