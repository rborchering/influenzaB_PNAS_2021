## aligned peak A weeks plot with B epidemics plotted below
## reference week to week everything will be aligned is the peak week in state a state for number of pos A samples

p.relative.peaks.self <- (
  subset(peak.quant$prior, peak.type==type)
  %>% ggplot()
  + aes(x=plot.week, fill=ftype)
  + geom_ribbon(aes(ymin=prob0.1, ymax=prob0.9),alpha=.1)
  + geom_ribbon(aes(ymin=prob0.25, ymax=prob0.75),alpha=0.5)
  + geom_line(aes(y=prob0.5))
  + geom_vline(xintercept=0, color='grey40', linetype=2)
  + facet_grid(ftype~.) #scales='free' #, scales='fixed'
  + scale_fill_manual('', values=gg.col.type)
  + coord_cartesian(xlim = c(-22,22), expand=F)
  #+scale_x_continuous(limits = c(-22,22), expand=c(0,0))
    + xlab('Week (relative to peak)')
    + ylab('')
    + gg.theme
    + theme(
        #panel.spacing=unit(0.8,'lines'),
        ## not possible to change vertical space between legend items
        ## see https://github.com/tidyverse/ggplot2/issues/3180#issuecomment-474030332
        legend.title = element_blank()
    )
)

p.relative.peaks.other <- (
    p.relative.peaks.self 
    %+% subset(peak.quant$prior, peak.type!=type)
    + facet_grid(ftype_rev~., scales='free_y') #
    + xlab("Week (relative to other type)")
)

.type.lev <- levels(peak.quant$prior$ftype)
## relative to A
p.peaks.from.a <- (
  p.relative.peaks.self 
  %+% subset(peak.quant$prior, peak.type=='percap_a')
  ## just include A in legend
  #+ scale_fill_manual('', values=gg.col.type, breaks=.type.lev[1], labels=.type.lev[1])
  ## ?? Relative to Type A Peak?
  + xlab('Week (relative to Type A peak)')
)

## relative to B
p.peaks.from.b <- (
  p.relative.peaks.self 
  %+% subset(peak.quant$prior, peak.type=='percap_b')
  + facet_grid(ftype~., scales='free')
  #+ scale_fill_manual('', values=gg.col.type, breaks=.type.lev[2], labels=.type.lev[2])
  + xlab('Week (relative to Type B peak)')
)

## Rework above as grid
## relative to A
p.peaks.main <- (
  p.relative.peaks.self 
  %+% subset(peak.quant.all, peak.type=='percap_a')
  + facet_grid(ftype ~ period, scales='free_y')
  + xlab('Week (relative to Type A peak)')
  + ylab("Positive samples per 100,000\n\n ")
)
