## seasonality plots
## see 02-Scripts/glmer.R

## how were these chosen?
## what about CA, Washington, TX, NY, Ohio?
temp.sub.state <- subset(ili.clean, grepl('York|Mass|Louis|Geor|Tex|Ariz', state))

p.mod.season <- (
    pred.season
    %.>% ggplot(.)
    + aes(x=week_mod, y=response, group=Type, color=Type, ymin=lower.CL, ymax=upper.CL)
    + geom_line()
    + geom_ribbon(alpha=0.2, color=NA, fill='grey20')
    + xlab('Week of influenza season')
    + ylab('Expected positive samples\nper 100,000 individuals')
    + scale_color_manual(name='Type', values=gg.col.type, labels= c("A","B"))
    + scale_x_continuous(expand=c(0,0))
    #+ facet_grid(type~.)
    + gg.theme
    + theme(
        legend.position=c(0.99,0.99),
        legend.justification=c(1,1)
    )
)

## everything
p.totals <- (
  ggplot(ili.clean) +
  aes(
    x=percap_a,y=percap_b,
    group=state,
    fill=week_mod, color=week_mod
  )+
  geom_path(size=0.3,alpha=0.2)+
  #geom_point(size=2,shape=1,alpha=0.5)+
  #geom_point(size=2,alpha=0.1,shape=21, color='transparent')+
  geom_abline(slope=1,intercept=0, linetype=3) +
  scale_x_sqrt() +
  scale_y_sqrt() +
  #scale_fill_gradient(low='blue',high='red')+
  gg.scale.week + 
  #scale_color_gradient(low='orange',high='purple')+
  #coord_fixed()+
  xlab(cap.percap.a) +
  ylab(cap.percap.b) +
  facet_wrap(season~.)+
  gg.theme +
  gg.leg.pos(c(0.7,0.15)) +
  theme(legend.direction='horizontal') +
  gg.xrot
)

## Keep this
## Seasonality of total A vs B, color by season
p.totals.by.state <- (
    ggplot(temp.sub.state) + 
    aes(x=percap_a, y=percap_b, color=season_fac, group=paste(state,season_fac)) +
    ## timeseries
    #aes(x=week_mod, y=total_b/(total_a+total_b), color=week_mod, group=state) + 
    geom_path(size=0.6, alpha=0.5) + #, color='grey20')+
    ## show observations
    #geom_point(size=2,alpha=0.1,shape=21, color='transparent')+
    geom_abline(slope=1,intercept=0, linetype=3) +
    scale_x_sqrt() +
    scale_y_sqrt() +
    scale_color_manual('Season', values=gg.col.season) +
    #coord_fixed()+
    facet_wrap(state~.)+
  xlab(cap.percap.a) +
  ylab(cap.percap.b) +
    theme_bw()
    #gg.xrot
)

p.totals.by.season <- (
    p.totals.by.state 
    + aes(color=state)
    + scale_color_manual('State', values=gg.col.state)
    + facet_wrap(~season)
    + guides(color=guide_legend(ncol=2))
    + gg.leg.outline
    + gg.leg.pos(c(0.75, 0.15))
)

p.totals.split <- (
    p.totals.by.state 
    + aes(color=week_mod)
    + facet_grid(season_fac~state) #, scales='free_x')
    + scale_color_manual(guide=gg.colorbar)
        #+ scale_color_gradient('Week', low='blue',high='red')
    + gg.scale.week
)

#############################
## distance from 1-to-1 line
#############################
.dist.ylab <- 'Type A - Type B:\nsamples per 100,000 residents'
#.dist.ylab <- 'IVA - IVB: pos samples per 100,000'

## distance from 1-1 line (per capita positive samples)
## Difference between the number of positive Type B and Type A samples 
## per 100,000 individuals. The estimated marginal mean (least-squares mean)
## is calculated along with the corresponding 95% confidence intervals
## (marginalized over states, conditioned on time).
p.totals.dist.base <- (
    pred.dist
    %>% ggplot()
    + aes(x=week_mod, y=emmean, group=season_fac, ymin=lower.CL, ymax=upper.CL)
    + geom_hline(yintercept=0, linetype=2, color='grey30')
    + geom_ribbon(alpha=0.2, color=NA, fill='grey10')
    + scale_x_continuous('Week of influenza season', expand=c(0,0))
    + ylab(.dist.ylab)
    + gg.theme 
)

p.totals.dist <- (
    p.totals.dist.base
    + aes(color=season_fac) 
    + geom_line(alpha=0.8, size=0.8)
    + scale_color_manual('Season', values=gg.col.season)
    + guides(color=guide_legend(ncol=1)) #2
    #+ gg.leg.outline
    #+ gg.leg.pos(c(0.78, 0.32))
)

## set limits to omit extreme points
#.dist.ylim <- with(pred.dist,
#    c(min(lower.CL), max(upper.CL))
#)
.dist.ylim <- with(ili.clean,
    quantile(percap_dist, c(0.01, 0.99), na.rm=T)
)

p.totals.dist.season <- (
    p.totals.dist.base
    + geom_line(alpha=0.8, size=0.5, color='blue')
    + geom_point(
        data=ili.clean, inherit.aes=F,
        aes(x=week_mod, y=percap_dist),
        alpha=0.12, shape=1, size=0.2
    )
    + facet_wrap(~season_fac, nrow=2)
    + scale_y_continuous(.dist.ylab, limits=.dist.ylim, expand=c(0.01,0,0.01,0))
)

## testable prediction!?
p.prop.density <- (
    ggplot(ili.clean)
    + aes(x=total_b/(total_a+total_b), color=season_fac, group=season)
    + geom_density()
    + scale_color_manual('Season', values=gg.col.season) ## need to check colors on this 
    + gg.theme
)

## Seasonality of total A vs B, by year
## proportion
not_used <- (
    ggplot(ili.clean)
    + aes(y=total_b/(total_a+total_b),x=week_mod)
    + geom_point()
      #scale_x_sqrt() 
    + facet_wrap(season~.)
    + theme_bw()
)
