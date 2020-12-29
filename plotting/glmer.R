## density plot of out-of-sample prediciton residuals 
p.pred.diff.base <- (
    pred.loo
    %>% ggplot()
    + aes(x=pred.diff, group=season_fac)
    #+ geom_density()
    + stat_density(geom='line', position='identity')
    ## reference line
    + geom_vline(xintercept=0, linetype=2, color='grey30', alpha=0.5)
    + xlab('Proportion B: observed - predicted')
    + ylab('density\n(all season weeks)') 
    + gg.theme
)

## split by season
p.pred.diff.wrap <- (
    p.pred.diff.base 
    + facet_wrap(~season_fac, nrow=2)
    + geom_rug(alpha=0.05)
)


## overlaid density plots of difference between predicted probability of observing be and observed proportion B
p.pred.diff <- (
    p.pred.diff.base
    + aes(color=season_fac)
    + ylab('Density')
    + scale_color_manual('Season', values=gg.col.season) ## need to check colors on this 
    + guides(color=guide_legend(ncol=5, override.aes=list(size=1)))
    + theme(
        legend.position = c(0.98,0.95),
        legend.justification = c(1,1),
        legend.title.align=0.5
    )
    + gg.leg.outline
)

## compare leave-season-out predictions of early weeks
## against 2019
p.pred.diff.early <- (
    p.pred.diff
    #%+% loo.pred.early
    %+% subset(pred.loo.early)
    + ylab('Density (early season weeks)') 
)


## summary stats
## for early weeks and all weeks
p.pred.diff.stats <- (
    pred.stats
    %.>% ggplot(.) 
    + aes(x=p0.5, y=season_fac) 
    + facet_grid(~Weeks)
    + geom_vline(xintercept=0, size=0.5, linetype=2, color='grey40') 
    + geom_point() 
    + geom_segment(aes(x=p0.25, xend=p0.75, yend=season_fac)) 
    + xlab('Observed -predicted (median + IQR)')
    + scale_y_discrete('', limits=rev(levels(.$season_fac)))
    + gg.theme
)

cap.dev.expl <- sprintf('%2.1f\\%%', summary(mod.all)$dev.expl*100)

## as above, but violin plot
p.pred.diff.box <- (
    subset(pred.all, Weeks=='Early')
    %.>% ggplot(.) 
    + aes(x=season_fac, y=pred.diff) 
    + geom_boxplot(size=0.25, outlier.alpha=0.2, outlier.shape=1, outlier.size=0.2)
    + geom_hline(yintercept=0, size=0.2, linetype=2, alpha=0.4)
    #+ ylab('Proportion B (Obs - Pred)')
    + ylab('Proportion B\n(Observed - Predicted)')
    + xlab('Influenza season')
    + scale_y_continuous(expand=expand_scale(mult=c(0.07, 0.04)))
    #+ scale_y_discrete('', limits=rev(levels(.$season_fac)))
    + gg.theme
)

## predicted probabilities versus observed proportions plot
p.obs.v.predicted <- (
    rbind(pred.loo, pred.2019)
    %.>% ggplot(.) +
    aes(x=prop_b,y=pred.prob.b, group=season_fac) +
    geom_point(alpha=.1)+
    facet_wrap(~season_fac, ncol=5)+
    coord_fixed()+
    xlab("Observed proportion B (out of positive samples)")+
    ylab("Predicted Pr(B)") +
    gg.theme+
    gg.xrot
)

## predicted probabilities versus observed proportions plot
p.obs.v.predicted.prop <- (
    p.obs.v.predicted +
    aes(y=pred.b.draw) +
    coord_fixed()+
    ylab("Predicted Proportion B (GLMM, includes binomial sampling error)")
)


## density plot faceted over HHS regions
## TO DO: fix Missisippi - showing up as NA values for HHS region
not_used <- (
    pred.loo.early %>% 
    ggplot() + 
    aes(pred.diff,colour=season_fac)+
    geom_density()+
    geom_vline(xintercept = 0,alpha=.2,show.legend = NA)+
    ## This xlab is waaaaaaaaaaaaaaaaaaay too long
    xlab('proportion of positive samples that are Type B - predicted probability of an observed positive sample being Type B')+
    ylab('Density (early season weeks)')+
    ## how does this work?
    facet_wrap(vars(hhs)) +
    gg.theme
    #facet_grid(vars(hhs),vars(pred.year))
)


## scatterplot faceted over HHS regions
## TO DO: fix Missisippi - showing up as NA values for HHS region
## xian 2020-02-24: how does group_by work here?
## ggplot(loo.pred, aes(x=prop_b,y=pred.prob.b,color=factor(pred.year)),group_by(pred.year))+
not_used <- (
    ggplot(pred.loo) + 
    aes(x=prop_b,y=pred.prob.b,color=season_fac) +
    geom_point(alpha=.2)+
    facet_grid(hhs~season_fac)
)
