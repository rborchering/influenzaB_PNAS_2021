## yearly difference: meadian + IQR + 80%
p.phase.diff.stats <- (
  subset(phase.diff.stats) #, epiweek >= 44 | epiweek <= 8)
  %.>% ggplot(.)
  + aes(x = date)
  #+ geom_ribbon(aes(ymin=p0.5, ymax=p0.95), alpha=0.2)
  + geom_hline(yintercept=0, linetype=2)
  + geom_ribbon(aes(ymin=p0.25, ymax=p0.75), alpha=0.2)
  #+ geom_ribbon(aes(ymin=p0.1, ymax=p0.9), alpha=0.2)
  + geom_line(aes(y=p0.5))
  + scale_x_date(limits=xlim.ili,expand=c(0,0))
  #+ gg.scale.date(xlim.ili)
  + xlab('Date')
  ## ?? check w/Arash
  + ylab('Peak week phase lag (weeks)')
  + gg.theme
)

p.phase.diff.box <- (
  #subset(dat.phase, !(epiweek > 8 & epiweek < 44))
  subset(dat.phase, !(epiweek > 8 & epiweek < 44))
  %.>% ggplot(.)
  + aes(x = season, y=val.week)
  + geom_boxplot(size=0.25, outlier.alpha=0.2, outlier.shape=1, outlier.size=0.2)
  + geom_hline(yintercept=0, size=0.3, linetype=2, alpha=0.4)
  + xlab('Influenza season')
  + ylab('Phase lag (weeks)')
  + gg.theme
)

