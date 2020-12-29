## average year, excluding 2019 (slow)
## predict percap incidence by type in the "average" year/state
dat.season <- (
    subset(ili.clean, season < 2019)
    %.>% reshape2::melt(., 
        id.vars=c('state_lat', 'season_fac', 'week_mod'),
        measure.vars=c('percap_a', 'percap_b'),
        value.name='percap',
        variable.name='Type'
    )
)
## log link function: keep response non-negative
mod.season <- (
    gam(data=dat.season, formula=percap ~ state_lat + s(season_fac, bs='re') + s(week_mod, bs='cs', k=12, by=Type), family=gaussian(link='log'))
)
pred.season <- (
    emmeans(mod.season, ~Type+week_mod, at=list(week_mod=1:53), type='response')
    %.>% as.data.frame(.)
)

## model of difference in percap incidence
## smooth over states
mod.dist <- gam(data=ili.clean,
    formula=percap_dist ~ state_lat + s(week_mod, bs='cs', k=25, by=season_fac)
)
pred.dist <- (
    emmeans(mod.dist, ~season_fac+week_mod, 
        at=with(ili.clean,  
            list(season_fac=levels(season_fac), week_mod=unique(week_mod))
        )
    ) %.>% as.data.frame(.)
    %.>% subset(., paste(season_fac, week_mod) %in% with(ili.clean, paste(season_fac, week_mod)))
)


## regression model for estimating the probability that a positive influenza sample is type B.

## glmer nAGQ (0/F: fast, 1/T: slow)
## see refs: https://stats.stackexchange.com/questions/365716/what-does-it-mean-when-a-low-number-of-quadrature-points-gives-a-very-different
.slow <- F
## 15785 sec (4.4 hrs)
#.slow <- 10
.ncore = 2
## formula
.form <- formula(
    ## gam uses by= ugly hack to select random effects for prediction
    ## See https://stat.ethz.ch/pipermail/r-help/2011-June/281886.html
    ## see ?choose.k and gam.check for background re k
    cbind(total_b, total_a) ~ state + s(week, bs='cs', k=22) + s(season_fac, bs='re', by=do.ranef)
)
## 
.weeks.early <- c(40:53,1:4)

## helper function: change level
.lev.to <- function(x, from, to) {
    levels(x)[levels(x)==from] <- to
    x
}
## helper function
## reset factor level to not-current level
## for predict.gam with (unseen) random effects
.lev.rot <- function(x) {
    lev <- levels(x)
    curr <- as.character(x[1])
    lev.to <- setdiff(lev, curr)[1]
    x <- .lev.to(x, curr, lev.to)
    x
}

## convenience function
## add dummy variable
## for model interpretation tutorial, see:
## https://stats.stackexchange.com/questions/190172/how-i-can-interpret-gam-results
mk.gam <- function(dat, form) {
    dat <- cbind(dat, do.ranef=1)
    gam(form, family=binomial, data=dat, method='ML')
}

## helper function: make prediction from mod, dat
## proportion B
.predict <- function(mod, dat, do.ranef=0) {
    if (nrow(dat)==0) return()
    ## use for newdata=
    ## (don't retain nuissance cols)
    dat.tmp <- cbind(dat, do.ranef)
    if (!do.ranef) {
        dat.tmp$season_fac <- .lev.rot(dat.tmp$season_fac)
    }
    ## leave out this year
    ## and last week(s) of year (fixed effect)
    dat %.>% mutate(., 
        ## point estimate
        pred.prob.b=predict(mod, newdata=dat.tmp, type='response'),
        ## includes binomial sampling error
        pred.b.draw=rbinom(nrow(.), size=total_pos, prob=pred.prob.b)/total_pos,
        ## observed - predicted
        pred.diff=prop_b - pred.prob.b
    ) %.>% data.table(.)
}

## model-buiding data: leave out recent season
.dat <- droplevels(
    subset(ili.clean, season<2019)
)
.fseasons <- make.names(levels(.dat$season_fac))

## for each leave-one-out season, 
## fit model, get predictions
## (never use 2019)
#lmod.loo.season  <- lapply(.fseasons, function(fseason) {
lmod.loo.season  <- mclapply(.fseasons, mc.cores=.ncore, function(fseason) {
    ## Report progress
    cat('## in glmer, season', fseason, '\n')
    ## fit model, print out timings
    ps(
      mod <- (
          ## leave out newest, and one
          subset(.dat, (season_fac!=fseason))
          %.>% mk.gam(., .form)
      )
    )
    ## predictions for left-out year
    ## nothing here for "all"
    pred <- (
        subset(.dat, season_fac==fseason)
        %.>% .predict(mod, .)
    )
    return(list(mod=mod, pred=pred))
})


## model for all data but 2019
mod.all <- mk.gam(.dat, .form)
## pred for 2019
pred.2019 <- (
    subset(ili.clean, season_fac=='2019')
    %.>% .predict(mod.all, .)
)
## combine model predictions into single data.frame
pred.loo <- (
    lapply(lmod.loo.season, '[[', i='pred')
    %>% rbindlist(.)
)

    
## weeks of 2019 data, excluding 53
pred.loo.early <- (
    pred.loo
    %>% filter(week %in% .weeks.early)
    %>% rbind(., pred.2019)
    %>% as.data.table()
)


## combine all/early into long form
pred.all <- rbindlist(idcol='Weeks',
    list(All=pred.loo, Early=pred.loo.early)
)
## summarize by season
pred.stats <- pred.all[, 
    by=.(Weeks, season_fac), 
    j=make.stats(pred.diff)
]

## inspect models: AIC
if (F) {
    loo.mod.aic <- (
        lapply(lmod.loo.season, '[[', i='mod')
        %>% lapply(., function(x) list(aic=AIC(x)))
        %>% rbindlist(., idcol='season')
    )
}

## Check QQ plot with real statistician (Deven?)
# summary(mod.pre)
## is rsq available for glmer?
# library(rsq)
# rsq(mod.pre)
## consider using stan and Bayesian Rsq metric?
