## weeks per year
.deltat <- (2*pi)/52.25
.fn <- '../extra/data-phase/full_time_series_phase_lag.csv'
#.fn <- '../extra/data-phase/phase-diff-a-minus-b.csv'

## read each file, merge state hhs
dat.phase <- (
    fread(.fn)
    %.>% melt.data.table(., 
        id.vars='date', variable.name='state',
        variable.factor=F
    )
    %.>% merge(., state.lat.long, by='state')
    %.>% within(., {
        date <- as.Date(date)
        epiweek <- epiweek(date)
        season <- factor(year(date)-(epiweek<season.week$start))
        val.week <- value/.deltat
    })
)

## summary stats, by week
phase.diff.stats <- dat.phase[,
    by=.(date, epiweek), j={
        ## return NAs for out-of-bounds period
        #if ( epiweek > 10 ) val.week <- NA
        #if ( epiweek > 10 & epiweek < 46) val.week <- NA
        if ( epiweek > 8 & epiweek < 44) val.week <- NA
        make.stats(val.week, probs=c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975))
    }
]

## summary stats, by season
phase.diff.season.stats <- dat.phase[,
    by=.(season), j={
        ## return NAs for out-of-bounds period
        #if ( epiweek > 10 & epiweek < 46) val.week <- NA
        val.week[ epiweek > 8 & epiweek < 44] <- NA
        make.stats(val.week, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
    }
]
