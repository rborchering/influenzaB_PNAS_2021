## Process FluView Data
#' Load subtype-specific data from FluView
#source("../src/libraries.R")

## default reader for cdc format
## comments in quote at top of file
mk.csv.cdc <- function(x, ...) {
    read.csv(x, skip=1,header=T,stringsAsFactors = F, na.strings='X', ...)
}

## locations
.dir.spatial <-  "00-RawData/spatial/"
.dir.cdc <-  "00-RawData/FluView_current/"

files <- list(
    old = paste0(.dir.cdc, "WHO_NREVSS_Combined_prior_to_2015_16.csv"),
    new = paste0(.dir.cdc, "WHO_NREVSS_Clinical_Labs.csv")
)

ILI <- lapply(files, mk.csv.cdc) 
## sum types over select cols
ILI$old$TOTAL.A <- (
    ILI$old %>% select(A..2009.H1N1.,A..H1.,A..H3.,A..Subtyping.not.Performed.,A..Unable.to.Subtype.,H3N2v)
    %>% rowSums(na.rm=TRUE)
)
## fix colnames between old and new
ILI$old$TOTAL.B <- ILI$old$B
## do this to both old and new
## cleanup colnames
ILI <- lapply(ILI, function(x) {
    colnames(x) <- sub(x=tolower(colnames(x)), '.', '_', fixed=T)
    x$state <- x$region
    select(x, state, year, week, total_specimens, percent_positive, total_a, total_b)
})

## combine
ILIplus <- rbind(ILI$old, ILI$new)

#state.names=unique(ILIplus$state)
## decide which states/non-states not to include in the analysis
#not.these.states = c("Wyoming","Virgin Islands","Rhode Island","Puerto Rico","New Jersey","New York City","New Hampshire","Florida","District of Columbia","Alaska")
not.these.states = c("Virgin Islands","Puerto Rico","New York City","District of Columbia")
ILIplus <- ILIplus %>% filter( !(state %in% not.these.states))

## not needed
#these.states=state.names[-which(state.names %in% not.these.states)]

####
## TODO: automate which week is the last week available in most recent data update?
####
season.week <- list(
    start=40,
    end = 4
)

## health and human services region
tmp.hhs <- read.csv(paste0(.dir.spatial, 'state-hhs.csv'), stringsAsFactors = F)
## order per becky, 2020-02-28
hhs.order <- c(1, 2, 3, 5, 7, 8, 10, 9, 6, 4)

# add average latitude and longtitude coordinates
# and index of state N-S position
## add in hhs region, state order
state.lat.long <- (
    read.csv(paste0(.dir.spatial, "StateLatLongAvg.csv"), stringsAsFactors = F)
    %>% left_join(tmp.hhs, by="state")
    %>% mutate(
        index_lat = rank(latitude),
        hhs_fac = factor(hhs, levels=hhs.order, labels=paste0('HHS ', hhs.order)),
        ## factor column with levels in latitude order,
        ## S to N
        state_lat = factor(state, 
            levels=state[order(index_lat)]
        ),
        state_abb_lat = factor(state.abb,
            levels = state.abb[order(index_lat)]
        )
    )
)

## join lat/long, hhs region
ILIplus <- left_join(
    ILIplus, state.lat.long, by="state"
)

## add cols:
## Epiweek to date, pomp index
ILIplus <- within(ILIplus, {
    ## CDC epiweek starts on Sat??
    date <- sprintf('%d-W%02d-6', year,week) 
    date <- ISOweek2date(date) + (7*(year<2016))
    ## tmp: 1 Jan of start year
    day <- paste0(year(min(date)), '-01-01')
    ## days since 1 Jan
    day <- as.integer(date - as.Date(day))
    ## order factor by epidemic week for plotting
    week_fac <- factor(week, levels=c(season.week$start:53, 1:(season.week$start-1)))
    ## center week to start of epidemic season
    week_mod <- ifelse(week>(season.week$start-1), week-53, week)
    ## modify above, start at 1
    week_mod <- week_mod +1-min(week_mod)
    date_num <- decimal_date(date)
    ## CDC influenza season:
    ## for proportions below we want the denominator to 
    ## reflect season and not calendar year
    season <- year-(week<season.week$start)
    season_fac <- factor(season)
    season_cut <- cut(season, breaks=seq(from=min(season)-1, to=max(season), length.out=5))
})

## for each week, number of states with missing/zero positives
nmiss <- (
  ILIplus
  %>% group_by(week)
  %>% summarize(
    nmiss.a = sum(total_a==0 | is.na(total_a)),
    nmiss.b= sum(total_b==0 | is.na(total_b)),
    prop.nmiss=(nmiss.a+nmiss.b)/(2*length(total_a)),
    ## total specimens
    prop.na=sum(is.na(total_specimens))/n()
  )
)

##
# find states without any typed typed a or b cases
good.states <- (
  ILIplus 
  %>% group_by(state) 
  %>% summarise(max=max(sum(total_a+total_b,na.rm = T)))
  #%.>% browser()
  %>% filter(max>0)
  ## return vector
  %.>% .$state
)

ILIplus <- (
    ILIplus %>% 
    filter(state %in% good.states)%>% 
    ## Fix reduncancy in values for "2015-01-03". Keep season 2014 week 53 values 
    ## (remove repeated values designated as week 1)
    #summary(select(ILIplus[which(ILIplus$date=="2015-01-03"),],date,week_fac))
    filter(!(date=="2015-01-03" & week_fac==1))
)

## gather / reshape: type as factor
ili.wide <- ( 
    ILIplus
    %>% select(state, state_lat, year, date, date_num, week_fac, total_a, total_b)
    %>% gather(key='type', value='total', -state:-week_fac)
    ## nice format for type col 
    %>% mutate(type=paste0('Type ', toupper(sub('total_','', type))))
)

## samples per week
## was weekly_specimens
ili.samples <- (
    ILIplus 
    %>% group_by(date) 
    %>% summarise(
        total=sum(total_specimens,na.rm=T), 
        total_a=sum(total_a,na.rm=T),
        total_b=sum(total_b,na.rm=T)
    )
)


## choose weeks  available for this season so far
ILIplus.early <- ILIplus %>% filter(week_fac %in% c(season.week$start:53,1:season.week$end))

## do everything the same for each of the following
ili.list <- list(
    full=ILIplus, early=ILIplus.early
)

## Season totals:
# add columns for total positve A and total positive B for each state in each week in each season 
ili.list <- lapply(ili.list, function(dat) {
    ## modify dat in-place
    dat <- (
        dat 
        ## compute totals, merge back in
        #%.>% browser()
        %.>% group_by(., state,season) 
        %.>% summarise(., 
            ## TODO: name, distinguish from total_a
            season_a=sum(total_a,na.rm = T),
            season_b=sum(total_b,na.rm = T),
            season_specimens=sum(total_specimens,na.rm = T)
        )
        %.>% left_join(dat, ., by=c("state","season")) 
    )
    ## add columns: proportion of samples occuring in each week for each (type x season x state)
    ## TODO: please check logic
    dat <- mutate(dat,
        prop_a=total_a/season_a,
        prop_b=total_b/season_b,
    )
    dat
})

################################
## clean / subset ILI data
## (prep for glmer.R)
################################

## arbitrary cutoff
## alternately, prop.na < quantile(x, prob=0.9)
## unused?
.max.prop.na = 0.3
## per 100,000
.percap.per = 1e5

## final cleanup and ordering
ili.clean <- (
  ILIplus
  ## add state level poopulation size data by year
  %>% left_join(state.pops,by=c("state","year"))
  ## subsetting
  ## remove weeks with lots of missing data?
  ## TODO: move downstream to glmer.R?
  ## remove weeks with high prop NA (inter-epidemic period)
  #%>% filter(week %in% unique(subset(nmiss, prop.na<.max.prop.na)$week))
  ## %>% filter(week %in% unique(subset(nmiss, prop.na<quantile(prop.na, 0.8))$week))
  %>% mutate(
    rate_a = c(NA,(total_a[-1])/head(total_a, -1)),
    rate_b = c(NA,(total_b[-1])/head(total_b,-1)),
    percap_a = .percap.per*(total_a/pop),
    percap_b = .percap.per*(total_b/pop),
    ## distance from equality
    percap_dist = (percap_a-percap_b),
    total_pos=total_a+total_b,
    prop_b = (total_b)/total_pos,
  )
  ## set order
  %>% arrange(state, season_fac, week_mod)
)
