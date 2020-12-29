## process population size data by aggregating over the CDC FluView age-groups

## Process population data available from CDC Wonder
projPop <- read.csv("00-RawData/demography/CDC_Wonder/StatePopulationProjections_2004-2030_CDC_Wonder.csv"
                    , skip=0
                    , header=T
                    , stringsAsFactors = F)
projPop <- (
  projPop 
  %>% mutate(Year.Code=NULL,State.Code=NULL)
  %>% mutate(
    age=as.numeric(sub('+','', Age.Code,fixed=T)),
    age_cut = cut(age, breaks=c(-1, 4, 24, 64,100), labels=c('0-4 yr', '5-24 yr', '25-64 yr', '65+ yr'))
  )
  %>% rename(age_desc=Age, state=State,year=Year,"population"=Projected.Populations,age_group=age_cut)
)
#summary(projPop)
#subset(projPop,is.na(age_cut))
#levels(projPop$age_cut)
age.pops <- (
  projPop
  %>% group_by(year, age_group)
  %>% summarise(
    pop=sum(population)
  )
  %>% filter(year<=2020)
)

state.pops <-(
  projPop
  %>% group_by(year,state)
  %>% summarize(pop = sum(population))
  %>% filter(year<=2020)
)
