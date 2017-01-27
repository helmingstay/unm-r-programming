# read data
allfog <- read.csv("data/allfog.csv")
str(allfog)

# create date and day of year (doy) columns
allfog$Date <- as.Date(allfog$Convert.Date, format="%m/%d/%y")
allfog$doy  <- as.numeric(strftime(allfog$Date, format="%j"))

# which results are non-numeric?
levels(allfog$RESULT)[grep('<', levels(allfog$RESULT))] # many.....
allfog$inequality <- grepl('<', allfog$RESULT) # which elements have inequality?

# initialise new vector for cleaned data
allfog$fog <- NA
# directly copy numeric strings
allfog$fog[!allfog$inequality] <- as.numeric(
  as.character(allfog$RESULT[!allfog$inequality]))
# remove inequality sign and use half detection limit, per Bruce Thomson
allfog$fog[allfog$inequality] <- as.numeric(
  as.character( gsub('<', '', allfog$RESULT[allfog$inequality]) ) ) / 2 
# log transform because of extreme values
allfog$lfog <- log(allfog$fog)

#### Calculating weekly mean FOG reading
## extract only log(fog) and date
all.lfog <- subset(allfog, select=c('lfog', 'Date') )

## convert to xts format
all.lfog$Date <- as.POSIXct(all.lfog$Date, format='%Y-%m-%d')
all.lfog <- all.lfog[order(all.lfog$Date),]
allfog.xts <- xts( all.lfog$lfog, order.by=all.lfog$Date ) # only the lfog col


## aggregate FOG measurements by week
allfog.day <- apply.daily(allfog.xts, mean)
allfog.week <- apply.weekly(allfog.day, mean) # mean is default function; no NAs
nrow(allfog.week) # 232 observations + 1 dummy

## convert back to dataframe for joining and analysis
allfog.week.df <- as.data.frame(allfog.week)
## convert index to Date format
allfog.week.df$Date <- as.Date( index(allfog.week) )
names(allfog.week.df)[1] <- 'meanlfog' # rename column

#### additional columns for compatibility with sewblock, greaseblock &c
## sporadic FOG sampling means columns do not align with other dataframes
allfog.week.df <- mk.yearweek(allfog.week.df) 

# are all year-week combos unique?
allfog.week.df$year.week <- sprintf('%s.%s', allfog.week.df$year, allfog.week.df$week)
length(unique (allfog.week.df$year.week)) / nrow(allfog.week.df) # not quite: 1 duplicate!
# which are the duplicates?
allfog.week.df[duplicated(allfog.week.df$year.week),] # only 2 weeks, can be excluded for now
allfog.week.df <- allfog.week.df[!duplicated(allfog.week.df$year.week),]# excluding duplicates
