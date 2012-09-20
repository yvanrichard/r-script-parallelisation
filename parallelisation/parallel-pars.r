
user <- 'yvan'
thiscomp <- 'robin'

ca <- data.frame(comp  = c('robin', 'tieke', 'leon', 'frank', 'taiko'),
                 cores = c(   5   ,    5   ,    5  ,    4   ,    5   ),
                 weight= c(   1   ,   1.1  ,    1  ,    1   ,   1.2  ),
                 stringsAsFactors=F
                 )


datafile <- '../init-samples/all-results/all-good-samples-bh_dem.rdata'
dataname <- 'dat'  # name of data frame
rowidcol <- 'row'

##  For makefiles
projectname <- 'projectx'
runfold <- 'modelling/calc'  # where the run file is, relatively to project base folder
runfile <- 'run1.r'   # file to run job on each slice, needs to take sequence of ids as txt in arguments
outputfold <- 'results'  # relatively to runfold
outputbasename <- 'slice'  # slice seq and .rdata will be appended to it
allresfold <- 'all-results'

sources <- NULL
libs <- NULL
deps <- NULL
## sources <- 'functions.r'
## libs <- 
## deps <- c('functions-model.r', 'functions.r')  # dependencies in runfold
