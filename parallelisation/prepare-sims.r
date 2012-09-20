## Small script to get row numbers to get divided between cores
rm(list=ls())

args <- commandArgs(trailingOnly=T)
test <- ifelse('test' %in% tolower(args), T, F)


source('../pars_parallel.r')

if (!is.null(sources))
  {
    for (i in 1:length(sources))
      source(sources[i])
  }

if (!is.null(libs))
  {
    for (i in 1:length(libs))
      library(libs[i])
  }

collapseseq <- function(x, with.attr=F)  # x=c(2,4,6,7,9,10,11,12,16)
  {
    if (!(class(x) %in% c('numeric','integer')))  return(NA)
    if (length(x)>1)
      {
        x <- sort(unique(x))            # just in case
        n <- length(x)
        d <- x[2:n]-x[1:(n-1)]
        d2 <- c(2,d)
        s <- cumsum(d2-1)
        r <- split(x, s)
        rs <- sapply(r, function(x)
                     {
                       nx <- length(x)
                       if (nx > 1)
                         return(sprintf('%s-%s', x[1], x[nx])) else
                       return(x)
                     }, simplify=T)
        ns <- sapply(r, length)
        txt <- paste(rs, collapse=',')
        if (with.attr) {
          attr(txt,'elements') <- rs
          attr(txt,'ns') <- ns
        }
        return(txt)
      } else
          {
            txt <- as.character(x)
            if (with.attr) {
              attr(txt,'elements') <- as.character(x)
              attr(txt,'ns') <- 1
            }
            return(txt)
          }
  }


nc <- sum(ca$cores)  # number of cores

cs <- data.frame(comp = unlist(Map(rep, ca$comp, ca$cores)),
                 id   = 1:nc,
                 w    = unlist(Map(rep, ca$weight, ca$cores)),
                 stringsAsFactors=F)
names(cs) <- c('comp','id','w')

if (grepl('rdata$', tolower(datafile)))
  {
    load(sprintf('../%s', datafile))
    data <- eval(parse(text=dataname))
  }
if (grepl('csv$', tolower(datafile)))
  data <- read.csv(sprintf('../%s',datafile), as.is=T)


n <- nrow(data) # number of simulations

if (n < nrow(cs))
  {
    warning('Less rows to be processed than available cores.')
    cs <- cs[1:n,]
  }
  
if (n > nrow(cs))
  iv <- findInterval(1:n/n, cumsum(cs$w/sum(cs$w)), rightmost.closed=T) + 1  else
         iv <- 1:n 

ss <- tapply(as.numeric(data[[rowidcol]]), list(iv), collapseseq, simplify=F)
cs <- cbind(cs, do.call('rbind', ss))
names(cs)[length(names(cs))] <- 'seq'

## Save
cores_alloc <- cs
if (!test)
  save(cores_alloc, file='cores_alloc.rdata') else {print(cores_alloc); cat('\n\n')}

dir.create('makefiles',showWarnings=F)

system(sprintf("cd %s/%s; git add .; git commit -m 'auto-commit'; git push", basefold, projectname))

## Create makefiles and send them to the computers
cp='robin'
comps <- ca$comp
for (cp in comps)
  {
    sx <- cs[cs$comp %in% cp,]
    
    ## create makefiles locally
    makef <- sprintf('makefile0-%s',cp)
    fullmakef <- sprintf('makefiles/%s', makef)
    outs <- sprintf('%s/%s-%s.rdata', outputfold, outputbasename, sx$seq)
    cat(sprintf('all: %s\n\n', paste(outs, collapse=' \\\n\t\t\t')), file=fullmakef)
    if (!is.null(deps)) {
      suff <- paste(sprintf(' \\\n\t\t\t%s', deps), collapse='')
    }  else
      suff <- ''
    for (f in sx$seq)
      cat(sprintf('%s/%s-%s.rdata: %s%s\n\tscreen -d -L -m Rscript %s %s\n\n',
                 outputfold, outputbasename, f, runfile, suff, runfile, f), file=fullmakef, append=T)

    if (test) {system(sprintf('cat %s', fullmakef)); cat('\n\n')}
    
    ## check if project exists on computer, and clone the git repo if necessary
    suppressWarnings(ex <- system(sprintf("ssh %s@%s test -d '%s/%s' && echo TRUE",
                                          user, cp, basefold, projectname),
                                  intern=T))
    if (!length(ex))
      system(sprintf("ssh -A %s@%s 'cd %s; git clone %s' 2>&1", user, cp, basefold, gitproj), intern=T)

    ## copy makefile to remote computer
    cmd <- sprintf('scp %1$s %2$s@%3$s:%4$s/%5$s/%6$s',
                     fullmakef, user, cp, basefold, projectname, runfold)
    if (!test)  system(cmd)  else  {print(cmd); cat('\n\n')}

  }



