###  Fetch results from all computers  ###
##########################################

library(compiler)

source('../pars_parallel.r')
## source('functions.r')
loadcmp('functions_cmp.r')

load(sprintf('../%s', datafile))
data <- eval(parse(text=dataname))
if (dataname != 'data') {rm(dataname); gc()}

load('cores_alloc.rdata')
othercomps <- unique(cores_alloc$comp[!(cores_alloc$comp %in% thiscomp)])

c=othercomps[1]
for (c in othercomps)
  {
    fold <- sprintf('../%s-%s', outputfold, c)
    dir.create(fold, showWarnings=F)
    ## Mount results from other computers
    if (!length(dir(fold)))
      system(sprintf('sshfs %s:/home/%s/dragonfly/%s/%s/%s %s',
                     c, user, projectname, runfold, outputfold, fold))
  }


resi <- NULL
resf <- NULL

folds <- c(outputfold, sprintf('%s-%s', outputfold, othercomps))
for (d in folds)  # d='results-taiko'    # d='results'
  {
    cat('\nMerging files in', d, '...\n')
    setwd(sprintf('%s/%s/%s/%s', basefold, projectname, runfold, d))
    
    inter <- dir(pattern='.*incomplete.*..rdata')
    fin <- dir(patter='.*finished.*..rdata')

    for (f in inter)                    # f=inter[1]
      {
        load(f)
        resi <- rbind2(resi, data1)
      }

    for (f in fin)                      # f=fin[2]
      {
        load(f)
        resf <- rbind2(resf, data)
      }
  }


setwd(sprintf('%s/%s/%s', basefold, projectname, runfold))

## Unmount other computers
for (c in othercomps)  # c='frank'
  {
    fold <- sprintf('%s-%s', outputfold, c)
    system(sprintf('fusermount -u %s', fold))
    unlink(fold, recursive=T)
  }

dir.create(allresfold, showWarnings=F)
setwd(allresfold)


## Merge intermediate and final results
resi2 <- resi[!(resi[[rowidcol]] %in% resf[[rowidcol]]),]  # remove finished rows from intermediate results
res <- rbind(resi2, resf)                  

res <- res[order(res[[rowidcol]]),]



## Check missing rows and summarise
missingrows <- data[[rowidcol]][!(data[[rowidcol]] %in% res[[rowidcol]])]
if (length(missingrows))
  {
    mr <- as.numeric(missingrows)
    mseq <- collapseseq(mr)
    cat(sprintf('\n%i missing sequences (%i rows):\n', length(mseq$n), length(mr)))
    df <- data.frame(seq=mseq$seq, n=mseq$n, row.names=1:length(mseq$n))
    print(df)
    save(df, file='missing-rows_df.rdata')
  }

cat(sprintf('\n%i rows done (%i duplicates) after merging intermediates and final results.
   Details:', nrow(res), sum(duplicated(res[[rowidcol]]))),'\n')


### Save appended results
res <- res[order(res[[rowidcol]]),]

setwd(allresfold)
save(res, file='results_res.rdata')






