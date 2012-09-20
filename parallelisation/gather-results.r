###  Fetch results from all computers  ###
##########################################

rm(list=ls())
library(compiler)

basefold <- '~/dragonfly/sra-foundations/modelling/bh-dd-k50/'
setwd(basefold)

source('parallel-pars.r')
## source('functions.r')
loadcmp('functions_cmp.r')

load(datafile)

load('cores_alloc.rdata')
othercomps <- unique(cores_alloc$comp[!(cores_alloc$comp %in% thiscomp)])

c=othercomps[1]
for (c in othercomps)
  {
    fold <- sprintf('%s-%s', outputfold, c)
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
    setwd(sprintf('%s%s', basefold, d))
    
    inter <- dir(pattern='.*pars1-steps.*..rdata')
    fin <- dir(patter='.*pars-steps.*..rdata')

    for (f in inter)                    # f=inter[1]
      {
        load(f)
        resi <- rbind2(resi, pars1)
      }

    for (f in fin)                      # f=fin[2]
      {
        load(f)
        resf <- rbind2(resf, pars)
      }
  }


setwd(basefold)

## Unmount other computers
for (c in othercomps)  # c='frank'
  {
    fold <- sprintf('%s-%s', outputfold, c)
    system(sprintf('fusermount -u %s', fold))
    unlink(fold, recursive=T)
  }

allresdir <- sprintf('%sall-results', basefold)
dir.create(allresdir, showWarnings=F)
setwd(allresdir)


## Merge intermediate and final results
resi2 <- resi[!(resi$row %in% resf$row),]  # remove finished rows from intermediate results
res <- rbind(resi2, resf)                  

res <- res[order(res$row),]



## Check missing rows and summarise
missingrows <- dem$row[!(dem$row %in% res$row)]
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
   Details:', nrow(res), sum(duplicated(res$row))),'\n')
print(table(res$species))

if ('pbr0' %in% names(res))
  cat(sprintf('\n%i PBR values (%0.1f%%) already calculated.\n', sum(!is.na(res$pbr0)),
      100*sum(!is.na(res$pbr0))/nrow(res))) else
         cat('\nNo PBR values already calculated.\n')


### Save appended results
res <- res[order(res$row),]

setwd(allresdir)
save(res, file='results_res.rdata')






