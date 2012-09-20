rm(list=ls())
source('../pars_parallel.r')

fls <- dir(sprintf('%s/%s', basefold, projectname))

comps <- ca$comp

timeflag <- format(Sys.time(), '%y%m%d-%H%M')

cp <- comps[1]
for (cp in comps)
  {
    cat('\n***  Parking ', cp, '... \n', sep='')

    ## results
    f <- sprintf('%s/%s/%s/%s', basefold, projectname, runfold, outputfold)
    a <- suppressWarnings(system(sprintf('ssh -A %s@%s "ls -d %s" 2>&1', user, cp, f), intern=T))
    if (!grepl('^ls:',a))
      {
        cmd <- sprintf('ssh -A %s@%s "cd %s/%s/%s; mv %s %s_%s"', user, cp,
                       basefold, projectname, runfold, outputfold, outputfold, timeflag); cat(cmd,'\n')
        system(cmd, wait=T)
      } else cat(sprintf('!!!  Folder %s does not exist on %s. Skip...\n', f, cp))

    
    ## all-results
    f <- sprintf('%s/%s/%s/%s', basefold, projectname, runfold, allresfold)
    a <- suppressWarnings(system(sprintf('ssh -A %s@%s "ls -d %s" 2>&1', user, cp, f), intern=T))
    if (!grepl('^ls:',a))
      {
        cmd <- sprintf('ssh -A %s@%s "cd %s/%s/%s; mv %s %s_%s"', user, cp,
                       basefold, projectname, runfold, allresfold, allresfold, timeflag); cat(cmd,'\n')
        system(cmd, wait=T)
      } else cat(sprintf('!!!  Folder %s does not exist on %s. Skip...\n', f, cp))

    cat('Done.\n\n')
  }

if (length(warnings()))
  print(warnings())

