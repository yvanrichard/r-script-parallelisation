rm(list=ls())
source('../pars_parallel.r')

load('cores_alloc.rdata')
comps <- unique(cores_alloc$comp)
comps <- comps[!(comps %in% thiscomp)]

cp <- comps[1]
for (cp in comps)
  {
    cat('*****', cp,'\n')
    cmd <- sprintf('ssh -A %s@%s "cd %s/%s; git pull"', user, cp, basefold, projectname)
    cat(cmd,'\n')
    system(cmd, wait=T)
    cat('\n\n')
  }

cat('Done.\n\n')
