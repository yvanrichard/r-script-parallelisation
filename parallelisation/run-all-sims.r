## Run makefiles on all computers specified in parallel-pars.r
rm(list=ls())
source('../pars_parallel.r')
load('cores_alloc.rdata')
comps <- unique(cores_alloc$comp)

cp <- comps[1]

for (cp in comps)
  {
    cat('*****', cp,'\n')
    ## run make on remote computer within screen
    makef <- sprintf('makefile0-%s',cp)
    cmd <- sprintf('ssh -A %s@%s "cd %s/%s/%s; make -f %s -j%i &"',
                     user, cp, basefold, projectname, runfold, makef, ca$cores[ca$comp==cp])
    cat(cmd,'\n')
    system(cmd, wait=F)
  }




