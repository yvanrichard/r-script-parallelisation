rm(list=ls())
source('parallel-pars.r')

load('cores_alloc.rdata')
comps <- unique(cores_alloc$comp)

cp <- comps[1]

for (cp in comps)
  {
    cat('*****', cp,'\n')
    cmd <- sprintf('ssh -A %s@%s "killall -u %s screen"', user, cp, user)
    cat(cmd,'\n')
    system(cmd, wait=T)
    cat('\n\n')
  }

cat('Done.\n\n')
    
