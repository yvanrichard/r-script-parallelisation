rm(list=ls())

library(compiler)

setwd('~/dragonfly/r-script-parallelisation')
load('data.rdata')

source('pars_job.r')
source('pars_parallel.r')

## source('functions.r')
## source('functions-model.r')
loadcmp('functions_cmp.r')
loadcmp('functions-model_cmp.r')
## source('functions-loggamma.r')

dir.create('results', showWarnings=F)

args <- commandArgs(trailingOnly=T)
if (!length(args)) args <- '3-10,1002'
## args <- '3-1000,1002'
# args <- '56059,48001,104437,96078,90678,72124,64077'
# args <- '2'
# args <- '60001'
rows <- makeseq(args)

if (any(rows>nrow(dem)))
  stop('Some row numbers exceed maximum number of rows')
if (any(is.na(rows)))
  stop('Some row values are invalid')


## Propensity of being killed in fisheries (sum does not have to be 1)
## fj = 1  # juveniles
## fm = 1  # males
## ff = 1  # females

## keep only rows specified as argument
pars <- dem[as.numeric(dem$row) %in% rows,]
pars$fcrit <- pars$fcrit_acc <- pars$fcrit_nsteps <- NA
pars$K <- pars$N0 <- pars$Na0 <- pars$Nbp0 <- pars$Na_end <- pars$N_end <- pars$n_extinct <- NA

## initial population matrices, not to recalculate them every time
nmf_mat0 <- ntot_mat0 <- kmf_mat0 <- matrix(rep(0, maxyear*trajs))
dim(nmf_mat0)  <- dim(kmf_mat0) <- c(trajs, maxyear)
dim(ntot_mat0) <- c(maxyear, trajs)


###  MAIN
allsteps = NULL
startt <- Sys.time()
row=1
for (row in 1:nrow(pars)) # row=1
    {
      rw <- pars$row[row]
      cat(sprintf('row %s - %i/%i - ', rw, row, nrow(pars)))
      ps <- pars[row,]
      k <- ps$a - 1

      ki_mat0 <- matrix(rep(0, k*maxyear*trajs)); dim(ki_mat0) <- c(k, maxyear, trajs)
      ks_mat0 <- matrix(rep(0, (k+2)*maxyear*trajs)); dim(ks_mat0) <- c(k+2, maxyear, trajs)

      ## res <- sim(ps, trajs, maxyear, propkilled=0.015)
      ## plot_res(res)
      # fcsteps  <-  find_fcrit(Pars=ps, keep='all')  # the BIG calculations
      fcsteps  <-  find_fcrit(Pars=ps, keep='last')  # the BIG calculations
      fcsteps$lastsim[['ni']] <- fcsteps$lastsim[['ks']] <- NULL
      save(fcsteps, file=sprintf('%s/res-row_%s.rdata', bigresultdir, rw))

      ns <- fcsteps$lastsim
      steps <- fcsteps$steps
      ntot <- ns[['ntot']]
      ntot0 <- ns[['ntot']][1,1]
      na0 <- ns[['nam']][1,1]+ns[['naf']][1,1]
      last <- nrow(steps)
      pars$fcrit[row] = steps[last, 'fc']
      pars$fcrit_acc[row] = steps[last, 'diff95']
      pars$fcrit_nsteps[row] = last
      pars$K[row] <- attr(ns$pars,'K')    # total carrying capacity
      pars$Na0[row] <- na0     # number of adults at t0
      pars$N0[row] <- ntot0        # total population at t0
      pars$Na_end[row] <- mean(ns[['nam']][,maxyear]+ns[['naf']][,maxyear])
      pars$N_end[row] <- mean(ns[['ntot']][maxyear,])
      pars$n_extinct[row] <- ns$nextinct
      pb_ <- applyDD(ps$pb, ps$cdd, ntot0, ps$theta, SDIST)
      pars$Nbp0[row] <- round(na0/2 * pb_)
 
      cat(sprintf('%s: fc=%f, in %i steps, acc=%0.2f', pars$species[row], pars$fcrit[row], pars$fcrit_nsteps[row], pars$fcrit_acc[row] ))
      if (row %% 10 == 0)
	{
          tdiff <- (nrow(pars)-row) * difftime(Sys.time(), startt, units='hours') / row
          cat(' - saving... ', format(Sys.time(), '%d/%m %H:%M'), '-',
              sprintf('%0.1f %s remaining',as.numeric(tdiff), attr(tdiff,'units')))
          pars1 = pars[1:row,]
          save(pars1, file=sprintf('results/fcrit-res_pars1-steps-%s.rdata',args))
	}
      cat('\n')
    }
cat('Final save...\n')
save(pars, file=sprintf('results/fcrit-res_pars-steps-%s.rdata', args))


