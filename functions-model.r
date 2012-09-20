
##########################################################################################
###  FOR SIMULATIONS
##########################################################################################

lesmat <- function(Sa, Si, F, A)
  {
    m <- matrix(0, nrow=A, ncol=A)
    m[A,A] <- Sa
    m[1,A] <- F
    for (i in 1:(A-1))
      m[i+1,i] <- Si
    return(m)
  }


findKlcl <- function(Pars, n0=1e4, p=0.95)  # Pars=dem1[1,]; n0=1e4; p=0.95
  { ## UNFINISHED
    a <- Pars$a; sa <- Pars$sa; se <- Pars$se; si <- Pars$si; cs <- Pars$cs; pb <- Pars$pb
    c_sa <- Pars$c_sa; c_se <- Pars$c_se; c_pb <- Pars$c_pb
    lm <- attr(Pars, 'lesmat')  #lesmat(sa, si, cs*se*pb/2, a)

    ss <- attr(Pars, 'eig')$stable.stage
    N0 <- ss*n0
    #prevn <- n0*2
    N <- NULL
    t <- 1
    N <- cbind(N, N0)
    newn <- n0; prevn <- 0
    while (abs(1 - newn/prevn) > 0.00001 | t<4)
      {
        prevn <- newn
        t <- t+1

        ## density dependence
        se_ <- se/(1 + c_se * newn)
        sa_ <- sa/(1 + c_sa * newn)
        pb_ <- pb/(1 + c_pb * newn)

        lm[1,a] <- .5*cs*se_*pb_
        lm[a,a] <- sa_
        newN <- lm %*% N[,t-1]
        newn <- sum(newN)
        N <- cbind(N, newN)
      }
    #plot(apply(N,2,sum))
    #N[,t]
    nk <- N[,t]
    attr(nk, 'found_in') <- t
    return(nk)
  }


## Apply environmental stochasticity to vital rate
envstoch <- function(u, n, par_mean, par_sd, type=SDIST)
  {
    if (par_sd > 0)
      {
        if (type=='beta')
          {
            par_es <- matrix(qbeta(u, shape1=beta_shp1(par_mean, par_sd),
                                   shape2=beta_shp2(par_mean, par_sd)), nrow=n)
          } else if (type=='nlg')
            {
              par_es <- matrix(rloggamma2(u, a=par_mean, theta=Theta(a=par_mean, Sd=par_sd)), nrow=n)
            } else stop('Wrong type of stochasticity distribution')
      } else
        par_es <- matrix(rep(par_mean, length(u)), nrow=n)
     return(par_es)
   }



## Apply density dependence to vital rate
applyDD <- function(par, cdd, ntot, thetadd=1, type=DDTYPE)
  {
    if (type=='BH')                   # Beverton-Holt
      {
        par_ <- par/(1 + (cdd * ntot)^thetadd)
      } else if (type=='BH2')         # Beverton-Holt
        {
          par_ <- par/(1 + (cdd * ntot)^thetadd)
        } else if (type=='Ricker1')
          {                             # Ricker
            par_ <- pmax(par*(1-(cdd*ntot)^thetadd), 0)
          } else
    {
      par_ <- par*exp(-(cdd * ntot)^thetadd)
    }
  return(par_)
  }


## Pars=ps; Ntraj=trajs; Maxyear=maxyear; Propkilled=0; Ktype='lcl'; ddtype='BH2'; thetadd=10; Propklim=propklim; sdist='beta'
sim <- function(Pars, Ntraj=trajs, Maxyear=maxyear, Propkilled=0, 
                Ktype=KTYPE, Propklim=propklim, ddtype=DDTYPE, sdist=SDIST)
   {
     if (!('rloggamma2' %in% ls(envir=.GlobalEnv)) & sdist!='beta')
       source('functions-loggamma.r')
     a <- Pars$a; cs <- Pars$cs; pb <- Pars$pb; sd_s <- Pars$ssd
     se0 <- Pars$se; si0 <- Pars$si; sa0 <- Pars$sa
     pK <- Pars$pK; cdd <- Pars$cdd
     fj <- Pars$fj; fm <- Pars$fm; ff <- Pars$ff
     thetadd <- Pars$theta
     k <- a-1 ## number of immature stages
     nimmdim <- k * Ntraj

       
     if (!exists('ntot_mat0', envir=.GlobalEnv))
       {
         nmf_mat0 <- ntot_mat0 <- kmf_mat0 <- matrix(rep(0, Maxyear*Ntraj))
         dim(nmf_mat0)  <- dim(kmf_mat0) <- c(Ntraj, Maxyear)
         dim(ntot_mat0) <- c(Maxyear, Ntraj)
         ki_mat0 <- matrix(rep(0, k*Maxyear*Ntraj)); dim(ki_mat0) <- c(k, Maxyear, Ntraj)
         ks_mat0 <- matrix(rep(0, (k+2)*Maxyear*Ntraj)); dim(ks_mat0) <- c(k+2, Maxyear, Ntraj)
       }

     if (is.null(attr(Pars,'eig')))
       {
         m <- lesmat(sa0, si0, se0*cs*pb/2, a) ## leslie matrix to get stable stage distribution
         eig <- eigen.analysis(m)
         attr(Pars, 'lesmat') <- m
         attr(Pars, 'eig') <- eig
       }

     ## intialise zero matrices (depend on a)
     ni_mat1 <- ki_mat0
     nam_mat1 <- naf_mat1 <- nmf_mat0
     ntot_mat1 <- ntot_mat0

     ## pre-fill first year (depend on K if exists, or start at n0K)
     if (!is.null(attr(Pars,'K')))
       tot0 <- round(attr(Pars,'K') * pK * attr(Pars, 'eig')$stable.stage) else
         if (exists('K', envir=.GlobalEnv))
           tot0 <- round(K/10 * attr(Pars,'eig')$stable.stage) else
              {
                n0K <- 0.5*(thetadd/10)*(1/cdd)
                tot0 <- round(n0K * attr(Pars,'eig')$stable.stage)
              }

     
     ni_mat1[,1,] <- tot0[1:k]
     nam_mat1[,1] <- round(tot0[k+1] * 0.5) # even sex-ratio
     naf_mat1[,1] <- tot0[k+1] - nam_mat1[,1]
     ntot_mat1[1,] <- sum(tot0) # colSums(rbind(ni[,1,], nam[,1], naf[,1]))

      
     ## pre-calculate environmental stochasticity
     ## sx [traj, year]
     u <- runif(Ntraj*Maxyear)
     sa <- envstoch(u, Ntraj, sa0, sd_s, SDIST)
     si <- envstoch(u, Ntraj, si0, sd_s, SDIST)
     se <- envstoch(u, Ntraj, se0, sd_s, SDIST)
     
     ## Traj init. Stable initial population structure
     ## ni [age class, year, traj]
     ## nam or naf [traj, year]
     ## ntot [year, traj]
     t = 1
     nam <- nam_mat1; naf <- naf_mat1; ntot <- ntot_mat1
     ni <- ni_mat1
     ks <- ks_mat0

     extinct = !any(ntot[t,]>0)
     
     t = 2
     burnin = 2
      
     while (t<=Maxyear & !extinct)      #Maxyear) #t=2 #t=t+1
       {
         ## density dependence
         se_ <- applyDD(se[,t], cdd, ntot[t-1,], thetadd, type=DDTYPE)
         sa_ <- applyDD(sa[,t], cdd, ntot[t-1,], thetadd, type=DDTYPE)
         pb_ <- applyDD(  pb  , cdd, ntot[t-1,], thetadd, type=DDTYPE)
         ## Breeding
         potpairs = pmin(nam[,t-1], naf[,t-1])
         nip = rbinom(Ntraj, cs*potpairs, se_*pb_) # number of immatures produced
         ## Natural mortality
         nam[,t] <- rbinom(Ntraj, nam[,t-1], sa_) # surviving male adults
         naf[,t] <- rbinom(Ntraj, naf[,t-1], sa_) # surviving female adults
         ni[,t,] <- rbinom(nimmdim, ni[,t-1,], matrix(si[,t], nrow=k, ncol=Ntraj, byrow=T)) # surving immatures

         ## Fishing mortality
         if (t >= burnin & Propkilled > 0)
           {
             if (k>1) totj = colSums(ni[,t,]) else totj = ni[,t,]
             totn = totj + nam[,t] + naf[,t]
             cond = totn > 0
             ntraj1 = sum(cond)
             if (ntraj1)
               {
                 ppj = totj[cond] / totn[cond]
                 ppm = nam[cond,t] / totn[cond]
                 ppf = naf[cond,t] / totn[cond]
                 sumfp = fj*ppj + fm*ppm + ff*ppf
                 kj = Propkilled*fj/sumfp
                 km = Propkilled*fm/sumfp
                 kf = Propkilled*ff/sumfp

                 if (ntraj1>1)
                   ks[,t,cond] = rbinom((k+2)*ntraj1, rbind(ni[,t,cond], nam[cond,t], naf[cond,t]), 
                       rbind(matrix(kj, nrow=k, ncol=ntraj1, byrow=T), km, kf)) else
                 ks[,t,cond] = rbinom((k+2)*ntraj1, c(ni[,t,cond], nam[cond,t], naf[cond,t]), 
                     c(rep(kj,k), km, kf))
                 if (any(is.na(ks[,t,cond])))
                   {
                     save.image(file='debug.rdata')
                     stop('Some NAs in ks. Environment saved in debug.rdata')
                   }
                 if (ntraj1)  ks[,t,!cond] <- 0
                 ni[,t,] = ni[,t,] - ks[1:k,t,]
                 nam[,t] = nam[,t] - ks[k+1,t,]
                 naf[,t] = naf[,t] - ks[k+2,t,]
               }
           }

         ## Transitions
         ni2ad = ni[k,t,]               # immatures becoming adults
         if (k>1)
           ni[2:k,t,] <- ni[1:(k-1),t,] # immatures moving to next immature stage
         ni[1,t,] <- nip                # new immatures produced
         ni2adf <- rbinom(Ntraj, ni2ad, .5) # fraction becoming females; sex ratio of .5
         naf[,t] <- naf[,t] + ni2adf # female immatures join female adults
         nam[,t] <- nam[,t] + (ni2ad-ni2adf) # male immatures join male adults
         ntot[t,] <- colSums(rbind(ni[,t,], nam[,t], naf[,t]))
         extinct <- !any(ntot[t,]>0)
         t = t+1
       }

     if (is.null(attr(Pars, 'K')))
       {
         if (Ktype == 'mean')
           K <- mean(ntot[Maxyear,]) else
             if (Ktype=='lcl')
               K <- quantile(ntot[Maxyear,], 0.05) else
                 stop('wrong Ktype')
         attr(Pars,'K') <- K
         attr(Pars,'Ktype') <- Ktype
         suslim <- K * Propklim 
         attr(Pars,'suslim') <- suslim
         n <- ntot[(Maxyear-9):Maxyear,]
         attr(Pars,'KLambdaMean') <- mean(n[2:nrow(n),]/n[1:(nrow(n)-1),]) # mean growth rate over all the trajectories for the last 10 years; should be really close to 1
       }

     propoverlim <- sum(ntot[Maxyear,] > attr(Pars,'suslim')) / Ntraj
     nextinct <- sum(ntot[Maxyear,] <= 10)  # quasi-extinction
     res <- list(ntot=ntot, nam=nam, naf=naf, ni=ni, ks=ks, propoverlim=propoverlim,
                 nextinct=nextinct, pars=Pars)
     
     return(res)
   }


###  Run simulation twice when carrying capacity is unknown
sim1 <- function(Pars, Ntraj=trajs, Maxyear=maxyear, Propkilled=0, 
                Ktype='lcl', Propklim=propklim, ddtype='BH2', ...)
  {
    r1 <- sim(Pars, Ntraj, Maxyear, Propkilled, Ktype, Propklim, ddtype, ...)
    r2 <- sim(r1$pars, Ntraj, Maxyear, Propkilled, Ktype, Propklim, ddtype, ...)
    return(r2)
  }


## xstart=0; incr=0.01; ylim=0.95; tol=0.00001
# Pars=ps; xstart=0; incr=0.01; ylim=0.950001; tol=0.00001; keep='last'
find_fcrit <- function(Pars, xstart=0, incr=0.01, ylim=0.950001, tol=0.00001, keep='last', ...) 
  {
    if (!(keep %in% c('none','some','all','last'))) stop('Wrong value for keep in find_crit()')
    if (keep %in% c('some','all'))    AllRes <- NULL
    xys <- NULL
    xi <- xstart
    res <- sim(Pars, trajs, maxyear, Propkilled=xi,  ...)
#    res <- sim(Pars, trajs, maxyear, Propkilled=xi)
    Pars <- res$pars                   # update Pars with K, suslim...
    yi <- res[['propoverlim']]
    prevchange = ifelse(yi > ylim, 1, -1)
    samedir = 0
    steps = 0
    stillhunt = T
    lx = 0; ux = 0.2
    first <- T
    ## 1- Hunt: start anywhere then move with increasing step until limit is passed
    while (stillhunt)
      {
        steps = steps + 1
        if (!first)
          {
            res <- sim(Pars, trajs, maxyear, Propkilled=xi, ...)
#            res <- sim(Pars, trajs, maxyear, Propkilled=xi)
            yi <- res[['propoverlim']]
          } else first <- F
        xys = rbind(xys, c(xi, yi, res[['nextinct']]))
        if (keep %in% 'all')    AllRes[[steps]] <- list(xi=xi, res=res[!(names(res) %in% 'pars')])
        if (keep %in% 'some')
          AllRes[[steps]] <- list(xi=xi, res=res[!(names(res) %in% c('ni','ks','pars'))])
        if (yi < ylim & xi == 0)  return(cbind(xys, xys[,2]-ylim))
        if (prevchange > 0 & yi > ylim)   lx = xi
        if (prevchange < 0 & yi < ylim)   ux = xi
        if (prevchange > 0 & yi < ylim)
          {
            ux = xi
            stillhunt = F
          }
        if (prevchange < 0 & yi > ylim)
          {
            lx = xi
            stillhunt = F
          }
        if (stillhunt) 
          {
            incr = incr * 2
            xi = xi + prevchange * incr
          } else
        {
          incr = incr / 2
          prevchange = -1 * prevchange
          xi = xi + prevchange * incr
        }
        xi = max(0, xi)
      }

    ## 2- Bisection: halve the step
    while (abs(yi - ylim) > tol  &  incr > 1e-6  &  incr != tol)
      {
                                        # cat('.')
        steps = steps + 1
        res <- sim(Pars, trajs, maxyear, Propkilled=xi, ...)
#        res <- sim(Pars, trajs, maxyear, Propkilled=xi)
        yi = res[['propoverlim']]
        xys = rbind(xys, c(xi, yi, res[['nextinct']]))
        if (keep %in% 'all')    AllRes[[steps]] <- list(xi=xi, res=res[!(names(res) %in% 'pars')])
        if (keep %in% 'some')    AllRes[[steps]] <- list(xi=xi,
                                                         res=res[!(names(res) %in% c('ni','ks','pars'))])
        dir = sign(yi-ylim)
        if (dir > 0)
          lx = xi else
        ux = xi
        incr = incr/2
        xi = xi + dir * incr
      }
	
    xys = cbind(xys, xys[,2]-ylim)
    colnames(xys) <- c('fc','poverlim','nextinct','diff95')
    
    if (keep %in% 'none') return(list(steps=xys))
    if (keep %in% 'last') return(list(steps=xys, lastsim=res))
    if (keep %in% 'all')  return(list(steps=xys, all=AllRes))
    if (keep %in% 'some') return(list(steps=xys, lastsim=res, all=AllRes))
           
    return(xys)
  }
