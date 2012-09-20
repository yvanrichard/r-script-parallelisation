##########################################################################################
###  FOR PARAMETER SAMPLES
##########################################################################################

## lesmat <- function(Sa, Si, F, A)
##   {
##     m <- matrix(0, nrow=A, ncol=A)
##     m[A,A] <- Sa
##     m[1,A] <- F
##     for (i in 1:(A-1))
##       m[i+1,i] <- Si
##     return(m)
##   }

popsim <- function(Sa,Si,F, a, ny, nsims, n0=1e4)  # Sa=0.96; Si=0.9
  {
    ## Simple population model, nsims simulations of ny years, given time series of Sa, Si and F
    mats <- rep(0, a*a*ny*nsims)
    dim(mats) <- c(a,a,ny,nsims)
    mats[1,a,,] <- F
    for (ai in 1:(a-1))
      mats[ai+1,ai,,] <- Si
    mats[a,a,,] <- Sa
    ea <- eigen.analysis(lesmat(mean(Sa), mean(Si), mean(F), a))
    N0 <- n0 * ea$stable.stage
    N <- matrix(NA, nrow=nsims, ncol=ny)
    Ns0 <- matrix(NA, nrow=a, ncol=ny)
    Ns0[,1] <- N0
    for (sim in 1:nsims)
      {
        Ns <- Ns0
        for (t in 2:ny)
          {
            Ns[,t] <- round(mats[,,t,sim] %*% Ns[,t-1])
          }
        N[sim,] <- colSums(Ns)
      }
    return(N)
  }


plotN <- function(N, nl=200, quant=0.999)
  {
    ## Plot a sample of trajectories, from N calculated from popsim(), i.e. with trajectories as rows and years as columns
    par(mar=c(4,4,1,0))
    maxN <- quantile(N, quant)
    if (nl>nrow(N)) nl=nrow(N)
    ll <- sample(1:nrow(N), nl)
    layout(matrix(c(1,1,1,2), 1, 4))  #layout.show(2)
    plot(NA, xlim=c(1,ncol(N)), ylim=c(0,maxN), xlab='Years', ylab='Ntot', xaxs='i',
         yaxs='i')
    for (l in 1:nl)
      lines(1:ncol(N), N[l,], col='#00000022')
    lines(1:ncol(N), colMeans(N), col='red')

    d <- density(N[,ncol(N)])
    par(mar=c(4,0,1,0))
    plot(NA, xlim=c(0,max(d$y)), ylim=c(0,maxN), xaxs='i', yaxs='i', xaxt='n',
         yaxt='n', xlab=NA, ylab=NA)
    lines(d$y, d$x, col='blue')
    mn <- mean(N[,ncol(N)])
    abline(h=mn, col='red', lty=3)
    text(0, mn, sprintf('%0.0f',mn), adj=c(-0.2,-1), col='red')
  }

stochgr <- function(N, burnin=10)
  {
    ## Calculates stochastic growth rate of trajectories, given N from popsim()
    if (burnin >= (ncol(N)-1))
      stop('Burnin too large compared to number of time steps')
    return( mean((N[,ncol(N)]/N[,1+burnin])^(1/(ncol(N)-1-burnin))) )
  }


beta_shp1 <- function(mu, sd)
    return(mu*(mu*(1-mu)/(sd^2) - 1))  ## from Samaranayaka & Fletcher 2010

beta_shp2 <- function(mu, sd)
    return((1-mu)*(mu*(1-mu)/(sd^2) - 1))   ## from Samaranayaka & Fletcher 2010


lambdas <- function(sa, si, se, ssd, a, pb, cs, ny, nsims, n0=1e4, burnin=10, plot=FALSE, type='nlg')
  {
    ## Run population simulations from arithmetic means of sa, si, se and associated
    ## standard deviation and calculate deterministic and stochastic growth rates,
    ## as well as exctinction probabilities
    u <- runif(ny*nsims)
    if (type=='nlg')
      {
        Sa <- rloggamma2(u, a=sa, theta=Theta(a=sa, Sd=ssd))
        Si <- rloggamma2(u, a=si, theta=Theta(a=si, Sd=ssd))
        Se <- rloggamma2(u, a=se, theta=Theta(a=se, Sd=ssd))
      } else if (type=='beta')
    {
        Sa <- qbeta(u, shape1=beta_shp1(sa, ssd), shape2=beta_shp2(sa, ssd))
        Si <- qbeta(u, shape1=beta_shp1(si, ssd), shape2=beta_shp2(si, ssd))
        Se <- qbeta(u, shape1=beta_shp1(se, ssd), shape2=beta_shp2(se, ssd))
    } else stop('Wrong type of stochasticity distribution')

    F <- Se*pb*cs/2
    N <- popsim(Sa, Si, F, a, ny, nsims, n0)
    nextinct <- sum(N[,ny]==0)
    pextinct <- nextinct/nsims
    if (plot==TRUE)
      plotN(N)
    ea <- eigen.analysis(lesmat(mean(Sa), mean(Si), mean(F), a))
    return(list(lambda1=ea$lambda1, lambdas=stochgr(N, burnin), nextinct=nextinct, pextinct=pextinct))
  }



diffL_sa <- function(sa, si, fec, a, ssd, lambda1)
  { # function for optimise() in adjust_sa(); uses Tuljapurkar (1982)
    m <- lesmat(sa, si, fec, a)
    ea <- eigen.analysis(m)
    l <- ea$lambda1
    ssa <- ea$sensitivities[a,a]
    return((log(l)-1/(l^2)*ssa^2*ssd^2 - log(lambda1))^2)
  }

adjust_sa <- function(sa, si, f, a, ssd)
  { # adjust sa so that deterministic lambda matches stochastic one
    m0 <- lesmat(sa, si, f, a)
    l1 <- eigen.analysis(m0)$lambda1
    A. <- optimise(diffL_sa, c(sa, 1), si=si, fec=f, a=a, ssd=ssd, lambda1=l1, tol=1e-8)$minimum
    return(A.)
  }


diffL_si <- function(sa, si, fec, a, ssd, lambda1)
  { # function for optimise() in adjust_si(); uses Tuljapurkar (1982)
    m <- lesmat(sa, si, fec, a)
    ea <- eigen.analysis(m)
    l <- ea$lambda1
    ssi <- (a-1)*ea$sensitivities[2,1]
    return((log(l)-1/(l^2)*ssi^2*ssd^2 - log(lambda1))^2)
  }

adjust_si <- function(sa, si, f, a, ssd)
  { # adjust si so that deterministic lambda matches stochastic one
    m0 <- lesmat(sa, si, f, a)
    l1 <- eigen.analysis(m0)$lambda1
    A. <- optimise(diffL_si, c(si, 1), sa=sa, fec=f, a=a, ssd=ssd, lambda1=l1, tol=1e-8)$minimum
    return(A.)
  }


diffL_se <- function(sa, si, se, cs, pb, a, ssd, lambda1)
  { # function for optimise() in adjust_se(); uses Tuljapurkar (1982)
    m <- lesmat(sa, si, se*cs*pb/2, a)
    ea <- eigen.analysis(m)
    l <- ea$lambda1
    sse <- ea$sensitivities[1,a]
    return((log(l)-1/(l^2)*sse^2*ssd^2 - log(lambda1))^2)
  }

adjust_se <- function(sa, si, se, cs, pb, a, ssd)
  { # adjust se so that deterministic lambda matches stochastic one
    m0 <- lesmat(sa, si, se*cs*pb/2, a)
    l1 <- eigen.analysis(m0)$lambda1
    A. <- optimise(diffL_se, c(se, 1), sa=sa, si=si, cs=cs, pb=pb, a=a, ssd=ssd, lambda1=l1, tol=1e-8)$minimum
    return(A.)
  }


##########################################################################################
###  FOR SIMULATIONS
##########################################################################################

## Mathematical functions
logit <- function(x)
    {
    return(log(x/(1-x)))
    }
invlogit <- function(x)
    {
    return(exp(x)/(1+exp(x)))
    }

## Survival variation
surv_var_norm_mean_se <- function(n=1000, smean, sse)
    {
    se_beta <- sse/(smean*(1-smean)) ## calculate se(beta(S)) from se(S), uses delta method
    logit_s <- rnorm(n=n, mean=log(smean/(1-smean)), sd=se_beta) ## apply normal variation to logit(s)
    surv <- exp(logit_s)/(1+exp(logit_s)) ## back-transform
    return(surv)
    }
surv_ci <- function(smean, sse)
    {
    se_beta <- sse/(smean*(1-smean)) ## calculate se(beta(S)) from se(S)
    logit_s <- log(smean/(1-smean))
    ll <- logit_s-1.96*se_beta
    ul <- logit_s+1.96*se_beta
    ll <- exp(ll) / (1 + exp(ll))
    ul <- exp(ul) / (1 + exp(ul))
    return(data.frame(ll=ll, ul=ul))
    }
surv_meansd_from_ci <- function(lcl, ucl)
    {
    logitlcl <- log(lcl/(1-lcl))
    logitucl <- log(ucl/(1-ucl))
    sdlogit <- (logitucl-logitlcl) / (2*1.96)
    meanlogit <- mean(c(logitlcl,logitucl))
    mean <- exp(meanlogit)/(1+exp(meanlogit))
    sd <- sdlogit*(mean*(1-mean))
    return(data.frame(mean=mean, sd=sd))
    }

## formula from Neil & Lebreton 2008
funNL<- function(x,a,s) (exp((a+s/(x-s))^-1)-x)^2
lmax_nl <- function(a,s) return(optimise(funNL, c(1,2), a=a, s=s, tol=1e-10)$minimum)
Lmax_nl <- function(a,s,usemc=F)
  {
    if (usemc)
      {
        library(parallel)
        return(mcmapply(lmax_nl, a, s))
      } else return(mapply(lmax_nl, a, s))
  }
Rmax_NL <- function(s,a,...) return(Lmax_nl(a,s,...)-1)

nbp_2_ntot <- function(nbp, pb, ntot_na_rat)
  return(2*nbp/pb * ntot_na_rat)

na_2_nbp <- function(na_tot, pb)
  return(na_tot*pb/2)

na_2_ntot <- function(na, ntot_na_rat)
  return(na * ntot_na_rat)


ntot_na_rat_g <- function(s, a)
  return(s^(1-a))
  

PBR <- function(rmax, ntot, f=1)
  return( 0.5 * rmax * ntot * f )


  
## Demography
## Create bh-dd-k50 Leslie matrix
les_mat <- function(A, Se, Si, Sa, Pb, Cs)  # A=a;Se=se0;Si=si0;Sa=sa0;Pb=pb;Cs=cs
    {
    nrows = A
    m <- matrix(0, nrow=nrows, ncol=nrows)
    ## juv surv
    for (i in 1:(A-1))
	m[i+1,i] <- Si
    ## ad surv
    m[A,A] = Sa
    ## reprod
    B <- Pb*Se*Cs/2   ## see parameters
    m[1, nrows] <- B
#     eig <- eigen.analysis(m)
#     ntot0 = round(2*kb/(eig$stable.stage[a]*pb))
    return(m)
    }

    
## Create bh-dd-k50 Leslie matrix WITH FISHING MORTALITY
les_mat_f <- function(A, Se, Si, Sa, Pb, Cs, F)  # A=a;Se=se0;Si=si0;Sa=sa0;Pb=pb;Cs=cs
    {
    nrows = A
    m <- matrix(0, nrow=nrows, ncol=nrows)
    ## juv surv
    for (i in 1:(A-1))
	m[i+1,i] <- Si*(1-F)
    ## ad surv
    m[A,A] = Sa*(1-F)
    ## reprod
    B <- Pb*Se*Cs/2   ## see parameters
    m[1, nrows] <- B
#     eig <- eigen.analysis(m)
#     ntot0 = round(2*kb/(eig$stable.stage[a]*pb))
    return(m)
    }




adratio_calc <- function()
    {
    adrat_ <<- sa_^(1-a_)
    }

ntot_calc <- function()
    {
    ntot_ <<- na_*adrat_/pb_
    }

nmin_calc <- function()
    {
    nest = ntot_
    mu = log(nam+naf)
    sdlogn <- sqrt((exp(sdn^2)-1)*exp(2*mu+sdn^2))   # from Wikipedia page on log-normal distribution
    nmin_ <<- nest*exp(z20*sqrt(log(1+(sdlogn/nest)^2))) # from Dillingham & Fletcher 08
    }

rmax_calc <- function()
    {
    lmax <- optimise(NL, c(1,2), a=a_, s=sa_)$minimum
    rmax_ <<- lmax - 1
    }

pbr_calc <- function()
    {
    pbr_ <<- 0.5 * f * nmin_ * rmax_
    }


## Get probability of breeding from density-dependent number of breeding pairs
#     x1 = kb*ddc/pb
#     x2 = kb/pb*(1+(1-ddc)*sqrt(1+pb^2))

nbpairs <- function(potp, DDC=ddc, KB=kb, PB=pb, X1=x1, X2=x2)
    {
	tt = suppressWarnings((-sqrt((DDC-1)*KB*(KB*(sqrt(PB^2+1)*DDC-1)-
		PB*(sqrt(PB^2+1)-1)*potp))-DDC*KB+KB)/((sqrt(PB^2+1)-1)*(DDC-1)*KB))
	return(ifelse(potp<X1, PB*potp, ifelse(potp>X2, KB,
		KB*(DDC + 2*tt*(1-DDC) - tt^2*(1-DDC)))))
    }

pbreed <- function(x) { # x=number of potential pairs
    r = nbpairs(x)/x
    return(ifelse(is.finite(r), r, 0))
#     return(y/x)
    }

nbpairs2 <- function(potp, DDC=ddc, KB=kb, PB=pb, X1=KB*DDC/PB, X2=KB/PB*(1+(1-DDC)*sqrt(1+PB^2)))
  {
	tt = suppressWarnings((-sqrt((DDC-1)*KB*(KB*(sqrt(PB^2+1)*DDC-1)-
		PB*(sqrt(PB^2+1)-1)*potp))-DDC*KB+KB)/((sqrt(PB^2+1)-1)*(DDC-1)*KB))
	return(ifelse(potp<X1, PB*potp, ifelse(potp>X2, KB,
		KB*(DDC + 2*tt*(1-DDC) - tt^2*(1-DDC)))))
    }

pbreed2 <- function(potp, DDC=ddc, KB=kb, PB=pb, X1=KB*DDC/PB, X2=KB/PB*(1+(1-DDC)*sqrt(1+PB^2)))
  { # x=number of potential pairs
    r = nbpairs2(potp, DDC, KB, PB, X1, X2)/potp
    return(ifelse(is.finite(r), r, 0))
                                        #     return(y/x)
  }


findK <- function(Pars, n0=1e4)  # Pars=pars; Pars$Kpairs <- 2000
  {
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


## findKlcl <- function(Pars, n0=1e4, p=0.95)  # Pars=dem1[1,]; n0=1e4; p=0.95
##   { ## UNFINISHED
##     a <- Pars$a; sa <- Pars$sa; se <- Pars$se; si <- Pars$si; cs <- Pars$cs; pb <- Pars$pb
##     c_sa <- Pars$c_sa; c_se <- Pars$c_se; c_pb <- Pars$c_pb
##     lm <- attr(Pars, 'lesmat')  #lesmat(sa, si, cs*se*pb/2, a)

##     ss <- attr(Pars, 'eig')$stable.stage
##     N0 <- ss*n0
##     #prevn <- n0*2
##     N <- NULL
##     t <- 1
##     N <- cbind(N, N0)
##     newn <- n0; prevn <- 0
##     while (abs(1 - newn/prevn) > 0.00001 | t<4)
##       {
##         prevn <- newn
##         t <- t+1

##         ## density dependence
##         se_ <- se/(1 + c_se * newn)
##         sa_ <- sa/(1 + c_sa * newn)
##         pb_ <- pb/(1 + c_pb * newn)

##         lm[1,a] <- .5*cs*se_*pb_
##         lm[a,a] <- sa_
##         newN <- lm %*% N[,t-1]
##         newn <- sum(newN)
##         N <- cbind(N, newN)
##       }
##     #plot(apply(N,2,sum))
##     #N[,t]
##     nk <- N[,t]
##     attr(nk, 'found_in') <- t
##     return(nk)
##   }



## sim <- function(Pars, Ntraj=trajs, Maxyear=maxyear, Propkilled=0, Singlerun=FALSE,
##                 Ktype='lcl', Propklim=propklim)  # Pars=ps; Ntraj=trajs; Maxyear=maxyear; Propkilled=0; Singlerun=T; Ktype='lcl'; Propklim=propklim
##    {
##      if (!('rloggamma2' %in% ls(envir=.GlobalEnv)))
##        source('functions-loggamma.r')
##      a <- Pars$a; cs <- Pars$cs; pb <- Pars$pb; sd_s <- Pars$ssd
##      se0 <- Pars$se; si0 <- Pars$si; sa0 <- Pars$sa
##      c_sa <- Pars$c_sa; c_se <- Pars$c_se; c_pb <- Pars$c_pb
##      pK <- Pars$pK
##      k <- a-1 ## number of immature stages
##      nimmdim = k*Ntraj

##      if (!exists('ntot_mat0', envir=.GlobalEnv))
##        {
##          nmf_mat0 <- ntot_mat0 <- kmf_mat0 <- matrix(rep(0, Maxyear*Ntraj))
##          dim(nmf_mat0)  <- dim(kmf_mat0) <- c(Ntraj, Maxyear)
##          dim(ntot_mat0) <- c(Maxyear, Ntraj)
##          ki_mat0 <- matrix(rep(0, k*Maxyear*Ntraj)); dim(ki_mat0) <- c(k, Maxyear, Ntraj)
##          ks_mat0 <- matrix(rep(0, (k+2)*Maxyear*Ntraj)); dim(ks_mat0) <- c(k+2, Maxyear, Ntraj)
##        }

##      if (is.null(attr(Pars,'eig')))
##        {
##          m <- lesmat(sa0, si0, se0*cs*pb/2, a) ## leslie matrix to get stable stage distribution
##          eig <- eigen.analysis(m)
##          attr(Pars, 'lesmat') <- m
##          attr(Pars, 'eig') <- eig
##        }

##      ## intialise zero matrices (depend on a)
##      ni_mat1 <- ki_mat0
##      nam_mat1 <- naf_mat1 <- nmf_mat0
##      ntot_mat1 <- ntot_mat0

##      ## pre-fill first year (depend on K if exists, or start at n0K)
##      if (!is.null(attr(Pars,'K')))
##        tot0 <- round(attr(Pars,'K') * pK * attr(Pars, 'eig')$stable.stage) else
##      tot0 <- round(n0K * attr(Pars,'eig')$stable.stage)
      
##      ni_mat1[,1,] <- tot0[1:k]
##      nam_mat1[,1] <- round(tot0[k+1] * 0.5) # even sex-ratio
##      naf_mat1[,1] <- tot0[k+1] - nam_mat1[,1]
##      ntot_mat1[1,] <- sum(tot0) # colSums(rbind(ni[,1,], nam[,1], naf[,1]))

      
##      ## pre-calculate environmental stochasticity
##      ## sx [traj, year]
##      u <- runif(Ntraj*Maxyear)
##      sa <- matrix(rloggamma2(u, a=sa0, theta=Theta(a=sa0, Sd=sd_s)), nrow=Ntraj)
##      si <- matrix(rloggamma2(u, a=si0, theta=Theta(a=si0, Sd=sd_s)), nrow=Ntraj)
##      se <- matrix(rloggamma2(u, a=se0, theta=Theta(a=se0, Sd=sd_s)), nrow=Ntraj)
     
##      ## Traj init. Stable initial population structure
##      ## ni [age class, year, traj]
##      ## nam or naf [traj, year]
##      ## ntot [year, traj]
##      t = 1
##      nam <- nam_mat1; naf <- naf_mat1; ntot <- ntot_mat1
##      ni <- ni_mat1
##      ks <- ks_mat0

##      extinct = !any(ntot[t,]>0)
    
##      t = 2
##      burnin = 2
      
##      while (t<=Maxyear & !extinct)      #Maxyear) #t=2 #t=t+1
##        {
##          ## density dependence
##          se_ <- se[,t]/(1 + c_se * ntot[t-1,])
##          sa_ <- sa[,t]/(1 + c_sa * ntot[t-1,])
##          pb_ <- pb/(1 + c_pb * ntot[t-1,])
##          ## Breeding
##          potpairs = pmin(nam[,t-1], naf[,t-1])
##          nip = rbinom(Ntraj, cs*potpairs, se_*pb_) # number of immatures produced
##          ## Natural mortality
##          nam[,t] <- rbinom(Ntraj, nam[,t-1], sa_) # surviving male adults
##          naf[,t] <- rbinom(Ntraj, naf[,t-1], sa_) # surviving female adults
##          ni[,t,] <- rbinom(nimmdim, ni[,t-1,], matrix(si[,t], nrow=k, ncol=Ntraj, byrow=T)) # surving immatures

##          ## Fishing mortality
##          if (t >= burnin & Propkilled > 0)
##            {
##              if (k>1) totj = colSums(ni[,t,]) else totj = ni[,t,]
##              totn = totj + nam[,t] + naf[,t]
##              cond = totn > 0
##              ntraj1 = sum(cond)
##              if (ntraj1)
##                {
##                  ppj = totj[cond] / totn[cond]
##                  ppm = nam[cond,t] / totn[cond]
##                  ppf = naf[cond,t] / totn[cond]
##                  sumfp = fj*ppj + fm*ppm + ff*ppf
##                  kj = Propkilled*fj/sumfp
##                  km = Propkilled*fm/sumfp
##                  kf = Propkilled*ff/sumfp

##                  if (ntraj1>1)
##                    ks[,t,cond] = rbinom((k+2)*ntraj1, rbind(ni[,t,cond], nam[cond,t], naf[cond,t]), 
##                        rbind(matrix(kj, nrow=k, ncol=ntraj1, byrow=T), km, kf)) else
##                  ks[,t,cond] = rbinom((k+2)*ntraj1, c(ni[,t,cond], nam[cond,t], naf[cond,t]), 
##                      c(rep(kj,k), km, kf))
##                  if (any(is.na(ks[,t,cond])))
##                    {
##                      save.image(file='debug.rdata')
##                      stop('Some NAs in ks. Environment saved in debug.rdata')
##                    }
##                  if (ntraj1)  ks[,t,!cond] <- 0
##                  ni[,t,] = ni[,t,] - ks[1:k,t,]
##                  nam[,t] = nam[,t] - ks[k+1,t,]
##                  naf[,t] = naf[,t] - ks[k+2,t,]
##                }
##            }

##          ## Transitions
##          ni2ad = ni[k,t,]               # immatures becoming adults
##          if (k>1)
##            ni[2:k,t,] <- ni[1:(k-1),t,] # immatures moving to next immature stage
##          ni[1,t,] <- nip                # new immatures produced
##          ni2adf = rbinom(Ntraj, ni2ad, .5) # fraction becoming females; sex ratio of .5
##          naf[,t] <- naf[,t] + ni2adf # female immatures join female adults
##          nam[,t] <- nam[,t] + (ni2ad-ni2adf) # male immatures join male adults
##          ntot[t,] <- colSums(rbind(ni[,t,], nam[,t], naf[,t]))
##          extinct = !any(ntot[t,]>0)
##          t = t+1
##        }

##      if (is.null(attr(Pars, 'K')))
##        {
##          if (Ktype == 'mean')
##            K <- mean(ntot[Maxyear,]) else
##              if (Ktype=='lcl')
##                K <- quantile(ntot[Maxyear,], 0.05) else
##                  stop('wrong Ktype')
##          attr(Pars,'K') <- K
##          attr(Pars,'Ktype') <- Ktype
##          suslim <- K * Propklim 
##          attr(Pars,'suslim') <- suslim
##          n <- ntot[(Maxyear-9):Maxyear,]
##          attr(Pars,'KLambdaMean') <- mean(n[2:nrow(n),]/n[1:(nrow(n)-1),]) # mean growth rate over all the trajectories for the last 10 years; should be really close to 1
##        }

##      propoverlim <- sum(ntot[Maxyear,] > attr(Pars,'suslim')) / Ntraj
##      nextinct <- sum(ntot[Maxyear,] <= 10)  # quasi-extinction
##      res <- list(ntot=ntot, nam=nam, naf=naf, ni=ni, ks=ks, propoverlim=propoverlim,
##                  nextinct=nextinct, pars=Pars)
     
##      return(res)
##    }


plot_res <- function(res, ntrajs=200) # res=x
  {
    ntot <- res[['ntot']]
    pars <- res[['pars']]
    trjs = sample(1:ncol(ntot), ntrajs)
    K <- attr(pars,'K')
    plot(NA, xlim=c(1,maxyear), ylim=c(0,max(max(ntot[,trjs]),K)*1.01),
         xaxs='i', yaxs='i', xlab='years', ylab='n')
    for (t in trjs)
      {
        lines(1:maxyear, ntot[,t], col='#00000022') #
      }
    lines(1:maxyear, ntot[,sample(1:trajs,1)], col='green')
    abline(h=attr(pars, 'suslim'), col='red')
    abline(h=K, col='blue')
  }



## xstart=0; incr=0.01; ylim=0.95; tol=0.00001
## find_fcrit <- function(Pars, xstart=0, incr=0.01, ylim=0.950001, tol=0.00001, keep='lastx', ...) # Pars=ps; xstart=0; incr=0.01; ylim=0.950001; tol=0.00001; keep='none'
##   {
##     if (!(keep %in% c('none','some','all','last'))) stop('Wrong value for keep in find_crit()')
##     if (keep %in% c('some','all'))    AllRes <- NULL
##     xys <- NULL
##     xi <- xstart
##     res <- sim(Pars, trajs, maxyear, Propkilled=xi, ...)
## #    res <- sim(Pars, trajs, maxyear, Propkilled=xi)
##     Pars <- res$pars                   # update Pars with K, suslim...
##     yi <- res[['propoverlim']]
##     prevchange = ifelse(yi > ylim, 1, -1)
##     samedir = 0
##     steps = 0
##     stillhunt = T
##     lx = 0; ux = 0.2
##     first <- T
##     ## 1- Hunt: start anywhere then move with increasing step until limit is passed
##     while (stillhunt)
##       {
##         steps = steps + 1
##         if (!first)
##           {
## #            res <- sim(Pars, trajs, maxyear, Propkilled=xi, ...)
##             res <- sim(Pars, trajs, maxyear, Propkilled=xi)
##             yi <- res[['propoverlim']]
##           } else first <- F
##         xys = rbind(xys, c(xi, yi, res[['nextinct']]))
##         if (keep %in% 'all')    AllRes[[steps]] <- list(xi=xi, res=res[!(names(res) %in% 'pars')])
##         if (keep %in% 'some')
##           AllRes[[steps]] <- list(xi=xi, res=res[!(names(res) %in% c('ni','ks','pars'))])
##         if (yi < ylim & xi == 0)  return(cbind(xys, xys[,2]-ylim))
##         if (prevchange > 0 & yi > ylim)   lx = xi
##         if (prevchange < 0 & yi < ylim)   ux = xi
##         if (prevchange > 0 & yi < ylim)
##           {
##             ux = xi
##             stillhunt = F
##           }
##         if (prevchange < 0 & yi > ylim)
##           {
##             lx = xi
##             stillhunt = F
##           }
##         if (stillhunt) 
##           {
##             incr = incr * 2
##             xi = xi + prevchange * incr
##           } else
##         {
##           incr = incr / 2
##           prevchange = -1 * prevchange
##           xi = xi + prevchange * incr
##         }
##         xi = max(0, xi)
##       }

##     ## 2- Bisection: halve the step
##     while (abs(yi - ylim) > tol  &  incr > 1e-6  &  incr != tol)
##       {
##                                         # cat('.')
##         steps = steps + 1
## #        res <- sim(Pars, trajs, maxyear, Propkilled=xi, ...)
##         res <- sim(Pars, trajs, maxyear, Propkilled=xi)
##         yi = res[['propoverlim']]
##         xys = rbind(xys, c(xi, yi, res[['nextinct']]))
##         if (keep %in% 'all')    AllRes[[steps]] <- list(xi=xi, res=res[!(names(res) %in% 'pars')])
##         if (keep %in% 'some')    AllRes[[steps]] <- list(xi=xi,
##                                                          res=res[!(names(res) %in% c('ni','ks','pars'))])
##         dir = sign(yi-ylim)
##         if (dir > 0)
##           lx = xi else
##         ux = xi
##         incr = incr/2
##         xi = xi + dir * incr
##       }
	
##     xys = cbind(xys, xys[,2]-ylim)
##     colnames(xys) <- c('fc','poverlim','nextinct','diff95')
    
##     if (keep %in% 'none') return(list(steps=xys))
##     if (keep %in% 'last') return(list(steps=xys, lastsim=res))
##     if (keep %in% 'all')  return(list(steps=xys, all=AllRes))
##     if (keep %in% 'some') return(list(steps=xys, lastsim=res, all=AllRes))
           
##     return(xys)
##   }



##########################################################################################
###  FOR PLOTS
##########################################################################################

alpha <- function (colour, alpha) 
  {
    alpha[is.na(alpha)] <- 0
    col <- col2rgb(colour, TRUE)/255
    if (length(colour) != length(alpha)) {
      if (length(colour) > 1 && length(alpha) > 1) {
        stop("Only one of colour and alpha can be vectorised")
      }
      if (length(colour) > 1) {
        alpha <- rep(alpha, length.out = length(colour))
      }
      else if (length(alpha) > 1) {
        col <- col[, rep(1, length(alpha)), drop = FALSE]
      }
    }
    col[4, ] <- ifelse(col[4, ] == 1, alpha, col[4, ])
    new_col <- rgb(col[1, ], col[2, ], col[3, ], col[4, ])
    new_col[is.na(colour)] <- NA
    return(new_col)
  }
    
plotlines <- function(df, ind=1:ncol(df), alpha=1, col.shuffle=F, pal='Spectral', ...) # df=ntot
  {
    library(RColorBrewer)
    colf = colorRampPalette(brewer.pal(brewer.pal.info[pal,'maxcolors'], pal))
    cols = alpha(colf(length(ind)), alpha)
    if (col.shuffle)
      cols = cols[sample(1:length(cols))]
    plot(NA, xlim=c(1,nrow(df)), ylim=c(0,max(df[,ind])), ...)
    for (i in 1:length(ind))
      lines(df[,ind[i]], col=cols[i])
  }
    
draw.env <- function(x=1:length(lcl), lcl, ucl, fill=gray(0.9), line=gray(0.75))
    {
    x = 1:length(lcl)
    polygon(c(x,rev(x),x[1]),c(ucl, rev(lcl), ucl[1]), border=NA, col=fill)
    lines(x, ucl, col=line)
    lines(x, lcl, col=line)
    }



## Calculate density of y for each x, and plot them on same plot
# col='red'; alph=0.5; col.border=NULL; xmin=NULL; at=NULL; ylevels=NULL; ylab=NA; cap1st=T
# alph=0.5; col.border=NULL; xmin=NULL; cap1st=T; gridx=0; gridy=0; gridcol=grey(0.8)
plotdensapply <- function(y, by, col='red', alph=0.5, col.border=NULL, xmin=NULL, at=NULL,
                          ylevels=NULL, ylab=NA, cap1st=T, gridx=0, gridy=0,
                          gridcol=grey(0.8), ...)
  {
    if (!is.factor(by))
      by <- factor(by, levels=sort(unique(by)))
    by <- factor(by, levels=rev(levels(by)))
    spp <- levels(by)
    col <- rev(col); col.border <- rev(col.border)
    nspp <- nlevels(by)
    dd <- tapply(y, by, function(x) density(x, from=min(x, na.rm=T), to=max(x, na.rm=T)))
    minx <- min(sapply(dd, function(x) min(x$x)))
    maxx <- max(sapply(dd, function(x) max(x$x)))
    rngx <- extendrange(c(minx, maxx), f=0.04)
    ## plot(NA, xlim=c(ifelse(is.null(xmin), minx*0.1, xmin), maxx*1.02), ylim=c(0,nspp+1), ylab=NA, yaxs='i', xaxs='i', yaxt='n')
    plot(NA, xlim=switch(is.null(xmin)+1, c(xmin, rngx[2]), rngx), ylim=c(0,nspp+1),
         ylab=ylab, yaxs='i', xaxs='i', yaxt='n', ...)
    grid(gridx, gridy, gridcol)
    if (is.null(at))   at <- 1:nspp  else  at <- rev(at)
    if (length(at) != nspp) stop('Length of "at" does not match that of levels of "by"')
    if (length(col)==1)  col <- rep(col, nspp)
    if (is.null(col.border))  col.border <- col
    names(col) <- names(col.border) <- spp
    at <- as.vector(at); names(at) <- spp
    for (di in 1:nspp)  # di=nspp
      {
        sp <- spp[di]
        d1 <- dd[[sp]]
        d1$y2 <- d1$y/max(d1$y)
        polygon(c(d1$x, rev(d1$x)), .5*c(d1$y2,-rev(d1$y2))+at[sp], col=alpha(col[sp],alph),
                border=col.border[sp], lwd=.5)
      }
    if (is.null(ylevels)) ylevels <- spp  else ylevels <- rev(ylevels)
    z <- sapply(tapply(ylevels, at, unique), function(x) paste(x, collapse=','))
    ylev2 <- z; at2 <- as.numeric(names(z))
    if (cap1st) ylev2 <- upper1st(ylev2)
    mtext(ylev2, 2, at=at2, las=1, line=0.5)
  }




##########################################################################################
###  UTILS
##########################################################################################


makeseq <- function(x)        # x='2,10-15,103'
  {
    x <- unlist(strsplit(x, ','))
    areranges <- grepl('-',x)
    return(sort(unique(c(as.numeric( x[!areranges]), unlist(sapply(x[areranges],
          function(x) {
          x <- as.numeric(unlist(strsplit(x, '-')))
          return(seq(x[1],x[2]))
          }))))))
  }


collapseseq <- function(x)  # x=c(2,3,4,6,7,9,10,11,12,16)
  {
    if (length(x)>1)
      {
    x <- sort(unique(x))                # just in case
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
    return(list(seq=rs, n=ns))
  } else
      return(list(seq=as.character(x), n=1))
  }

rbind2 <- function(a,b) # a=data.frame(a=c(1,2,3), b=c(4,5,6), c=c(7,8,9)); b=data.frame(a=c(1,2,3), c=c(7,8,9))
  { ## Merge two data frames when names of one are a subset of the other's (or when they are equal
    if (!(identical(names(a), names(b))) & !(is.null(a) | is.null(b)))
        {
          if (!all(names(a) %in% names(b)) & !all(names(b) %in% names(a)))
            stop('Names are not nested, and this case is not yet considered')
          na <- length(names(a))
          nb <- length(names(b))
          if (na > nb) {l <- a; s <- b; wl <- 1}
          if (na < nb) {l <- b; s <- a; wl <- 2}
          missc <- which(!(names(l) %in% names(s)))
          nms <- names(l)
    
          for (i in names(l)[missc])    # i=names(l)[missc][1]
              s[[i]] <- NA

          if (wl==1)
            return(rbind(l[,sort(names(l))], s[,sort(names(s))])[,nms]) else
          return(rbind(s[,sort(names(s))], l[,sort(names(l))])[,nms])
          
        } else  return(rbind(a,b))
  }


progressbar <- function(n, length=50)  # n=237; length=50
  {
    cat(sprintf('|%s|\n', paste(rep('-',length-2), collapse='')))
    s <- 1:n
    sp <- s/n * length
    target <- 1:length
    ids <- sapply(target, function(x) which.min(abs(sp-x)))
    return(ids)
  }


upper1st <- function(x)
    {
    x1 <- strsplit(x, '')
    x <- sapply(x1, function(x) {
	x[1] <- toupper(x[1]) 
	return(paste(x, collapse=''))
	})
    return(x)
    }


## my optimise function
## target=ptarget; lims=c(0,1); incr=0.05; tol=1e-7; pbr=pbr_nlg; spx=spi
optimise_y <- function (f, target, lims, incr = 0.01, tol = 0.00001, ...) 
{
    xys <- NULL
    xi <- lims[1]
    ci <- lims
    yi <- f(xi, ...)
    slope <- sign(f(ci[2], ...) - f(ci[1], ...))
    dir = ifelse(sign(yi - target) == slope, -1, 1)
    samedir = 0
    steps = 0
    stillhunt = T
    first <- T
    #browser()
    while (stillhunt) {
        steps = steps + 1
        if (!first) {
            yi <- f(xi, ...)
        }
        else first <- F
        xys = rbind(xys, c(xi, yi, dir, incr))
        if (dir < 0 & yi < target) {
            ci[1] = xi
            stillhunt = F
        }
        if (dir > 0 & yi > target) {
            ci[2] = xi
            stillhunt = F
        }
        if (dir > 0 & yi < target) {
            ci[1] = xi
        }
        if (dir < 0 & yi > target) {
            ci[2] = xi
        }
        if (stillhunt) {
            incr = incr * (sqrt(5) + 1)/2
            xi = xi + dir * incr
        }
        else {
            incr = incr * (sqrt(5) - 1)/2
            dir = -1 * dir
            xi = xi + dir * incr
        }
    }
    while (abs(yi - target) > tol & incr > 0.000001 & incr != tol & steps < 666) {
        steps = steps + 1
        yi <- f(xi, ...)
        xys <- rbind(xys, c(xi, yi, dir, incr))
        incr <- incr * (sqrt(5) - 1)/2
        dir = -sign(yi - target)
        ci[ifelse(dir > 0, 1, 2)] <- xi
        xi = xi + dir * incr
    }
    xys = cbind(xys, xys[, 2] - target)
    colnames(xys) <- c("x", "y", "dir", "incr", "dist")
    if (xys[steps, "dist"] > tol) 
        warning("Final accuracy greater than specified tolerance")
    return(list(x = xys[steps, "x"], acc = xys[steps, "dist"], 
        step = xys))
}
