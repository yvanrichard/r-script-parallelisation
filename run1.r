rm(list=ls())

library(compiler)

source('pars_parallel.r')

loadcmp('parallelisation/functions_cmp.r')
loadcmp('parallelisation/functions-model_cmp.r')

load(datafile)
data <- eval(parse(text=dataname))
if (dataname != 'data') {rm(dataname); gc()}

dir.create(outputfold, showWarnings=F)

args <- commandArgs(trailingOnly=T)
if (!length(args)) args <- '3-10,100'
rows <- makeseq(args)

if (any(rows>nrow(data)))
  stop('Some row numbers exceed maximum number of rows')
if (any(is.na(rows)))
  stop('Some row values are invalid')

## keep only rows specified as argument
data <- data[as.numeric(data[[rowidcol]]) %in% rows,]

## where the results will be stored
data$res <- NA

###  MAIN
allsteps = NULL
startt <- Sys.time()
row=1
for (row in 1:nrow(data)) # row=1
    {
      rw <- data[[rowidcol]][row]
      cat(sprintf('row %s - %i/%i - ', rw, row, nrow(data)))
      ps <- data[row,]

      ## Do something
      data$res[row] <- ps$v1 + ps$v2
      
      if (row %% saveevery == 0)
	{
          tdiff <- (nrow(data)-row) * difftime(Sys.time(), startt, units='hours') / row
          cat(' - saving... ', format(Sys.time(), '%d/%m %H:%M'), '-',
              sprintf('%0.1f %s remaining',as.numeric(tdiff), attr(tdiff,'units')))
          data1 = data[1:row,]
          save(data1, file=sprintf('%s/%s-incomplete_data-%s.rdata', outputfold, outputbasename, args))
	}
      cat('\n')
    }
cat('Final save...\n')
save(data, file=sprintf('%s/%s-finished_data-%s.rdata', outputfold, outputbasename, args))


