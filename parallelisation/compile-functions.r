rm(list=ls())

library(compiler)
enableJIT(3)

## library(popbio)
## library(stats)

## setwd('~/dragonfly/sra-foundations/modelling/bh-dd-k50')

is.compile <- function(func)
{
  ## this function lets us know if a function has been byte-coded or not
  ##If you have a better idea for how to do this - please let me know...
  if(class(func) != "function") stop("You need to enter a function")
  last_2_lines <- tail(capture.output(func),2)
  any(grepl("bytecode:", last_2_lines)) # returns TRUE if it finds the text "bytecode:" in any of the last two lines of the function's print
}

## source('functions.r')
cmpfile('../functions.r', 'functions_cmp.r')
cmpfile('../functions-model.r', 'functions-model_cmp.r')

