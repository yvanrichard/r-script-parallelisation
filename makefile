all: all-results/all-results.rdata

#  1: specify computers and threads to use in parallel-pars.r
#  2: split job among computers with "make preparesims"
#  3: park previous results with "make park"
#  4: "make runallsims" to start all the simulations
#  5: harvest results on different computers with "make harvest"
# Check runs with "make checkruns"
# If something goes wrong: "make killr", to kill all the R processes on all computers, before starting again


## collect results from all computers, and append them
all-results/all-results.rdata: parallelisation/gather-results.r  .runallsims
	cd parallelisation; Rscript gather-results.r

## Run simulations
.runallsims: parallelisation/run-all-sims.r   .compile   .preparesims
	cd parallelisation; Rscript run-all-sims.r
	touch .runallsims

## Prepare simulations; split between cores; 
## run "make preparsesims a=test" just for testing what's about to be run
.preparesims: 
	cd parallelisation; Rscript prepare-sims.r $(a)
	touch .preparesims

## Byte compile R functions to go a bit faster
.compile: functions.r  functions-model.r  parallelisation/compile-functions.r 
	cd parallelisation; Rscript compile-functions.r
	touch .compile

## Park previous results (results, all-results)
park:
	cd parallelisation; Rscript park-prev.r





###  Various utilities

## Do a git pull on dfly computers (as in pars_parallel.r)
gitsync:
	cd parallelisation; Rscript gitsync.r

## Kill screens on all dfly computers specified in pars_parallel.r
killscreens:
	cd parallelisation; Rscript kill-all-screens.r

## Kill all R processes (of user) on all computers specified in pars_parallel.r
killr:
	cd parallelisation; Rscript kill-all-r.r

## Check log of screen on each computer and check R processes running on all computers
checkruns:
	cd parallelisation; Rscript check-screens.r
	Rscript -e "proc_in_dfly()"

## make tags for emacs
tags:
	Rscript -e "rtags(ofile='TAGS', recursive=T)"

clean:
	rm makefile0*

cleantmp: 
	rm *~

