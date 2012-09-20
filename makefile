all: all-results/all-results.rdata

#  1: park previous results with "make park"
#  2: specify computers and threads to use in parallel-pars.r
#  3: split job among computers with "make preparesims"
#  4: --- !!! COMMIT AND PUSH CHANGES !!! ---
#  5: synchronise computers with "make gitsync"
#  6: "make runallsims" to start all the simulations
#  7: harvest results on different computers with "make harvest"
# Check runs with "make checkruns"
# If something wrong: "make killr", to kill all the R processes on all computers, before starting again


## collect results from all computers, and append them
all-results/all-results.rdata: parallelisation/gather-results.r
	cd parallelisation; Rscript gather-results.r

## Run simulations
runallsims: .compile   parallelisation/run-all-sims.r 
	cd parallelisation; Rscript run-all-sims.r

## Prepare simulations; split between cores; 
## run "make preparsesims a=test" just for testing what's about to be run
preparesims: 
	cd parallelisation; Rscript prepare-sims.r $(a)

## Park previous results (results, all-results)
park:
	cd parallelisation; Rscript park-prev.r

## Do a git pull on dfly computers (as in pars_parallel.r)
gitsync:
	cd parallelisation; Rscript gitsync.r




## Kill screens on all dfly computers specified in pars_parallel.r
killscreens:
	cd parallelisation; Rscript kill-all-screens.r

## Kill all R processes (of user) on all computers specified in pars_parallel.r
killr:
	cd parallelisation; Rscript kill-all-r.r


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

.compile: functions.r  functions-model.r  parallelisation/compile-functions.r 
	cd parallelisation; Rscript compile-functions.r
	touch .compile
