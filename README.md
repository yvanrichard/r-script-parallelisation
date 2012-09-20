r-script-parallelisation
========================

Template for splitting a job in an R script across multiple threads and computers


Context
=======

One often wants to run a heavy R job across multiple threads and computers, like for doing simulations. Each simulation may require a set of parameters, and each simulation is carried out independently of each other. This template facilitates the splitting of the job, the execution of the job, and the harvest of results into a single file.


Process
=======

The data is in a single file, data.rdata, containing the rows id, and some parameters for each subjob. The threads and computers are specified in pars_parallel.r.  


Organisation
============

data.rdata: initial data providing parameters for running each job
run1.r: script to run action on slice. Requires to take as arguments a sequence of row ids (e.g. Rscript run1.r 1-10,21-30
makefile: organises scripts and actions


Requirements
============

- R (>2.14)
- GNU make
- GIT
- screen
- ssh


