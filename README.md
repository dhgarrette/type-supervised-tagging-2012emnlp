[Dan Garrette]: http://cs.utexas.edu/~dhg
[Jason Baldridge]: http://www.jasonbaldridge.com


Type-supervised tagging: EMNLP 2012
===================================

This repository contains the code, scripts, and instructions needed to reproduce the results in the paper

  > [Type-Supervised Hidden Markov Models for POS Tagging with Incomplete Tag Dictionaries](http://www.cs.utexas.edu/users/dhg/papers/garrette_baldridge_emnlp2012.pdf)  
  > [Dan Garrette] and [Jason Baldridge]  
  > In Proceedings of EMNLP 2012

This code is frozen as of the version used to obtain the results in the paper. It will not be maintained. 

To see the most up-to-date version of the HMM code, visit the [scalabha](https://github.com/utcompling/Scalabha) repository.

Running the experiments
-------

**Set up English data**

The English experiments rely on Penn Treebank data. This script prepares that data for use by the experiments.  The treebank directory referenced when running the script should contain a folder `combined` containing files `wsj_0000.mrg` through `wsj_2454.mrg`.

    sh run.sh "en-data /path/to/treebank"
    
**Run English experiments on sections 00-15**

    sh run.sh en-run16

**Run English experiments on sections 00-07**

    sh run.sh en-run8

**Run Italian experiments**

The Italian data is already located in the `data` directory, so this 
experiment can be launched immediately without need for data setup. 

    sh run.sh it-run
    

Questions
------
    
If you have any questions, please contact Dan Garrette (dhg@cs.utexas.edu).
