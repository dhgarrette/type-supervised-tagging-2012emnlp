[Dan Garrette]: http://cs.utexas.edu/~dhg
[Jason Baldridge]: http://www.jasonbaldridge.com


Type-supervised tagging: EMNLP 2012
===================================

This repository contains the code, scripts, and instructions needed to reproduce the results in the paper

  > Type-supervised Hidden Markov Models for POS Tagging with Incomplete Tag Dictionaries  
  > [Dan Garrette] and [Jason Baldridge]  
  > In Proceedings of EMNLP 2012

This code is outdated and will not be maintained.
To see the most up-to-date version of the HMM code, visit the [scalabha](https://github.com/utcompling/Scalabha) repository.

Running the experiments
-------

**Set up English data**

    sh run.sh "en-data /path/to/treebank"
    
That treebank directory should contain a folder `combined` containing files 
`wsj_0000.mrg` through `wsj_2454.mrg`

**Run English experiments on sections 00-15**

    sh run.sh en-run16

**Run English experiments on sections 00-07**

    sh run.sh en-run8

**Run Italian experiments**

The Italian data is already located in the `data` directory, so this 
experiment can be launched immediately. 

    sh run.sh it-run
    
If you find any problems with these instructions, please contact Dan Garrette (dhg@cs.utexas.edu).
