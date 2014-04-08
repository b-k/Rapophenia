Rapophenia is an R wrapper for [Apophenia](http://apophenia.info), a library of C functions for statistical and scientific computing.

See the ```tests``` directory for an example, which uses a C-side simulated annealing routine to optimize versions of [Rosenbrock's Banana function](ttp://en.wikipedia.org/wiki/Rosenbrock%27s_banana_function) written in C and R, with and without constraints.

It focuses on the two main data structures: 

* ```apop_data```, which is analogous to an R data frame. See ```data_frame_from_apop_data``` and ```apop_data_from_frame```.

* ```apop_model```, which represents elements from a large class of statistical models, and has no real analogue in R. Its intent is to make models truly swappable: you should be able to switch from OLS model to Logit model to agent-based simulation model by just changing the name of the model used. The Rapophenia interface focuses on providing wrappers for the model methods (estimate, draw, et cetera). You can write an ```apop_model``` that has R functions for these methods, and have C code use that model.

## Installation

R's package management system has no real mechanism for installing C libraries, so you have to install Apophenia yourself before installing this package. See [Apophenia's setup page](http://apophenia.info/setup.html) for details.

Once that is installed, we recommend installing from this repository via ```devtools```:

```
library(devtools)
install_github("b-k/Rapophenia", ref="pkg")
```

## Status

To tell you the truth, we primarily wrote this as a stepping-stone to writing [Tea](https://github.com/rodri363/tea), a system for survey processing. It works well in that regard, but once it served that purpose, we didn't extend it much further. You are invited to improve and extend this package.

Development of this package predated a lot of development on ```Rcpp```, which means we spent a lot of time  in the undocumented weeds of R's C interface. Porting parts to ```Rcpp``` may or may not help when extending the package.
