# sigfig 0.0.0.9000

* First release. `precision()` creates numbers with known precision, and arithmetic uses the coarsest known precision of its inputs while keeping the full value.
* The precision is stored with run length encoding using ALTREP, so repeated precision uses little memory.
