# nock.rkt

A [nock](https://urbit.org/docs/nock/definition) interpreter written in [racket](https://docs.racket-lang.org/reference/index.html).


I don't really know racket, but I use it for a PL course. I'm sure theres a way to package / test this interpreter properly. Maybe later.

<hr>

The nock interpreter and naive tests are in ```nock.rkt```

use: Install [DrRacket](https://download.racket-lang.org/)
     Open ```nock.rkt```
     Play around with it in the REPL window

<hr>


This repo also includes ```rkt_noun.py``` which is just a script for converting regular noun syntax into the syntax needed for use in ```nock.rkt```


use:  ```$python rkt_noun.py "[[11 22] [33 44] 55]"```
      ```(cell (cell 11 22) (cell (cell 33 44) 55))```

<hr>

