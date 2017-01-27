


# tracer

> Slick Call Stacks

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Linux Build Status](https://travis-ci.org/MangoTheCat/tracer.svg?branch=master)](https://travis-ci.org/MangoTheCat/tracer)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/mangothecat/tracer?svg=true)](https://ci.appveyor.com/project/gaborcsardi/tracer)
[![](http://www.r-pkg.org/badges/version/tracer)](http://www.r-pkg.org/pkg/tracer)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/tracer)](http://www.r-pkg.org/pkg/tracer)
[![Coverage Status](https://img.shields.io/codecov/c/github/MangoTheCat/tracer/master.svg)](https://codecov.io/github/MangoTheCat/tracer?branch=master)

Better looking call stacks after an error.

## Installation


```r
source("https://install-github.me/mangothecat/tracer")
```

## Usage


```r
library(tracer)
```

After an error, call `tb()` instead of `traceback()`, to get a nice
error stack summary:

![](/inst/screenshot1.png)

To browse the code at the locations of the calls, supply the number of
the call to `tb()`:

![](/inst/screenshot2.png)

Note that `tracer` can often show the (deparsed) source code of the
call location, even if the original R source code is no more available,
like in the case of installed packages.

## Caveat

`tracer` sets the error handler via `options(error = ...)`. If the user
changes the error handler manually, then `tracer` will not work.

## License

MIT © Mango Solutions
