# Overview

My personal settings for use with [Prelude](https://github.com/bbatsov/prelude)

These settings are minimal, most of the real work is done by Prelude
and other Emacs packages.

## Configuration

```bash
$ git clone https://github.com/bbatsov/prelude .emacs.d
$ ln -s ~/prelude-personal/*.el ~/.emacs.d/personal/
```

## Go

The Emacs packages for the Go programming language are initialized via the
[prelude-go](https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el) module.

* [go-projectile](https://github.com/dougm/go-projectile) -  Go add-ons for Projectile

* [go-mode](https://github.com/dominikh/go-mode.el) - Major mode for editing Go code

* [company-mode](http://company-mode.github.io/) - Code completion

* [flycheck](http://www.flycheck.org/) - Syntax checking

* [go-eldoc](https://github.com/syohex/emacs-go-eldoc) - Type information

* [go-test](https://github.com/nlamirault/gotest.el) - Run Go tests and programs

* [go-oracle](http://golang.org/s/oracle-user-manual)

* [go-rename](https://godoc.org/golang.org/x/tools/cmd/gorename)
