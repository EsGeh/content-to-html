# content2html - simple website based on structured text files

## Disclaimer

Warning: This is a work in progress.

## Features

* simple static websites, with navigation menu
* appereance highly configurable via css
* html properties highly configurable for most html elements (this includes css classes).
* structured text content is entered in [YAML](yaml.org) format. No html needed!
* all content is being rendered by the "website" plugin (propably this is the only plugin ever needed). When configured correctly via yaml files it will render a nice html view: a hierarchy of visual content with navigation menu. Currently the following types of content are supported:
	* text paragraphs
	* image
	* audio
	* download link
	* html forms

* functionality can easily be extended via "embeddables". These are conceptual entities wich can be included into any secion of the website to embed specific functionality. Currently the following embeddables are available:
	* "projDB": a "database" of persons, artists, and projects. This can be used to display information about your own projects, and give recommendations of things that you like.
	* "form": email form for user feedback

## Dependencies

* git
* stack
* Docker (only needed for static compilation)

## compiling and running

	$ stack setup
	$ stack build

to run, see examples in the ./examples dir.
How to run them:

	$ cd examples
	$ ./demo/run.sh

## configuration

see ./examples directory

## static compilation (experimental)

This is helpful in scenarios where compiling on the destination platform is not an option, e.g. installation on a shared hoster lacking haskell toolchain.
static compilation requires docker to be installed

	$ ./production_build/build.sh

## Credits

[haskell web framework](https://hackage.haskell.org/package/Spock)

* static compilation is based on:

	* [](https://github.com/dkubb/haskell-builder)
	* [](https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack)

* [docker container used](https://github.com/mitchty/alpine-ghc)
