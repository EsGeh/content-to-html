# sgHomePage

## Disclaimer

Warning: This is a work in progress.

## Features

* simple static websites, with navigation menu
* appereance highly configurable via css
* html properties highly configurable for most html elements (this includes css classes).
* structured text content is entered in [YAML](yaml.org) format. No html needed!
* most functionality is realized via "Plugins". These are conceptual entities wich can be included into any secion of the website to embed specific functionality. Currently the following plugins are available:
	* "website" Plugin: Hierarchy of visual content with navigation menu
		* text paragraphs
		* image
		* audio
		* download link
		* html forms
	* "projDB" Plugin: a "database" of persons, artists, and projects. This can be used to display information about your own projects, and give recommendations of things that you like.
	* "form" Plugin: email form for user feedback

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
