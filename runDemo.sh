#!/bin/bash

# format: --plugin "pluginName:uriPrefix:configFile"
stack build && stack exec sgHomePage-exe -- \
	--css-config demo/config/attributes.yaml \
	--plugin projDB:projDB:demo/config/projDB.yaml \
	--plugin website:content:demo/config/website.yaml \
	$@
