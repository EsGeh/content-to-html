#!/bin/bash

# format: --plugin "pluginName:uriPrefix:configFile"
stack build && stack exec sgHomePage-exe -- \
	--plugin projDB:projDB:demo/projDB.yaml \
	--plugin website:content:demo/websiteConfig.yaml \
	$@
