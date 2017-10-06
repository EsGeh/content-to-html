#!/bin/bash

# format: --plugin "pluginName:uriPrefix:configFile"
stack build && stack exec sgHomePage-exe -- \
	--css-config bootstrapDemo/attributes_cfg.yaml \
	--plugin projDB:projDB:demo/projDB.yaml \
	--plugin website:content:bootstrapDemo/website_cfg.yaml \
	$@
