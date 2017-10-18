#!/bin/bash

# format:   --plugin "pluginName:uriPrefix:configFile"
# or:       --embeddable "embeddableName:configFile"
stack build && stack exec sgHomePage-exe -- \
	--css-config demo/config/attributes.yaml \
	--plugin website:content:demo/config/website.yaml \
	--embeddable projDB:demo/config/projDB.yaml \
	--embeddable form:demo/config/form.yaml
	$@
