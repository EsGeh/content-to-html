#!/bin/bash

# format:   --plugin "pluginName:uriPrefix:configFile"
# or:       --embeddable "embeddableName:configFile"
stack build && stack exec sgHomePage-exe -- \
	--css-config bootstrapDemo/config/attributes.yaml \
	--plugin website:content:bootstrapDemo/config/website.yaml \
	--embeddable projDB:demo/config/projDB.yaml \
	--embeddable form:demo/config/form.yaml \
	$@
