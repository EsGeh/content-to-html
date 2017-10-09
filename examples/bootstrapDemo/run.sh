#!/bin/bash

# format: --plugin "pluginName:uriPrefix:configFile"
stack build && stack exec sgHomePage-exe -- \
	--css-config bootstrapDemo/config/attributes.yaml \
	--plugin projDB:projDB:demo/config/projDB.yaml \
	--plugin website:content:bootstrapDemo/config/website.yaml \
	--plugin form:form:demo/config/form.yaml \
	$@
