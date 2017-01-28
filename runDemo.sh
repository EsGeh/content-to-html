##

stack build && stack exec sgHomePage-exe -- \
	--data demo/content:content \
	--data demo/css:css \
	--data demo/data:data \
	--plugin projDB:projDB:demo/projDB.yaml \
	--user_css "css/style.css" \
	--content "demo/content.yaml" $@
