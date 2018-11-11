output: header.yml report.md	
	cat header.yml report.md | \
	sed -e 's/、/，/g' | sed -e 's/。/．/g' |\
	pandoc -V geometry:margin=1in --filter \
	pandoc-citeproc --bibliography=../share/ref.bib \
	-o output.pdf --pdf-engine=lualatex \
	--number-sections --standalone \
	-f markdown+smart


