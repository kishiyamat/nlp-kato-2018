output: report.md abstract.tex
	pandoc report.md -o report.tex
	sed -i 's/、/，/g' report.tex
	sed -i 's/。/．/g' report.tex
	sed -i 's/\\emph{/\\textit{/g' report.tex
	platex abstract.tex
	platex abstract.tex
	pbibtex abstract
	platex abstract.tex
	platex abstract.tex 
	dvipdfmx abstract.dvi 
