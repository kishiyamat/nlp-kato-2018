# cd abstract
# pandoc content.md -o content.tex
# pandoc references.md -o references.tex
sed -i 's/includegraphics/includegraphics[width=8.5cm]/g' content.tex
cd ../
cd main
platex abstract.tex
platex abstract.tex
pbibtex abstract
platex abstract.tex
platex abstract.tex
# http://blog.livedoor.jp/tmako123-programming/archives/34963127.html?1512833674#comment-form
# このオプションを渡さないと詰む
dvipdfmx -p letter abstract.dvi
cd ..
