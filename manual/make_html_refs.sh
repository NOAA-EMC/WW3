#!/bin/bash

# Simple script to create an html version of references in the WW3 manual
# J.H. Alves, Feb 2019

 sed ':a;$!N;/bibitem/s/\n //;ta;P;D' manual.bbl > temp1
 sed  's/^\\bibitem.*/\\bibitem\[-\]\{-\}/g' temp1 | sed 's/textit//g' > temp2
 awk -f bbl2html.awk labelwidth=20% bigtable=1 temp2 > temp3
 awk '{for(x=1;x<=NF;x++)if($x~/\[-\]/){sub(/\[-\]/,++i)}}1' temp3 > man_bib.html
 rm -f temp1 temp2 temp3

