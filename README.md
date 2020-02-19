# homogenization code 
Homogenization of Groningen/Eelde

This directory contains the following files:

1. Readme file.

2. Input data file 'GroningenEelde_overlap.txt'. 
   Containing the standardised daily data of Groningen and Eelde (1906-1970,
   Tg, Tn, Tx) prior to homogenisation, inclusive the overlap data 
   (Groningen = 6, Eelde = 4).
   
3. Input data file 'Eelde_prehom.txt'.
   Containing the daily data of Eelde (1906-2015, Tg, Tn, Tx) prior to homogenisation 
   in the KNMI database (combination Groningen and Eelde).
   
4. Output file 'Eelde_hom_v0.txt'.
   Containing the daily non-homogenized and homogenized data of Groningen/Eelde
   (1906-2016, Tg, Tn, Tx, Tg.hom, Tn.hom, Tx.hom). 
   
5. R-code file ' hom_Groningen_Eelde.R.
   Containing the R-code use for homogenizing Groningen to Eelde. 
   
The R-code runs with standard R libraries. It is assumed that data and code 
are in the working directory. Running the code with the given input
files produces the given output file. 
