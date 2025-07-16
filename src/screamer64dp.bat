gfortran -c -O03 -mcmodel=medium zdem.for *.f
ar crv screamer64.a *.o
rm *.o
ranlib screamer64.a
g++ -o screamer64dp screamer64.a -static-libgcc /usr/local/lib/libgfortran.a /usr/local/lib/libquadmath.a -fdefault-double-8
cp screamer64dp ../run_decks/screamer64dp
rm screamer64.a
