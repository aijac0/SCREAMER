gfortran -c -O03 -mcmodel=medium zdem.for *.f
ar crv screamer64.a *.o
rm *.o
ranlib screamer64.a
g++ -o screamer64 screamer64.a -static-libgcc /usr/local/lib/libgfortran.a /usr/local/lib/libquadmath.a
cp screamer64 ../run_decks/screamer64
rm screamer64.a
