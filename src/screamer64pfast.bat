gfortran -c -Ofast -fopenmp -mcmodel=medium zdem.for *.f
ar crv screamer64pfast.a *.o
rm *.o
ranlib screamer64pfast.a
g++ -o screamer64pfast screamer64pfast.a -fopenmp -static-libgcc /usr/local/lib/libgfortran.a /usr/local/lib/libquadmath.a
cp screamer64pfast ../run_decks/screamer64pfast
rm screamer64pfast.a
