gfortran -c -O03 -fopenmp -mcmodel=medium zdem.for *.f
ar crv screamer64p.a *.o
rm *.o
ranlib screamer64p.a
g++ -o screamer64p screamer64p.a -fopenmp -static-libgcc /usr/local/lib/libgfortran.a /usr/local/lib/libquadmath.a
cp screamer64p ../run_decks/screamer64p
rm screamer64p.a
