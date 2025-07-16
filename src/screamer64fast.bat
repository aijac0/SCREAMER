gfortran -c -Ofast -mcmodel=medium zdem.for *.f
ar crv screamer64fast.a *.o
rm *.o
ranlib screamer64fast.a
g++ -o screamer64fast screamer64fast.a -static-libgcc /usr/local/lib/libgfortran.a /usr/local/lib/libquadmath.a
cp screamer64fast ../run_decks/screamer64fast
rm screamer64fast.a
