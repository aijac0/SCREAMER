gfortran zdem.for *.f -fopenmp -O03 -mcmodel=medium -o screamer64p
cp screamer64p ../run_decks/screamer64p
