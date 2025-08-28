# SCREAMER Build and Run Instructions

1. Prerequisites
• GFortran (version 10+ recommended)
• GNU Make

Verify installation:
 gfortran --version

2. Building SCREAMER

2.1 Build All Variants
To compile all SCREAMER executables (optimized, debug, etc.):
 make all

This will produce the following binaries:
- screamer64 – Optimized build (-O3)
- screamer64_debug – Debug build (-O0 -fcheck=all -fbacktrace)
- screamer64_warning – Build with warnings (-O2 -Wall)
- screamer64dp – Double-precision build
- screamer64dyn – Dynamic linking build
- screamer64fast – Aggressive optimization (-Ofast)

2.2 Build a Single Variant
For example, to build just the debug version:
 make screamer64_debug

3. Running SCREAMER
SCREAMER executables read an input file (deck) and produce output files.

3.1 Run an Input Deck:
 ./screamer64 run_decks/example.txt

4. Notes
* Input decks in run_decks/ must be correctly formatted for SCREAMER.
