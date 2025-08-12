# ------------------------------------------------------------
#  Screamer Makefile
# ------------------------------------------------------------
FC          = gfortran
BASE_FFLAGS = -fallow-argument-mismatch -I mod -J mod
DPFLAG      = -fdefault-double-8

# directories
SRCDIR      = src
MODSRCDIR   = src/mod
OBJDIR      = obj
MODDIR      = mod
DEPDIR      = dep

# sources
SRC_F    := $(wildcard $(SRCDIR)/*.f)
SRC_FOR  := $(wildcard $(SRCDIR)/*.for)
SRC_F90  := $(wildcard $(SRCDIR)/*.f90)

MOD_F    := $(wildcard $(MODSRCDIR)/*.f)
MOD_FOR  := $(wildcard $(MODSRCDIR)/*.for)
MOD_F90  := $(wildcard $(MODSRCDIR)/*.f90)

# objects
OBJ      := \
  $(patsubst $(SRCDIR)/%.f,   $(OBJDIR)/%.o, $(SRC_F))   \
  $(patsubst $(SRCDIR)/%.for, $(OBJDIR)/%.o, $(SRC_FOR)) \
  $(patsubst $(SRCDIR)/%.f90, $(OBJDIR)/%.o, $(SRC_F90))

MOD_OBJ  := \
  $(patsubst $(MODSRCDIR)/%.f,   $(OBJDIR)/%.o, $(MOD_F))   \
  $(patsubst $(MODSRCDIR)/%.for, $(OBJDIR)/%.o, $(MOD_FOR)) \
  $(patsubst $(MODSRCDIR)/%.f90, $(OBJDIR)/%.o, $(MOD_F90))

ALL_OBJ  := $(MOD_OBJ) $(OBJ)

# dependencies
DEPS := $(wildcard $(DEPDIR)/*.d)
-include $(DEPS)

# ensure dirs
$(OBJDIR) $(MODDIR):
	mkdir -p $@

# non-module objects get compiled before module objects
$(OBJ): $(MOD_OBJ)

# compile module sources
$(OBJDIR)/%.o: $(MODSRCDIR)/%.f   | $(OBJDIR) $(MODDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@
$(OBJDIR)/%.o: $(MODSRCDIR)/%.for | $(OBJDIR) $(MODDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@
$(OBJDIR)/%.o: $(MODSRCDIR)/%.f90 | $(OBJDIR) $(MODDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@

# compile regular sources
$(OBJDIR)/%.o: $(SRCDIR)/%.f   | $(OBJDIR) $(MODDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@
$(OBJDIR)/%.o: $(SRCDIR)/%.for | $(OBJDIR) $(MODDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@
$(OBJDIR)/%.o: $(SRCDIR)/%.f90 | $(OBJDIR) $(MODDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@

# helper macro
define MAKE_BIN
$1: EXTRA   = $2
$1: LDEXTRA = $3
$1: $(ALL_OBJ)
	$(FC) $(ALL_OBJ) $(EXTRA) $(LDEXTRA) -o $$@
endef

# build variants (unchanged names)
$(eval $(call MAKE_BIN,screamer64,         -O3,))
$(eval $(call MAKE_BIN,screamer64_debug,   -O0 -fcheck=all -fbacktrace,))
$(eval $(call MAKE_BIN,screamer64_warning, -O2 -Wall,))
$(eval $(call MAKE_BIN,screamer64dp,       -O3 $(DPFLAG),))
$(eval $(call MAKE_BIN,screamer64dyn,      -O3,))          # dynamic link
$(eval $(call MAKE_BIN,screamer64fast,     -Ofast,))

TARGETS := screamer64 screamer64_debug screamer64_warning \
           screamer64dp screamer64dyn screamer64fast

all: $(TARGETS)

.PHONY: clean print-debug all $(TARGETS)

clean:
	-@rm -rf $(MODDIR) $(OBJDIR)

print-debug:
	@echo "MOD_F    = $(MOD_F)"
	@echo "SRC_F    = $(SRC_F)"
	@echo "OBJ      = $(OBJ)"
	@echo "MOD_OBJ  = $(MOD_OBJ)"