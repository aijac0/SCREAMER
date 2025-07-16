# ------------------------------------------------------------
#  Screamer Makefile
# ------------------------------------------------------------
FC          = gfortran
BASE_FFLAGS = -fallow-argument-mismatch -I mod -J mod
DPFLAG      = -fdefault-double-8
SRCDIR      = src
OBJDIR      = obj
MODDIR      = mod
SRC_F   := $(wildcard $(SRCDIR)/*.f)
SRC_FOR := $(wildcard $(SRCDIR)/*.for)
SRC_F90 := $(wildcard $(SRCDIR)/*.f90)
OBJ := \
  $(patsubst $(SRCDIR)/%.f,   $(OBJDIR)/%.o, $(SRC_F))   \
  $(patsubst $(SRCDIR)/%.for, $(OBJDIR)/%.o, $(SRC_FOR)) \
  $(patsubst $(SRCDIR)/%.f90, $(OBJDIR)/%.o, $(SRC_F90))
DEP := $(OBJ:.o=.d)

# ensure object directory exists	
$(OBJDIR):
	mkdir -p $@

# ensure module directory exists	
$(MODDIR):
	mkdir -p $@

# compile rules
$(OBJDIR)/%.o: $(SRCDIR)/%.f | $(OBJDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@

$(OBJDIR)/%.o: $(SRCDIR)/%.for | $(OBJDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@

$(OBJDIR)/%.o: $(SRCDIR)/%.f90 | $(OBJDIR)
	$(FC) $(BASE_FFLAGS) $(EXTRA) -c $< -o $@

# helper macro
define MAKE_BIN
$1: EXTRA   = $2             # extra compile flags (none → uses BASE_FFLAGS)
$1: LDEXTRA = $3             # extra link flags (e.g. $(OMPFLAG))
$1: $(OBJ)
	$(FC) $(OBJ) $(EXTRA) $(LDEXTRA) -o $1
endef

# build variants
$(eval $(call MAKE_BIN,screamer64,         -O3,))
$(eval $(call MAKE_BIN,screamer64_debug,   -O2 -fcheck=all,))
$(eval $(call MAKE_BIN,screamer64_warning, -O2 -Wall,))
$(eval $(call MAKE_BIN,screamer64dp,       -O3 $(DPFLAG),))
$(eval $(call MAKE_BIN,screamer64dyn,      -O3,))                    # dynamic link
$(eval $(call MAKE_BIN,screamer64fast,     -Ofast,))

# convenience targets
TARGETS := screamer64 screamer64_debug screamer64_warning \
           screamer64dp screamer64dyn screamer64fast 

.PHONY: deps
deps: $(DEP)

all: $(MOD_OBJ) $(TARGETS)

clean:
	-@rm -f $(OBJ) $(TARGETS)

print-debug:
	@echo "SRC_F   = $(SRC_F)"
	@echo "SRC_FOR = $(SRC_FOR)"
	@echo "SRC_F90 = $(SRC_F90)"
	@echo "OBJ     = $(OBJ)"

.PHONY: all clean $(TARGETS)
