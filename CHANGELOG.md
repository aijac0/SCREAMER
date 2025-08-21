# Changelog

## Made arrays dynamically allocated
- Arrays in mod/zdemloop.f90 and mod/zdemwork.f90 declared ALLOCATABLE
- Subroutines in zdemalloc.f90 allocate arrays prior to usage
- Calls made in zdem.for to allocate arrays in two phases: before and after reading input

## Replaced header files with modules
- Header files converted to modules in src/mod/
- COMMON blocks removed with memory shared via module imports
- Dependencies between modules identified in dep/ to assist build process

### Added new build process
- Makefile to build project varieties (see USAGE.md)
