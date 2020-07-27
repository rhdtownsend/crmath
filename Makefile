# Makefile for Fortran correctly rounded math library

# Variables

include Make.inc

OBJECTS = crmath.o crmath_r_sp.o crmath_c_sp.o crmath_r_dp.o crmath_c_dp.o

# Rules

%.o : %.mod

%.o : %.f90
	@echo FC $<
	@${F9XC} ${F9XFLAGS} -c $<

%.mod : %.o
	@true

all : libcrmath.a

clean :
	rm -f ${OBJECTS} *.mod *.smod *.a

# Dependencies

libcrmath.a : ${OBJECTS}
	ar crs $@ $^

crmath_r_sp.o : crmath.mod
crmath_c_sp.o : crmath.mod
crmath_r_dp.o : crmath.mod
crmath_c_dp.o : crmath.mod

