.SUFFIXES: .F .o

#
# Add new diagnostic modules to DIAGNOSTIC_MODULES
#
DIAGNOSTIC_MODULES = \
	mpas_atm_diagnostic_template.o \
	mpas_isobaric_diagnostics.o \
	mpas_cloud_diagnostics.o \
	mpas_convective_diagnostics.o \
	mpas_pv_diagnostics.o \
	mpas_soundings.o \
	mpas_vertical_integrals.o

mpas_isobaric_diagnostics.o: mpas_atm_diagnostics_utils.o

mpas_cloud_diagnostics.o: mpas_atm_diagnostics_utils.o

mpas_convective_diagnostics.o: mpas_atm_diagnostics_utils.o

mpas_pv_diagnostics.o: mpas_atm_diagnostics_utils.o

mpas_soundings.o:

mpas_vertical_integrals.o: mpas_atm_diagnostics_utils.o

################### Generally no need to modify below here ###################


OBJS = mpas_atm_diagnostics_manager.o mpas_atm_diagnostics_utils.o

all: $(DIAGNOSTIC_MODULES) $(OBJS)

mpas_atm_diagnostics_manager.o: mpas_atm_diagnostics_utils.o $(DIAGNOSTIC_MODULES)


clean:
	$(RM) *.o *.mod *.f90
	@# Some Intel compilers generate *.i files; clean them up, too
	$(RM) *.i

.F.o:
	$(RM) $@ $*.mod
ifeq "$(GEN_F90)" "true"
	$(CPP) $(CPPFLAGS) $(PHYSICS) $(CPPINCLUDES) $< > $*.f90
	$(FC) $(FFLAGS) -c $*.f90 $(FCINCLUDES) -I../../framework -I../../operators -I../dynamics -I../physics -I../physics/physics_wrf -I../../external/esmf_time_f90
else
	$(FC) $(CPPFLAGS) $(PHYSICS) $(FFLAGS) -c $*.F $(CPPINCLUDES) $(FCINCLUDES) -I../../framework -I../../operators -I../dynamics -I../physics -I../physics/physics_wrf -I../../external/esmf_time_f90
endif
