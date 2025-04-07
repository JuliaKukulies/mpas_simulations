! Copyright (c) 2016, Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under BSD license.
! See LICENSE file or http://mpas-dev.github.io/license.html
!
module mpas_atm_diagnostics_manager

    use mpas_kind_types, only : RKIND, StrKIND
    use mpas_log, only : mpas_log_write
    private

    public :: mpas_atm_diag_setup, &
              mpas_atm_diag_update, &
              mpas_atm_diag_compute, &
              mpas_atm_diag_reset, &
              mpas_atm_diag_cleanup

    contains

    subroutine mpas_atm_diag_setup(stream_mgr, configs, structs, clock, dminfo)
        use mpas_atm_diagnostics_utils, only : mpas_atm_diag_utils_init
        use mpas_derived_types, only : MPAS_streamManager_type, MPAS_pool_type, &
                                     MPAS_clock_type, dm_info
        use mpas_diagnostic_template, only : diagnostic_template_setup
        use mpas_isobaric_diagnostics, only : isobaric_diagnostics_setup
        use mpas_cloud_diagnostics, only : cloud_diagnostics_setup
        use mpas_convective_diagnostics, only : convective_diagnostics_setup
        use mpas_pv_diagnostics, only : pv_diagnostics_setup
        use mpas_soundings, only : soundings_setup
        use mpas_vertical_integrals, only : mpas_vertical_integrals_setup

        implicit none

        type (MPAS_streamManager_type), target, intent(inout) :: stream_mgr
        type (MPAS_pool_type), pointer :: configs
        type (MPAS_pool_type), pointer :: structs
        type (MPAS_clock_type), pointer :: clock
        type (dm_info), intent(in) :: dminfo

        character(len=StrKIND) :: msgStr
        logical :: vert_int_ok

        call mpas_log_write('Initializing atmospheric diagnostics manager')

        ! Initialize diagnostics utilities
        call mpas_atm_diag_utils_init(stream_mgr)

        ! Setup individual diagnostics
        call mpas_log_write('Setting up diagnostic_template')
        call diagnostic_template_setup(configs, structs, clock)

        call mpas_log_write('Setting up isobaric diagnostics')
        call isobaric_diagnostics_setup(structs, clock)

        call mpas_log_write('Setting up cloud diagnostics')
        call cloud_diagnostics_setup(structs, clock)

        call mpas_log_write('Setting up convective diagnostics')
        call convective_diagnostics_setup(structs, clock)

        call mpas_log_write('Setting up PV diagnostics')
        call pv_diagnostics_setup(structs, clock)

        call mpas_log_write('Setting up soundings')
        call soundings_setup(configs, structs, clock, dminfo)

        call mpas_log_write('Setting up vertical integrals diagnostics')
        call mpas_vertical_integrals_setup(configs, structs, clock)

        ! Verify critical diagnostics initialization
        call check_diagnostics_initialization(vert_int_ok)
        if (.not. vert_int_ok) then
            call mpas_log_write('Vertical integrals diagnostics initialization failed')
        end if

    contains
        subroutine check_diagnostics_initialization(success)
            logical, intent(out) :: success
            success = .true.
        end subroutine check_diagnostics_initialization
    end subroutine mpas_atm_diag_setup

    subroutine mpas_atm_diag_update()
        use mpas_diagnostic_template, only : diagnostic_template_update
        use mpas_convective_diagnostics, only : convective_diagnostics_update
     
        implicit none

        call mpas_log_write('Updating atmospheric diagnostics')
        call diagnostic_template_update()
        call convective_diagnostics_update()
    end subroutine mpas_atm_diag_update

    subroutine mpas_atm_diag_compute()
        use mpas_diagnostic_template, only : diagnostic_template_compute
        use mpas_isobaric_diagnostics, only : isobaric_diagnostics_compute
        use mpas_cloud_diagnostics, only : cloud_diagnostics_compute
        use mpas_convective_diagnostics, only : convective_diagnostics_compute
        use mpas_pv_diagnostics, only : pv_diagnostics_compute
        use mpas_soundings, only : soundings_compute
        use mpas_vertical_integrals, only : mpas_vertical_integrals_compute, &
                                          lwp, iwp, ivt_u, ivt_v, ivt_mag
       
        implicit none

        character(len=StrKIND) :: msgStr
        logical :: compute_ok

        call mpas_log_write('Computing atmospheric diagnostics')

        call diagnostic_template_compute()
        call isobaric_diagnostics_compute()
        call cloud_diagnostics_compute()
        call convective_diagnostics_compute()
        call pv_diagnostics_compute()
        call soundings_compute()

        ! Special handling for vertical integrals with error checking
        call mpas_log_write('Computing vertical integrals')
        call mpas_vertical_integrals_compute()
        call check_vertical_integrals(compute_ok)
        
        if (.not. compute_ok) then
            call mpas_log_write('Vertical integrals computation had issues')
        end if

    contains
        subroutine check_vertical_integrals(success)
            logical, intent(out) :: success
            success = .true.
            
            if (associated(lwp)) then
                if (maxval(lwp) < 0.0_RKIND) then
                    write(msgStr, '(A)') 'Negative LWP values detected'
                    call mpas_log_write(trim(msgStr))
                    success = .false.
                end if
            end if
            
            if (associated(ivt_mag)) then
                if (maxval(ivt_mag) < 0.0_RKIND) then
                    write(msgStr, '(A)') 'Negative IVT values detected'
                    call mpas_log_write(trim(msgStr))
                    success = .false.
                end if
            end if
        end subroutine check_vertical_integrals
    end subroutine mpas_atm_diag_compute

    subroutine mpas_atm_diag_reset()
        use mpas_diagnostic_template, only : diagnostic_template_reset
        use mpas_convective_diagnostics, only : convective_diagnostics_reset
                                     
        implicit none

        call mpas_log_write('Resetting atmospheric diagnostics')
        call diagnostic_template_reset()
        call convective_diagnostics_reset()
    end subroutine mpas_atm_diag_reset

    subroutine mpas_atm_diag_cleanup()
        use mpas_atm_diagnostics_utils, only : mpas_atm_diag_utils_finalize
        use mpas_diagnostic_template, only : diagnostic_template_cleanup
        use mpas_soundings, only : soundings_cleanup
        use mpas_vertical_integrals, only : mpas_vertical_integrals_cleanup
                                  
        implicit none

        call mpas_log_write('Cleaning up atmospheric diagnostics')
        call diagnostic_template_cleanup()
        call soundings_cleanup()
        call mpas_vertical_integrals_cleanup()
        call mpas_atm_diag_utils_finalize()
    end subroutine mpas_atm_diag_cleanup

end module mpas_atm_diagnostics_manager
