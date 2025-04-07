module mpas_vertical_integrals

  use mpas_kind_types, only : RKIND, StrKIND
  use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
  use mpas_pool_routines, only : mpas_pool_get_subpool, &
                               mpas_pool_get_array, &
                               mpas_pool_get_dimension, &
                               mpas_pool_get_config
  use mpas_atm_diagnostics_utils, only : mpas_field_will_be_written
  use mpas_log, only : mpas_log_write

  implicit none

  ! Pool pointers
  type (MPAS_pool_type), pointer :: mesh => null()
  type (MPAS_pool_type), pointer :: state => null()
  
  ! Scalar array and indices
  real (kind=RKIND), dimension(:,:,:), pointer :: scalars => null()
  integer, pointer :: index_qv => null()
  integer, pointer :: index_qc => null()
  integer, pointer :: index_qr => null()
  integer, pointer :: index_qi => null()
  integer, pointer :: index_qs => null()
  integer, pointer :: index_qg => null()
  integer, pointer :: index_rho_zz => null()
  integer, pointer :: index_uReconstructZonal => null()
  integer, pointer :: index_uReconstructMeridional => null()
  integer, pointer :: num_scalars => null()
  integer, parameter :: time_lev = 1
  
  ! Grid variables
  real (kind=RKIND), dimension(:,:), pointer :: rdzw => null()
  
  ! Output diagnostics
  real (kind=RKIND), dimension(:), pointer :: lwp => null()
  real (kind=RKIND), dimension(:), pointer :: iwp => null()
  real (kind=RKIND), dimension(:), pointer :: ivt_u => null()
  real (kind=RKIND), dimension(:), pointer :: ivt_v => null()
  real (kind=RKIND), dimension(:), pointer :: ivt_mag => null()

  ! Error tracking
  logical :: hydrometeors_available = .false.
  logical :: ivt_available = .false.
  character (len=StrKIND) :: missing_vars = ''

  public :: mpas_vertical_integrals_setup, &
            mpas_vertical_integrals_compute, &
            mpas_vertical_integrals_cleanup

contains

  subroutine mpas_vertical_integrals_setup(configs, structs, clock)
    type (MPAS_pool_type), pointer :: configs
    type (MPAS_pool_type), pointer :: structs
    type (MPAS_clock_type), pointer :: clock
    integer :: ierr
    logical :: all_vars_found

    ! Get required pools
    call mpas_pool_get_subpool(structs, 'mesh', mesh)
    call mpas_pool_get_subpool(structs, 'state', state)
    call mpas_pool_get_subpool(structs, 'diag_physics', state)

    ! Initialize error tracking
    hydrometeors_available = .true.
    ivt_available = .true.
    missing_vars = ''
    all_vars_found = .true.
    
    ! Get all scalar indices
    ierr = 0
    call mpas_pool_get_dimension(state, 'index_qv', index_qv)
    if (.not. associated(index_qv) .or. index_qv <= 0) then
        call append_missing_var('index_qv', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_qc', index_qc)
    if (.not. associated(index_qc) .or. index_qc <= 0) then
        call append_missing_var('index_qc', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_qr', index_qr)
    if (.not. associated(index_qr) .or. index_qr <= 0) then
        call append_missing_var('index_qr', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_qi', index_qi)
    if (.not. associated(index_qi) .or. index_qi <= 0) then
        call append_missing_var('index_qi', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_qs', index_qs)
    if (.not. associated(index_qs) .or. index_qs <= 0) then
        call append_missing_var('index_qs', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_qg', index_qg)
    if (.not. associated(index_qg) .or. index_qg <= 0) then
        call append_missing_var('index_qg', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_rho_zz', index_rho_zz)
    if (.not. associated(index_rho_zz) .or. index_rho_zz <= 0) then
        call append_missing_var('index_rho_zz', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_uReconstructZonal', index_uReconstructZonal)
    if (.not. associated(index_uReconstructZonal) .or. index_uReconstructZonal <= 0) then
        call append_missing_var('index_uReconstructZonal', all_vars_found)
        ierr = 1
    end if
    
    call mpas_pool_get_dimension(state, 'index_uReconstructMeridional', index_uReconstructMeridional)
    if (.not. associated(index_uReconstructMeridional) .or. index_uReconstructMeridional <= 0) then
        call append_missing_var('index_uReconstructMeridional', all_vars_found)
        ierr = 1
    end if
    
    ! Get number of scalars
    call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
    if (.not. associated(num_scalars) .or. num_scalars <= 0) then
        call append_missing_var('num_scalars', all_vars_found)
        ierr = 1
    end if
    
    ! Get the scalars array
    call mpas_pool_get_array(state, 'scalars', scalars, time_lev)
    if (.not. associated(scalars)) then
        call append_missing_var('scalars', all_vars_found)
        ierr = 1
    end if
    
    ! Get grid variables that aren't in scalars array
    call mpas_pool_get_array(mesh, 'rdzw', rdzw)
    if (.not. associated(rdzw)) then
        call append_missing_var('rdzw', all_vars_found)
        ierr = 1
    end if
    
    ! Set availability flags
    hydrometeors_available = all_vars_found
    ivt_available = associated(scalars) .and. all_vars_found
    
    ! Initialize output diagnostics
    call mpas_pool_get_array(structs, 'lwp', lwp)
    call mpas_pool_get_array(structs, 'iwp', iwp)
    call mpas_pool_get_array(structs, 'ivt_u', ivt_u)
    call mpas_pool_get_array(structs, 'ivt_v', ivt_v)
    call mpas_pool_get_array(structs, 'ivt_mag', ivt_mag)

    ! Log any missing variables
    if (.not. all_vars_found) then
        call mpas_log_write('Some required variables are missing: '//trim(missing_vars))
    end if

  contains
    subroutine append_missing_var(var_name, all_ok)
      character (len=*), intent(in) :: var_name
      logical, intent(inout) :: all_ok
      if (len_trim(missing_vars) > 0) then
          missing_vars = trim(missing_vars)//', '//trim(var_name)
      else
          missing_vars = trim(var_name)
      end if
      all_ok = .false.
    end subroutine append_missing_var
  end subroutine mpas_vertical_integrals_setup


  subroutine mpas_vertical_integrals_compute()
    integer :: iCell, k, nCells, nVertLevels
    integer, pointer :: p_nCells, p_nVertLevels
    real (kind=RKIND) :: lwp_sum, iwp_sum, ivt_u_sum, ivt_v_sum, dp
    real(kind=RKIND) :: max_qc, max_qi, max_qv, max_rho_zz

    ! Get dimensions
    call mpas_pool_get_dimension(mesh, 'nCells', p_nCells)
    call mpas_pool_get_dimension(mesh, 'nVertLevels', p_nVertLevels)
    nCells = p_nCells
    nVertLevels = p_nVertLevels

    ! Get max values of input fields for diagnostics
    max_qc = maxval(scalars(index_qc,:,:))
    max_qi = maxval(scalars(index_qi,:,:))
    max_qv = maxval(scalars(index_qv,:,:))
    max_rho_zz = maxval(scalars(index_rho_zz,:,:))
  
    ! Print diagnostics
    call mpas_log_write('Input maxima:')
    call mpas_log_write('max(qc)=$r, max(qi)=$r, max(qv)=$r', &
                       realArgs=(/max_qc, max_qi, max_qv/))
    call mpas_log_write('max(rho_zz)=$r', realArgs=(/max_rho_zz/))


    if (.not. hydrometeors_available) return

      
    do iCell = 1, nCells
        ! LWP/IWP calculation
        lwp_sum = 0.0_RKIND
        iwp_sum = 0.0_RKIND
        do k = 1, nVertLevels
            lwp_sum = lwp_sum + scalars(index_rho_zz,k,iCell) * &
                     (scalars(index_qc,k,iCell) + scalars(index_qr,k,iCell)) / rdzw(k,iCell)
            iwp_sum = iwp_sum + scalars(index_rho_zz,k,iCell) * &
                     (scalars(index_qi,k,iCell) + scalars(index_qs,k,iCell) + scalars(index_qg,k,iCell)) / rdzw(k,iCell)
        end do
        if (associated(lwp)) lwp(iCell) = 1000.0_RKIND * lwp_sum
        if (associated(iwp)) iwp(iCell) = 1000.0_RKIND * iwp_sum

        ! IVT calculation
        if (ivt_available) then
            ivt_u_sum = 0.0_RKIND
            ivt_v_sum = 0.0_RKIND
            do k = 1, nVertLevels
                dp = scalars(index_rho_zz,k,iCell) / rdzw(k,iCell)
                ivt_u_sum = ivt_u_sum + scalars(index_qv,k,iCell) * &
                            scalars(index_uReconstructZonal,k,iCell) * dp
                ivt_v_sum = ivt_v_sum + scalars(index_qv,k,iCell) * &
                            scalars(index_uReconstructMeridional,k,iCell) * dp
            end do
            if (associated(ivt_u)) ivt_u(iCell) = ivt_u_sum
            if (associated(ivt_v)) ivt_v(iCell) = ivt_v_sum
            if (associated(ivt_mag)) ivt_mag(iCell) = sqrt(ivt_u_sum**2 + ivt_v_sum**2)
        end if
    end do
  end subroutine mpas_vertical_integrals_compute


  subroutine mpas_vertical_integrals_cleanup()
    nullify(mesh, state)
    nullify(scalars)
    nullify(rdzw)
    nullify(lwp, iwp, ivt_u, ivt_v, ivt_mag)
    nullify(index_qv, index_qc, index_qr, index_qi, index_qs, index_qg)
    nullify(index_rho_zz, index_uReconstructZonal, index_uReconstructMeridional)
    nullify(num_scalars)
    hydrometeors_available = .false.
    ivt_available = .false.
    missing_vars = ''
  end subroutine mpas_vertical_integrals_cleanup

end module mpas_vertical_integrals
