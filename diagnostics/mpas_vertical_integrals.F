module mpas_vertical_integrals

  use mpas_kind_types, only : RKIND, StrKIND
  use mpas_derived_types, only : MPAS_pool_type, MPAS_clock_type
  use mpas_pool_routines, only : mpas_pool_get_subpool, &
                               mpas_pool_get_array, &
                               mpas_pool_get_dimension
  use mpas_log, only : mpas_log_write

  implicit none

  ! Physical constants
  real(kind=RKIND), parameter :: gravity = 9.80616_RKIND   ! m/s²
  real(kind=RKIND), parameter :: kg_to_g = 1000.0_RKIND    ! conversion factor

  ! Pool pointers
  type(MPAS_pool_type), pointer :: mesh => null()
  type(MPAS_pool_type), pointer :: state => null()
  type(MPAS_pool_type), pointer :: diag => null()

  ! Input variables
  real(kind=RKIND), dimension(:,:,:), pointer :: scalars => null()
  real(kind=RKIND), dimension(:,:), pointer :: pressure => null()
  real(kind=RKIND), dimension(:,:), pointer :: uReconstructZonal => null()
  real(kind=RKIND), dimension(:,:), pointer :: uReconstructMeridional => null()

  ! Output diagnostics
  real(kind=RKIND), dimension(:), pointer :: ivt_u => null()
  real(kind=RKIND), dimension(:), pointer :: ivt_v => null()
  real(kind=RKIND), dimension(:), pointer :: ivt_mag => null()
  real(kind=RKIND), dimension(:), pointer :: lwp => null()
  real(kind=RKIND), dimension(:), pointer :: iwp => null()

  ! Indices
  integer, pointer :: index_qv => null()
  integer, pointer :: index_qc => null()
  integer, pointer :: index_qr => null()
  integer, pointer :: index_qi => null()
  integer, pointer :: index_qs => null()
  integer, pointer :: index_qg => null()
  integer, pointer :: num_scalars => null()

  logical, private :: first_call = .true.
  logical, private :: arrays_associated = .false.

  public :: mpas_vertical_integrals_setup, &
            mpas_vertical_integrals_compute, &
            mpas_vertical_integrals_cleanup

contains

  subroutine mpas_vertical_integrals_setup(configs, structs, clock)
    type(MPAS_pool_type), pointer :: configs
    type(MPAS_pool_type), pointer :: structs
    type(MPAS_clock_type), pointer :: clock

    ! Connect to pools
    call mpas_pool_get_subpool(structs, 'mesh', mesh)
    call mpas_pool_get_subpool(structs, 'state', state)
    call mpas_pool_get_subpool(structs, 'diag', diag)

    ! Verify pool connections
    if (.not. associated(mesh)) call mpas_log_write('ERROR: mesh pool not found!')
    if (.not. associated(state)) call mpas_log_write('ERROR: state pool not found!')

    ! Initialize output arrays on first call
    if (first_call) then
        if (associated(diag)) then
            call mpas_pool_get_array(diag, 'ivt_u', ivt_u)
            call mpas_pool_get_array(diag, 'ivt_v', ivt_v)
            call mpas_pool_get_array(diag, 'ivt_mag', ivt_mag)
            call mpas_pool_get_array(diag, 'lwp', lwp)
            call mpas_pool_get_array(diag, 'iwp', iwp)
            
            ! Initialize output arrays to zero
            if (associated(ivt_u)) ivt_u = 0.0_RKIND
            if (associated(ivt_v)) ivt_v = 0.0_RKIND
            if (associated(ivt_mag)) ivt_mag = 0.0_RKIND
            if (associated(lwp)) lwp = 0.0_RKIND
            if (associated(iwp)) iwp = 0.0_RKIND
        else
            call mpas_log_write('WARNING: diag pool not found. Diagnostics disabled.')
        end if
        first_call = .false.
    end if

    ! Get REQUIRED input arrays with thorough checking
    arrays_associated = .true.
    
    ! 1. Check scalars array (in state)
    call mpas_pool_get_array(state, 'scalars', scalars)
    if (.not. associated(scalars)) then
        call mpas_log_write('ERROR: scalars array not found in state pool!')
        arrays_associated = .false.
    end if
    
    ! 2. Check pressure array (in diag)
    if (associated(diag)) then
        call mpas_pool_get_array(diag, 'pressure', pressure)
        if (.not. associated(pressure)) then
            call mpas_log_write('ERROR: pressure array not found in diag pool!')
            arrays_associated = .false.
        end if
    else
        call mpas_log_write('ERROR: diag pool not available for pressure!')
        arrays_associated = .false.
    end if

    ! Get required indices (in state)
    if (arrays_associated) then
        call mpas_pool_get_dimension(state, 'index_qv', index_qv)
        if (.not. associated(index_qv)) then
            call mpas_log_write('ERROR: index_qv not found!')
            arrays_associated = .false.
        end if
        
        call mpas_pool_get_dimension(state, 'index_qc', index_qc)
        if (.not. associated(index_qc)) then
            call mpas_log_write('ERROR: index_qc not found!')
            arrays_associated = .false.
        end if
        
        call mpas_pool_get_dimension(state, 'index_qi', index_qi)
        if (.not. associated(index_qi)) then
            call mpas_log_write('ERROR: index_qi not found!')
            arrays_associated = .false.
        end if
    end if

    ! Get OPTIONAL arrays (winds for IVT)
    if (associated(diag)) then
        call mpas_pool_get_array(diag, 'uReconstructZonal', uReconstructZonal)
        if (.not. associated(uReconstructZonal)) call mpas_log_write('NOTE: uReconstructZonal not found - IVT_u will be zero')
        
        call mpas_pool_get_array(diag, 'uReconstructMeridional', uReconstructMeridional)
        if (.not. associated(uReconstructMeridional)) call mpas_log_write('NOTE: uReconstructMeridional not found - IVT_v will be zero')
    end if

    ! Additional optional indices
    if (arrays_associated) then
        call mpas_pool_get_dimension(state, 'index_qr', index_qr)
        if (.not. associated(index_qr)) call mpas_log_write('NOTE: index_qr not found - rain excluded from LWP')
        
        call mpas_pool_get_dimension(state, 'index_qs', index_qs)
        if (.not. associated(index_qs)) call mpas_log_write('NOTE: index_qs not found - snow excluded from IWP')
        
        call mpas_pool_get_dimension(state, 'index_qg', index_qg)
        if (.not. associated(index_qg)) call mpas_log_write('NOTE: index_qg not found - graupel excluded from IWP')
        
        call mpas_pool_get_dimension(state, 'num_scalars', num_scalars)
        if (.not. associated(num_scalars)) call mpas_log_write('WARNING: num_scalars not found!')
    end if

  end subroutine mpas_vertical_integrals_setup

subroutine mpas_vertical_integrals_compute()
    integer :: iCell, k, nCells, nVertLevels
    integer, pointer :: p_nCells, p_nVertLevels
    real(kind=RKIND) :: p_top, p_bot, dp, layer_mass
    real(kind=RKIND) :: lwp_sum, iwp_sum, ivt_u_sum, ivt_v_sum
    real(kind=RKIND) :: qv_temp, qc_temp, qr_temp, qi_temp, qs_temp, qg_temp

    ! Skip computation if required arrays aren't associated
    if (.not. arrays_associated) then
        call mpas_log_write('WARNING: Skipping vertical integrals - required arrays not associated')
        return
    end if

    ! Get dimensions
    call mpas_pool_get_dimension(mesh, 'nCells', p_nCells)
    call mpas_pool_get_dimension(mesh, 'nVertLevels', p_nVertLevels)
    nCells = p_nCells
    nVertLevels = p_nVertLevels

    ! Debug: Print pressure levels for first cell
    if (nCells > 0) then
        call mpas_log_write('DEBUG: Pressure levels for iCell=1 (surface to top):')
        do k = 1, nVertLevels
            call mpas_log_write('k=$i, p=$r hPa', &
                intArgs=(/k/), realArgs=(/pressure(k,1)/100.0_RKIND/))
        end do
    end if

    ! Main computation
    do iCell = 1, nCells
        ivt_u_sum = 0.0_RKIND
        ivt_v_sum = 0.0_RKIND
        lwp_sum = 0.0_RKIND
        iwp_sum = 0.0_RKIND

        ! Vertical integration (TOP to BOTTOM: k=nVertLevels -> k=2)
        do k = nVertLevels, 2, -1
            p_top = pressure(k,   iCell)  ! Higher altitude (lower pressure)
            p_bot = pressure(k-1, iCell)  ! Lower altitude (higher pressure)
            dp = p_bot - p_top            ! dp > 0 (since p_bot > p_top)
            layer_mass = dp / gravity      ! Mass per unit area [kg/m²]

            ! Debug output for first cell and top layers
            if (iCell == 1 .and. k >= nVertLevels-4) then
                call mpas_log_write('Layer $i: p_top=$r hPa, p_bot=$r hPa, dp=$r Pa, mass=$r kg/m²', &
                    intArgs=(/k/), realArgs=(/p_top/100.0_RKIND, p_bot/100.0_RKIND, dp, layer_mass/))
            end if

            ! Clip negative moisture values
            qv_temp = max(0.0_RKIND, scalars(index_qv, k, iCell))
            qc_temp = max(0.0_RKIND, scalars(index_qc, k, iCell))
            qi_temp = max(0.0_RKIND, scalars(index_qi, k, iCell))

            ! IVT components (if winds available)
            if (associated(uReconstructZonal) .and. associated(uReconstructMeridional)) then
                ivt_u_sum = ivt_u_sum + qv_temp * uReconstructZonal(k, iCell) * layer_mass
                ivt_v_sum = ivt_v_sum + qv_temp * uReconstructMeridional(k, iCell) * layer_mass
            end if

            ! LWP (cloud + rain if available)
            lwp_sum = lwp_sum + qc_temp * layer_mass * kg_to_g
            if (associated(index_qr)) then
                qr_temp = max(0.0_RKIND, scalars(index_qr, k, iCell))
                lwp_sum = lwp_sum + qr_temp * layer_mass * kg_to_g
            end if

            ! IWP (ice + snow + graupel if available)
            iwp_sum = iwp_sum + qi_temp * layer_mass * kg_to_g
            if (associated(index_qs)) then
                qs_temp = max(0.0_RKIND, scalars(index_qs, k, iCell))
                iwp_sum = iwp_sum + qs_temp * layer_mass * kg_to_g
            end if
            if (associated(index_qg)) then
                qg_temp = max(0.0_RKIND, scalars(index_qg, k, iCell))
                iwp_sum = iwp_sum + qg_temp * layer_mass * kg_to_g
            end if
        end do

        ! Store results
        if (associated(ivt_u))    ivt_u(iCell)    = ivt_u_sum
        if (associated(ivt_v))    ivt_v(iCell)    = ivt_v_sum
        if (associated(ivt_mag))  ivt_mag(iCell)  = sqrt(ivt_u_sum**2 + ivt_v_sum**2)
        if (associated(lwp))      lwp(iCell)      = lwp_sum
        if (associated(iwp))      iwp(iCell)      = iwp_sum
    end do

end subroutine mpas_vertical_integrals_compute

  subroutine mpas_vertical_integrals_cleanup()
    nullify(mesh, state, diag)
    nullify(scalars, pressure, uReconstructZonal, uReconstructMeridional)
    nullify(ivt_u, ivt_v, ivt_mag, lwp, iwp)
    nullify(index_qv, index_qc, index_qr, index_qi, index_qs, index_qg)
    nullify(num_scalars)
    arrays_associated = .false.
  end subroutine mpas_vertical_integrals_cleanup

end module mpas_vertical_integrals
