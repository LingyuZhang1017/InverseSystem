module module_global
  implicit none
  integer, parameter :: max_obs_record=300000
  integer, public :: N_obs, M_inv
  character(len=100), public :: output_dir
  character(len=30), public  :: rcfile      
  integer, public :: job_type
  integer, public :: inv_start, inv_end, spin,inv_start_mon,inv_end_mon

  type CONC_RECORD
    character(len=16) sname 
    integer id
    integer jd
    real    obs_conc
    real    sim_ff_conc, sim_bio_conc, sim_ocn_conc, sim_fire_conc
    real    R
  end type
  type(CONC_RECORD), public:: record(max_obs_record)
  
end module


!program test
!  use module_global
!
!  implicit none
!end program test
