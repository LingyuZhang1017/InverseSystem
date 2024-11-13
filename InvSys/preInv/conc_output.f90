subroutine conc_output
use module_global
use module_rc
implicit none
character(len=30) obs_conc_file, obs_r_file
real  initconc
real co2
integer i, status
type(TrcFile)                           ::  rcF

call Init( rcF, rcfile, status )
call ReadRc( rcF, 'inv.obs.conc.file', obs_conc_file, status)
call ReadRc( rcF, 'inv.obs.R.file', obs_r_file, status)
call ReadRc( rcF, 'forward.initconc', initconc, status)
call Done( rcF ,status)

open(1,file=trim(output_dir)//'/'//trim(obs_conc_file)) 
open(2,file=trim(output_dir)//'/'//trim(obs_r_file))

do i=1, N_obs
    write(2,*) record(i)%R
    if(job_type==1)then
        co2=record(i)%obs_conc-(record(i)%sim_ff_conc)
    elseif(job_type==2) then
        co2=record(i)%obs_conc-(record(i)%sim_ff_conc-initconc)-(record(i)%sim_fire_conc-initconc)  &
            -(record(i)%sim_ocn_conc-initconc)-(record(i)%sim_bio_conc-initconc)
    else
      print*,'job.type error!'
      stop
    endif 
  write(1,*) co2
enddo

close(1)
close(2)

end subroutine

