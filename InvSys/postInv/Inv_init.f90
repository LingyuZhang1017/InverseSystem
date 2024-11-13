subroutine inv_init()
use module_global
use module_rc
implicit none
character(len=100) job_dir
type(TrcFile)                           ::  rcF
integer status
integer N_month

call Init( rcF, rcfile, status )
call ReadRc( rcF, 'inv.start', inv_start, status)
call ReadRc( rcF, 'inv.end', inv_end, status)
call ReadRc( rcF, 'inv.spin-up', spin, status)
call ReadRc( rcF, 'job.name', job_name, status)
call ReadRc( rcF, 'job.dir', job_dir, status)
call ReadRc( rcF, 'job.type', job_type, status)
call ReadRc( rcF, 'inv.months', N_month, status)
call ReadRc( rcF, 'region.number', regN, status)
call Done( rcF ,status)

output_dir=trim(job_dir)//'/'//trim(job_name)
!M_inv=regN*(inv_end-inv_start+1)*12+1
M_inv=regN*N_month+1
end subroutine
