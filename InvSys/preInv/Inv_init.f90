subroutine inv_init()
use module_global
use module_rc
implicit none
character(len=100) job_dir
character(len=30) job_name
integer regN,N_month
type(TrcFile)                           ::  rcF
integer status

call Init( rcF, rcfile, status )
call ReadRc( rcF, 'inv.start', inv_start, status)
call ReadRc( rcF, 'inv.end', inv_end, status)
call ReadRc( rcF, 'inv.start.month.only', inv_start_mon, status)
call ReadRc( rcF, 'inv.end.month.only', inv_end_mon, status)
call ReadRc( rcF, 'job.name', job_name, status)
call ReadRc( rcF, 'job.dir', job_dir, status)
call ReadRc( rcF, 'region.number', regN, status)
call ReadRc( rcF, 'job.type', job_type, status)
call ReadRc( rcF, 'inv.months', N_month, status)
call Done( rcF ,status)

output_dir=trim(job_dir)//'/'//trim(job_name)
M_inv=regN*N_month+1
end subroutine
