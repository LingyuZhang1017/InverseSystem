Program PecordInv_Process
use module_global
implicit none

if (iargc() .lt. 1) then
   write(6,*) ' usage: preInv.exe  inv.rc '
   stop
endif
call getarg(1,rcfile)

! init 
call inv_init()

! read obs data
call conc_obs()

! read sim data
call conc_sim()

! output C and R
call conc_output()

! calculate prior flux and Q
call prior()

! create transport matrix
call matrix()

open(1,file=trim(output_dir)//'/N_obs')
open(2,file=trim(output_dir)//'/M_inv')
write(1,*) N_obs
write(2,*) M_inv
close(1)
close(2)

print*, '-------------------------------------------'
print*, '-  preprocess inversion data successful!  -'  
print*, '-------------------------------------------'
end program

