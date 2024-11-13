subroutine matrix()
use module_global
use module_rc
implicit none
include 'netcdf.inc'
integer ns_t
integer i, k, nx, ny, nt, s, j, nn,n1,np
integer status, n_attrs 

integer iy, is, im, it, ir, dt, dy, year, month, ns
integer sy1, sy2, ey, nvar
integer ny1, ny2
character(len=200) matrix_dir,q_dir,inifile
character(len=30) matrix_file
integer regN, regN1, regf, regN_t
integer, parameter :: nobsin=6000
integer, parameter :: nreg=69
integer nobs
real lat(nobsin), lon(nobsin), xco2_obs(nobsin),axco2(nobsin),grid_point_number(nobsin)
real xco2_uncertainty(nobsin),Yaj(nobsin)
real axinitial(nobsin)
character(len=200) filename, filename1, filename2, filename3,poinumfile
!integer obsnumber(80)
integer, allocatable, dimension(:):: obsnumber
logical alive
integer mon1(12), mon2(12), mon(12)
data mon1/31,28,31,30,31,30,31,31,30,31,30,31/
data mon2/31,29,31,30,31,30,31,31,30,31,30,31/
real, allocatable, dimension(:)  ::z,ave
real, allocatable, dimension(:,:)  :: ax,iarray,axtrans
real, allocatable, dimension(:,:)  :: axr
integer greater,sfolder,sfile,sregion,ii,ngreater,nsum
real jj
character(len=6) ym1,ym2,ym,matrix_sm,matrix_em
integer matrix_sy,matrix_ey
integer inv_sm,inv_em
character(len=6) nfolder,nfile
integer m1,m2,m,ms
integer y1,y2,y
type(TrcFile)                           ::  rcF
call Init( rcF, rcfile, status )
call ReadRc( rcF, 'region.number', regN, status)
call ReadRc( rcF, 'region.with_fraction', regN1, status)
call ReadRc( rcF, 'region.number.fraction', regf, status)
call ReadRc( rcF, 'matrix.dir', matrix_dir, status)
call ReadRc( rcF, 'matrix.start.month', matrix_sm, status)
call ReadRc( rcF, 'matrix.end.month', matrix_em, status)
call ReadRc( rcF, 'inv.start.month.only', inv_sm, status)
call ReadRc( rcF, 'inv.end.month.only', inv_em, status)
call ReadRc( rcF, 'inv.start', matrix_sy, status)
call ReadRc( rcF, 'inv.end', matrix_ey, status)
call ReadRc( rcF, 'inv.months', ms, status )
call ReadRc( rcF, 'inv.matrix.file', matrix_file, status)
call ReadRc( rcF, 'obs.poinumfile', poinumfile, status)
call ReadRc( rcF, 'obs.qfile', q_dir, status)
call ReadRc( rcF, 'obs.initialfile', inifile, status)
call Done( rcF ,status)

allocate(z(regN))
allocate(ave(regN))
allocate(ax(M_inv,N_obs)) 
allocate(iarray(M_inv,N_obs))
allocate(axr(regN,nobsin))
allocate(axtrans(N_obs,M_inv))
allocate(obsnumber(ms))

call matrix_bf(ax,poinumfile,q_dir,inifile,regN,matrix_sm,matrix_em,inv_sm,inv_em,matrix_sy,matrix_ey,ms,M_inv)

print*,"To array succefully!"

axtrans=TRANSPOSE(ax)

open(111,file=trim(output_dir)//'/'//trim(matrix_file),form='binary')
open(222,file=trim(output_dir)//'/'//trim(matrix_file)//'.txt')

do i=1,N_obs
   do j=1,M_inv
      write(111) axtrans(i,j)
      write(222) axtrans(i,j)
   enddo
enddo
close(222)
close(111)
deallocate(obsnumber)
deallocate(z)
deallocate(ave)
deallocate(ax) 
deallocate(iarray)
deallocate(axr)
deallocate(axtrans)
end subroutine

subroutine matrix_bf(ax,poinumfile,q_dir,inifile,regN,matrix_sm,matrix_em,inv_sm,inv_em,matrix_sy,matrix_ey,ms,M_length)
use module_global
use module_rc
implicit none
include 'netcdf.inc'
integer ns_t
integer i, k, nx, ny, nt, s, j, nn,n1,np
integer status, n_attrs ,M_length,nn_obs

integer iy, is, im, it, ir, dt, dy, year, month, ns
integer sy1, sy2, ey, nvar
integer ny1, ny2
character(len=200) matrix_dir,q_dir,inifile
character(len=30) matrix_file
integer regN, regN1, regf, regN_t
integer, parameter :: nobsin=6000
integer nreg
integer nobs
real lat(nobsin), lon(nobsin), xco2_obs(nobsin),axco2(nobsin),grid_point_number(nobsin),ax(M_length,N_obs)
real xco2_uncertainty(nobsin),Yaj(nobsin)
real axinitial(nobsin),axx(regN,nobsin)
character(len=200) filename, filename1, filename2, filename3,poinumfile
integer, allocatable, dimension(:):: obsnumber
logical alive
integer mon1(12), mon2(12), mon(12)
data mon1/31,28,31,30,31,30,31,31,30,31,30,31/
data mon2/31,29,31,30,31,30,31,31,30,31,30,31/
real, allocatable, dimension(:)  ::z,ave
real, allocatable, dimension(:,:)  :: iarray,axtrans
real, allocatable, dimension(:,:)  :: axr
integer greater,sfolder,sfile,sregion,ii,ngreater,nsum
real jj
character(len=6) ym1,ym2,ym,matrix_sm,matrix_em
integer matrix_sy,matrix_ey
integer inv_sm,inv_em
character(len=6) nfolder,nfile
integer m1,m2,m,ms
integer y1,y2,y

allocate(z(regN))
allocate(ave(regN))
allocate(iarray(M_length,N_obs))
allocate(axr(regN,nobsin))
allocate(axtrans(N_obs,M_length))
allocate(obsnumber(ms))
ym=matrix_sm
 
do sfile=1,ms 

   call readmon(ym,y,m)
   filename=trim(poinumfile)//'/mzt_v73_'//ym//'.nc' 
   print*,filename
   
   inquire(file=trim(filename),exist=alive)
   nobs=0
   if(alive)then
         call readncsim(filename,lat,lon,xco2_obs,axco2,grid_point_number,xco2_uncertainty,Yaj,&
            axx,nobsin,nobs,regN)
   endif
   obsnumber(sfile)=nobs
   call nextmon(ym)
enddo
nsum=sum(obsnumber)
print*,obsnumber
print*,nsum

ax=0
ym1=matrix_sm
ym2=matrix_sm
k=0
np=0
do sfolder=1,ms 
   print*,'第sfolder个文件夹：',sfolder,ym1
   do sregion=1,regN
         ym2=ym1
         k=0
         ngreater=0
         
         do sfile=1,36 
            call readmon(ym1,y1,m1)
            call readmon(ym2,y2,m2)
            if(sfile==1)then
               if(y2==matrix_sy)then
                     greater=m2-inv_sm
               else
                     greater=(y2-matrix_sy-1)*12+(12-inv_sm+1)+m2-1
               endif
            endif
            filename=trim(q_dir)//'/'//ym1//'/mzt_v73_'//ym2//'.nc' 
            inquire(file=trim(filename),exist=alive) 
            nobs=0
            if(alive)then
                  call readncsim(filename,lat,lon,xco2_obs,axco2,grid_point_number,xco2_uncertainty,Yaj,&
                     axx,nobsin,nobs,regN)
            endif
            axr=axx
            if(greater+36<ms)then
                  if(sfile==36)then
                     do i=1,regN
                        ave(i)=sum(axr(i,1:nobs))/nobs
                     enddo
                  endif
            endif            
            if(sfile==1)then
                  do i=1,greater
                     ngreater=ngreater+obsnumber(i)
                  enddo
                  k=k+ngreater
            endif
            
            if(sfile==36.and.sregion==1)then
            endif
            filename1=trim(inifile)//'/'//'mzt_v73_initial_'//ym2//'.nc'

            call readnc_initial(filename1,lat,lon,axinitial,nobsin,nobs)
            if((ms-greater-36)<=0)then
               if(sfile==1)then
                     ax((sfolder-1)*regN+sregion,1:ngreater)=0
                     do i=1,nobs
                        k=k+1
                        if(k==nsum)then
                           ax((sfolder-1)*regN+sregion,k)=axr(sregion,i)
                           if(sregion==1)then
                              print*,filename
                              print*,'ngreater:',greater,ngreater
                              print*,k,nsum
                           endif
                           goto 333
                        else
                           ax((sfolder-1)*regN+sregion,k)=axr(sregion,i)
                        endif
                        if(sregion==1)then
                           ax(regN*ms+1,k)=axinitial(i)
                        endif
                     enddo
                     if(sregion==1)then
                        print*,filename
                        print*,'ngreater:',greater,ngreater
                        print*,k,nsum
                     endif
               else
                     do i=1,nobs
                        k=k+1
                        if(k==nsum)then
                           ax((sfolder-1)*regN+sregion,k)=axr(sregion,i)
                           if(sregion==1)then
                              ax(regN*ms+1,k)=axinitial(i)
                              print*,filename
                              print*,'ngreater:',greater,ngreater
                              print*,k,nsum
                           endif
                           goto 333
                        else
                           ax((sfolder-1)*regN+sregion,k)=axr(sregion,i)
                           if(sregion==1)then
                              ax(regN*ms+1,k)=axinitial(i)
                           endif
                        endif
                     enddo
                     if(sregion==1)then
                        print*,filename
                        print*,'ngreater:',greater,ngreater
                        print*,k,nsum
                     endif
               endif
            elseif((ms-greater-36)>0)then
               if(sfile==1)then
                     ax((sfolder-1)*regN+sregion,1:ngreater)=0
                     do i=1,nobs
                        k=k+1
                        ax((sfolder-1)*regN+sregion,k)=axr(sregion,i)
                        if(sregion==1)then
                           ax(regN*ms+1,k)=axinitial(i)
                        endif
                     enddo
               else
                     do i=1,nobs
                        k=k+1
                        ax((sfolder-1)*regN+sregion,k)=axr(sregion,i)
                        if(sregion==1)then
                           ax(regN*ms+1,k)=axinitial(i)
                        endif
                     enddo
               endif
               if(sfile==36)then
                     ax((sfolder-1)*regN+sregion,k+1:nsum)=ave(sregion)                   
               endif
            endif
            call nextmon(ym2)
         enddo
         333 ym2=ym1
   enddo
   call nextmon(ym1)
   ym2=ym1
enddo
end subroutine
