subroutine conc_sim()
use module_global
use module_rc
implicit none
include 'netcdf.inc'
integer ns_t, i, k, nx, ny, nt, s, j, nn
character(len=200) infile, outfile, filename
integer, allocatable, dimension(:) :: sid, tid
integer iy, is, im, it, ir, dt, dy, year, month
integer smonth, syear, emonth, eyear
character(len=200) forward_dir
character(len=30)  forward_file
integer, parameter :: nobsin=6000,nmax=5305
integer, parameter :: nreg=69
integer nobs
real lat(nobsin), lon(nobsin), xco2_obs(nobsin),axco2(nobsin),grid_point_number(nobsin)
real xco2_uncertainty(nobsin),Yaj(nobsin)
real ax1(nobsin)
character(len=200) filename1, filename2, filename3,ffmz4file
!integer obsnumber(80)
integer, allocatable, dimension(:):: obsnumber
logical alive
integer mon1(12), mon2(12), mon(12)
data mon1/31,28,31,30,31,30,31,31,30,31,30,31/
data mon2/31,29,31,30,31,30,31,31,30,31,30,31/
real, allocatable, dimension(:)  :: ax
real, allocatable, dimension(:,:)  :: axr
integer greater,sfolder,sfile,sregion,ii,ngreater,nsum
real jj
character(len=6) ym1,ym2,ym,forward_sm,obs_sm
character(len=6) nfolder,nfile
integer m1,m2,m,ms
integer y1,y2,y

type(TrcFile)                           ::  rcF
integer status
call Init( rcF, rcfile, status )
call ReadRc( rcF, 'forward.dir', forward_dir, status)
call ReadRc( rcF, 'forward.start.month', forward_sm, status)
call ReadRc( rcF, 'inv.months', ms, status )
call ReadRc( rcF, 'obs.ffmz4file', ffmz4file, status )

call Done( rcF ,status)
allocate(ax(N_obs)) 
allocate(obsnumber(ms))

ym=forward_sm
do sfile=1,ms 
   call readmon(ym,y,m)
   filename=trim(forward_dir)//'/mzt_v73_'//ym//'.nc'
   print*,filename
   
   inquire(file=trim(filename),exist=alive) 
   nobs=0
   if(alive)then
      call readncff(filename,lat,lon,xco2_obs,axco2,grid_point_number,xco2_uncertainty,Yaj,&
            ax1,nobsin,nobs)
      print*,' obs number in satellite=', nobs
   endif
   obsnumber(sfile)=nobs
   call nextmon(ym)
enddo
nsum=sum(obsnumber)
print*,obsnumber
print*,nsum

ax=0
ym1=forward_sm
ym2=forward_sm
k=0
do sfile=1,ms 
   call readmon(ym2,y2,m2)
   filename=trim(ffmz4file)//'/mzt_v73_'//ym2//'.nc' 
   inquire(file=trim(filename),exist=alive) 
   if(alive)then
      call readncff(filename,lat,lon,xco2_obs,axco2,grid_point_number,xco2_uncertainty,Yaj,&
            ax1,nobsin,nobs)
   endif
   print*,filename        

   do i=1,nobs
      k=k+1
      ax(k)=axco2(i)+ax1(i)-yaj(i)
   enddo

   call nextmon(ym2)
enddo

print*,"To array succefully!"
nn=0
do nn=1, N_obs
   record(nn)%sim_ff_conc=ax(nn) 
enddo

deallocate(obsnumber)
deallocate(ax)

end subroutine


subroutine readncff(filename,lat,lon,xco2_obs,axco2,grid_point_number,xco2_uncertainty,Yaj,&
   ax1,nobsin,nobs)

   include 'netcdf.inc'
   integer nobsin, nobs
   real lat(nobsin), lon(nobsin), xco2_obs(nobsin),axco2(nobsin),grid_point_number(nobsin)
   real xco2_uncertainty(nobsin),Yaj(nobsin)
   real ax1(nobsin)
   character(len=200) varname
   character(len=200) filename
   
   status=nf_open(trim(filename),nf_nowrite,ncid)
   if ( status/=nf_noerr ) then
      write (*,*) nf_strerror(status)
      write (*,*) '!!Open NetCDF file Error!!'
   end if
   
   status=nf_inq_dimid(ncid, 'nSamples', dimid)
   if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   
   status=nf_inq_dimlen(ncid, dimid, nobs)
   if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   
   varname='lat'
   call get_1d_var(ncid,varname,lat(1:nobs),nobs)
   varname='lon'
   call get_1d_var(ncid,varname,lon(1:nobs),nobs)
   varname='xco2_obs'
   call get_1d_var(ncid,varname,xco2_obs(1:nobs),nobs)
   varname='axco2'
   call get_1d_var(ncid,varname,axco2(1:nobs),nobs)
   varname='grid_point_number'
   call get_1d_var(ncid,varname,grid_point_number(1:nobs),nobs)
   varname='xco2 uncertainty'
   call get_1d_var(ncid,varname,xco2_uncertainty(1:nobs),nobs)
   
   varname='Yaj sum(gosat vertical layers)'
   call get_1d_var(ncid,varname,Yaj(1:nobs),nobs)   
   
   varname='Ax fossil and fire'
   call get_1d_var(ncid,varname,ax1(1:nobs),nobs)
   status = nf_close(ncid)
end subroutine
   
   
   
integer function dmin(a, b, n)
   implicit none
   integer n
   real b(n), a
   real d, d1
   integer i
   d=abs(b(2)-b(1))/2

   do i=1, n
      d1=abs(a-b(i))
      if(d1<d)then
         dmin=i
         return
      endif
   enddo
end function

integer function dmin2(a,b,n)
   implicit none
   integer n
   real b(n),a
   integer i
   real m,m1

   m=(b(1)-a)*(b(1)-a)
   dmin2=1
   do i=2, n
      m1=(b(i)-a)*(b(i)-a)
      if(m1<m)then
         m=m1
         dmin2=i
      endif
   enddo
end function

subroutine readnc3d(filename,varname,a,nx, ny, nt)
   include 'netcdf.inc'
   integer nx, ny, nt
   real a(nx,ny,nt)
   character(len=25) varname
   character(len=200) filename
   
   status=nf_open(trim(filename),nf_nowrite,ncid)
   if ( status/=nf_noerr ) then
      write (*,*) nf_strerror(status)
      write (*,*) '!!Open NetCDF file Error!!'
      stop
   end if
   call get_3d_var(ncid,varname,a,nx,ny,nt)
   status = nf_close(ncid)
end subroutine
      
subroutine readnc4d(filename,varname,a,nx,ny,nz,nt)
   include 'netcdf.inc'
   integer nx,ny,nz,nt
   real a(nx,ny,nz,nt)
   character(len=25) varname
   character(len=200) filename

   status=nf_open(trim(filename),nf_nowrite,ncid)
   if ( status/=nf_noerr ) then
      write (*,*) nf_strerror(status)
      write (*,*) '!!Open NetCDF file Error!!'
      stop
   end if
   call get_4d_var(ncid,varname,a,nx,ny,nt,nt)
   status = nf_close(ncid)
end subroutine
      
