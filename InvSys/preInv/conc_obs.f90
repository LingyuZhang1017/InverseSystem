subroutine conc_obs
use module_global
use module_rc
implicit none
include 'netcdf.inc'
integer, parameter :: nobsin=6000,nmax=5305
integer nreg,regN
integer i, jd, ln
real co2, std
integer id1, mbl1, is, ns
character(len=15) sitename1, lab1, method1
real lat1, lon1, ht1, error1
real, allocatable, dimension(:,:) :: cc
logical fexist
integer n1, n2
integer nn
character(len=200) obs_dir
character(len=30) obs_file
integer obs_number
integer nobs
type station
   integer id
   character(len=15) sitename
   real lat, lon, ht
   character(len=15) lab, method
   real const
end type station
type(station) st(410)
type(TrcFile)                           ::  rcF
integer status
real lat(nobsin), lon(nobsin), xco2_obs(nobsin),axco2(nobsin),grid_point_number(nobsin)
real xco2_uncertainty(nobsin),Yaj(nobsin)
character(len=200) filename, filename1, filename2, filename3
integer, allocatable, dimension(:):: obsnumber
logical alive
integer mon1(12), mon2(12), mon(12)
data mon1/31,28,31,30,31,30,31,31,30,31,30,31/
data mon2/31,29,31,30,31,30,31,31,30,31,30,31/
real, allocatable, dimension(:,:)  :: ax,iarray,axx
real, allocatable, dimension(:,:)  :: axr
integer sfolder,sfile,nsum
real jj
character(len=6) ym1,ym2,ym,obs_sm
character(len=6) nfolder,nfile
integer m1,m2,m,ms
integer y1,y2,y

call Init( rcF, rcfile, status )
call ReadRc( rcF, 'obs.dir', obs_dir, status)
call ReadRc( rcF, 'inv.start.month', obs_sm, status )
call ReadRc( rcF, 'inv.months', ms, status )
call ReadRc( rcF, 'region.number', regN, status)
call Done( rcF ,status)
nreg=regN
allocate(axx(regN,nobsin))
allocate(obsnumber(ms))
N_obs=0
nn=0
ym=obs_sm
do sfile=1,ms
   call readmon(ym,y,m)
   filename=trim(obs_dir)//'/mzt_v73_'//ym//'.nc' 
   print*,filename
   inquire(file=trim(filename),exist=alive) 
   nobs=0
   if(alive)then
      call readncsim(filename,lat,lon,xco2_obs,axco2,grid_point_number,xco2_uncertainty,Yaj,&
            axx,nobsin,nobs,nreg) 
      print*,' obs number in satellite=', nobs
   endif
   obsnumber(sfile)=nobs
   
   do i=1,nobs
      nn=nn+1
      N_obs=N_obs+1
      record(N_obs)%sname='gosat'
      record(N_obs)%id=N_obs
      record(N_obs)%jd=i
      record(N_obs)%obs_conc=xco2_obs(i)
      if(xco2_uncertainty(i)*1.9<1)then
         record(N_obs)%R=1
      else
         record(N_obs)%R=xco2_uncertainty(i)*1.9
      endif
   enddo
   call nextmon(ym)
enddo
nsum=sum(obsnumber)
print*,obsnumber
print*,nsum
print*,'N_obs: ',N_obs
deallocate(obsnumber)
deallocate(axx)
end subroutine


subroutine readncsim(filename,lat,lon,xco2_obs,axco2,grid_point_number,xco2_uncertainty,Yaj,&
   axx,nobsin,nobs,nreg)

   include 'netcdf.inc'
   integer nobsin, nobs,nreg,i
   real lat(nobsin), lon(nobsin), xco2_obs(nobsin),axco2(nobsin),grid_point_number(nobsin)
   real xco2_uncertainty(nobsin),Yaj(nobsin)
   real axx(nreg,nobsin)
   character(len=100) varname
   character(len=200) filename
   character(len=31) vard
   character(len=2) varn
   integer :: j
   real temp_array(nobsin)
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
   !where(lon<0)lon=lon+360 
   varname='xco2_obs'
   call get_1d_var(ncid,varname,xco2_obs(1:nobs),nobs)
   
   varname='axco2'
   call get_1d_var(ncid,varname,axco2(1:nobs),nobs)
   !time=time*3600
   varname='grid_point_number'
   call get_1d_var(ncid,varname,grid_point_number(1:nobs),nobs)
   !print*,grid_point_number(10)
   
   varname='xco2 uncertainty'
   call get_1d_var(ncid,varname,xco2_uncertainty(1:nobs),nobs)
   !print*,xco2_uncertainty(10)
   
   varname='Yaj sum(gosat vertical layers)'
   call get_1d_var(ncid,varname,Yaj(1:nobs),nobs)
   !print*,Yaj(10)

   do i = 1, nreg
      write(varn, '(i2)') i
      vard = trim('Ax sum(gosat vertical layers)' // trim(adjustl(varn))) 
      !vard=trim('Ax sum(gosat vertical layers)'//trim(varn)) 
      !print*, vard
      varname = vard
      !print*, varname
      ! 将数据读取到中间数组 temp_array 中
      call get_1d_var(ncid, varname, temp_array(1:nobs), nobs)
      ! 将中间数组的数据复制到 axx 中
      do j = 1, nobs
         axx(i, j) = temp_array(j)
      end do
   end do

   status = nf_close(ncid)
end subroutine

subroutine readnc_initial(filename,lat,lon,ax1,nobsin,nobs)

   include 'netcdf.inc'
   integer nobsin, nobs
   real lat(nobsin), lon(nobsin), xco2_obs(nobsin),axco2(nobsin),grid_point_number(nobsin)
   real ax1(nobsin)
   character(len=100) varname
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
   
   varname='Ax sum(gosat vertical layers)1'
   call get_1d_var(ncid,varname,ax1(1:nobs),nobs)

   status = nf_close(ncid)
end subroutine


subroutine get_4d_var(ncid,vname,a,nx,ny,nz,nt)
   include 'netcdf.inc'
   integer nx,ny,ncid,nt,nz
   real a(nx,ny,nz,nt)
   character(len=25) vname
   integer*4   :: start(10)
   integer*4   :: count(10)
   integer     :: dimids(10)! allow up to 10 dimensions
   integer     :: dimid, xtype, ndim
   character(len=31) :: dummy

!   Retrieve data for Variable 'XLAT'
   status= nf_inq_varid(ncid, trim(vname),varid)
   status=nf_inq_var(ncid,  varid,dummy,xtype,ndim,dimids,natts)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   do j=1,ndim
   status=nf_inq_dim(ncid,dimids(j),dummy,len)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   start(j)=1 ; count(j)=len
   end do
   status=nf_get_vara_real(ncid,  varid,start,count,a)
end subroutine


subroutine get_1d_var(ncid,vname,a,n)
   include 'netcdf.inc'
   integer n,ncid
   real a(n)
   character(len=50) vname
   integer*4   :: start(10)
   integer*4   :: count(10)
   integer     :: dimids(10)! allow up to 10 dimensions
   integer     :: dimid, xtype, ndim
   character(len=50) :: dummy

!   Retrieve data for Variable 'XLAT'
   status= nf_inq_varid(ncid, trim(vname),varid)
   status=nf_inq_var(ncid,  varid,dummy,xtype,ndim,dimids,natts)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   do j=1,ndim
   status=nf_inq_dim(ncid,dimids(j),dummy,len)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   start(j)=1 ; count(j)=len
   end do
   status=nf_get_vara_real(ncid,  varid,start,count,a)
end subroutine

subroutine get_2d_var(ncid,vname,a,nx,ny)
   include 'netcdf.inc'
   integer nx,ny,ncid
   real a(nx,ny)
   character(len=25) vname
   integer*4   :: start(10)
   integer*4   :: count(10)
   integer     :: dimids(10)! allow up to 10 dimensions
   integer     :: dimid, xtype, ndim
   character(len=31) :: dummy

!   Retrieve data for Variable 'XLAT'
   status= nf_inq_varid(ncid, trim(vname),varid)
   status=nf_inq_var(ncid,  varid,dummy,xtype,ndim,dimids,natts)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   do j=1,ndim
   status=nf_inq_dim(ncid,dimids(j),dummy,len)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   start(j)=1 ; count(j)=len
   end do
   status=nf_get_vara_real(ncid,  varid,start,count,a)
end subroutine

subroutine get_3d_var(ncid,vname,a,nx,ny,nt)
   include 'netcdf.inc'
   integer nx,ny,ncid,nt
   real a(nx,ny,nt)
   character(len=25) vname
   integer*4   :: start(10)
   integer*4   :: count(10)
   integer     :: dimids(10)! allow up to 10 dimensions
   integer     :: dimid, xtype, ndim
   character(len=31) :: dummy

!   Retrieve data for Variable 'XLAT'
   status= nf_inq_varid(ncid, trim(vname),varid)
   status=nf_inq_var(ncid,  varid,dummy,xtype,ndim,dimids,natts)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   do j=1,ndim
   status=nf_inq_dim(ncid,dimids(j),dummy,len)
      if ( status/=nf_noerr ) write (*,*) nf_strerror(status)
   start(j)=1 ; count(j)=len
   end do
   status=nf_get_vara_real(ncid,  varid,start,count,a)
end subroutine

subroutine handle_err(status)
   integer status
   if (status .NE. NF_NOERR) then
         write(*,*), NF_STRERROR(status), status
         stop 'program stoped, and you must find theyour source code.'
   endif
end subroutine

subroutine readmon(sday,year,month)
   implicit none
   character(len=6) sday
   integer year, month

   read(sday(1:4),*) year
   read(sday(5:6),*) month

   write(sday,'(i4,i2.2)') year, month
end subroutine

subroutine nextmon(sday)
   implicit none
   character(len=6) sday
   integer year, month

   read(sday(1:4),*) year
   read(sday(5:6),*) month

   month=month+1

   if(month>12)then
      month=1
      year=year+1
   endif  
   write(sday,'(i4,i2.2)') year, month
end subroutine