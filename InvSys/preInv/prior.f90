subroutine prior()
   use module_rc
   use module_global
   implicit none
   integer, parameter :: nx=360, ny=180 !（-180，180）（-90，90）
   real initco2
   character(len=30) fprior_file, qprior_file, region_file,var_region
   character(len=100) region_dir, bio_dir, ocn_dir, npp_dir,filein
   real, allocatable, dimension(:) :: fprior, qprior
   integer region(nx, ny)
   real region1(nx, ny)
   integer i, j, status
   type(TrcFile)                           ::  rcF


   call Init( rcF, rcfile, status )
   call ReadRc( rcF, 'region.dir', region_dir, status)
   call ReadRc( rcF, 'region.file', region_file, status)
   call ReadRc( rcF, 'forward.initconc', initco2, status)
   call ReadRc( rcF, 'inv.prior.flux.file', fprior_file, status)
   call ReadRc( rcF, 'inv.prior.Q.file', qprior_file, status)
   call Done( rcF ,status)

   allocate(fprior(M_inv))
   allocate(qprior(M_inv))
   fprior(M_inv)=1.0
   qprior(M_inv)=0.0
   !read region map
   filein=trim(region_dir)//'/'//trim(region_file)
   var_region='region'
   call readncRegion(filein, var_region, region1, nx, ny)
   do i = 1,nx
      do j=1,ny
         region(i,j)=int(region1(i,j))
      enddo
   enddo
   print*,filein

   if(job_type == 1)then    !  sub ff and fire 
      call calc_flux_reg2(fprior)
   elseif(job_type == 2)then  ! sub ff, fire, ocn, and bio
      fprior(1:M_inv-1)=0.0
   else
      print*, 'job.type error!'
      stop
   endif
   !call cal_q_prior(qprior, region, nx, ny) 
   call cal_q_prior_lat30SNSD(qprior)

   do i=1,M_inv
      if (qprior(i)>=0.1)then
         qprior(i)=0.1
      endif
   enddo

   open(1,file=trim(output_dir)//'/'//trim(fprior_file))
   do i=1, M_inv
      write(1,*) fprior(i)
   enddo
   close(1)

   open(2,file=trim(output_dir)//'/'//trim(qprior_file))
   do i=1, M_inv
      write(2,*) qprior(i)
   enddo
   close(2) 
end subroutine


subroutine calc_flux_reg2(fpri)
   use module_rc
   use module_global
   implicit none
   include 'netcdf.inc'
   integer it, i, j,flag,k
   integer iy, im, id,imd,imn,yy,mm,dd,monthN,regN,landN,oceanN
   character(len=200) fileout, filein,filein2
   real, allocatable, dimension(:,:) :: xbio,xocn
   real  :: fpri(M_inv)
   real :: p,p1,p2
   character(len=20) models(12)
   data models/'CABLE-POP','DLEM','ISAM','LPX-Bern','OCN','ORCHIDEE','ORCHIDEEv3','SDGVM','VISIT','YIBs','BEPS','CASA'/
   type(TrcFile)                           ::  rcF
   integer status
   call Init( rcF, rcfile, status )
   call ReadRc( rcF, 'inv.months', monthN, status)
   call ReadRc( rcF, 'region.number', regN, status)
   call ReadRc( rcF, 'region.land.number', landN, status)
   call ReadRc( rcF, 'prior.TRENDY.file', filein, status)
   call ReadRc( rcF, 'prior.ocnflux.file', filein2, status)
   call Done( rcF ,status)
   fpri=1.0
   open(14,file=trim(filein2))
   open(13,file=trim(filein))
   k=0
   do i=1,monthN
      do j=1,regN
         k=k+1
         if(j<=51)then
            read(13,*) p
            read(14,*) p1
            fpri(k)=p
         elseif(j>51)then
            read(14,*) p2
            fpri(k)=p2
         endif
      enddo
   enddo
   close(13)
   close(14)
   print*,'--fprior calculate Successfully--'
end subroutine


subroutine cal_q_prior_lat30SNSD(qprior)
   use module_rc
   use module_global
   implicit none
   character(len=100) job_dir
   integer  nx, ny
   real qprior(M_inv), qprior1(M_inv),qprior2(M_inv)
   integer i, j, k
   type(TrcFile)                           ::  rcF
   integer status
   call Init( rcF, rcfile, status )
   call ReadRc( rcF, 'job.dir', job_dir, status)
   call Done( rcF ,status)
   open(1,file='preInv/Inversion_input/SD_priori_uncert.inv')
   open(2,file='preInv/Inversion_input/npp_priori_uncert.inv')
   qprior=0
   qprior1=0
   qprior2=0
   do i=1,M_inv
      if((MOD(i,69)>=12 .and. MOD(i,69)<=17) .or. (MOD(i,69)>=27 .and. MOD(i,69)<=32) .or. (MOD(i,69)>=42 .and. MOD(i,69)<=44) .or. (MOD(i,69)>=46 .and. MOD(i,69)<=51))then
         read(1,*) qprior1(i)
         read(2,*) qprior2(i)
         qprior(i)=qprior1(i)
      else
         read(1,*) qprior1(i)
         read(2,*) qprior2(i)
         qprior(i)=qprior2(i)
      endif
   enddo
   close(2)
   close(1)
   
end subroutine


Function dxyp( lat, re)
   implicit none
   real, parameter :: ae=6.371e3, pi=3.14159265358979
   real, parameter :: gtor=pi/180
   real re, lat, l
   real                  ::  dxx,dyy
   real dxyp

   dxx = 1.0*gtor*re
   dyy = 1.0*gtor*re
   l = (lat-re/2)*gtor

   dxyp = dxx * (sin(l+dyy)-sin(l))*ae**2

   return

End Function


subroutine caldays(rundays,sy,sm,sd, ey,em, ed)
   implicit none
   integer rundays, sy, sm, sd, ey, em, ed
   integer m1, m2
   integer mon(12), mon1(12), mon2(12)
   integer iy, im, id
   data mon1/31,28,31,30,31,30,31,31,30,31,30,31/
   data mon2/31,29,31,30,31,30,31,31,30,31,30,31/

   rundays=0
   do iy=sy, ey
      if(iy==2012.or.iy==2004.or.iy==2008.or.iy==2020.or.iy==2000.or.iy==2016.or.iy==2024.or.iy==2028.or.iy==2032)then
         mon=mon2
      else
         mon=mon1
      endif
      if(iy==sy) then
         m1=sm
         m2=12
      elseif(iy==ey)then
         m1=1
         m2=em
      elseif(iy==sy.and.iy==ey)then
         m1=sm
         m2=em
      else
         m1=1
         m2=12
      endif
      do im=m1, m2
         do id=1, mon(im)
               rundays=rundays+1
         enddo
      enddo
   enddo
end subroutine

subroutine calmondays(y,m,mon)
   implicit none
   integer y, m
   integer i,monthnumber
   integer mon1(12), mon2(12)
   integer mon
   data mon1/31,28,31,30,31,30,31,31,30,31,30,31/
   data mon2/31,29,31,30,31,30,31,31,30,31,30,31/

   if(y==2012.or.y==2004.or.y==2008.or.y==2020.or.y==2000.or.y==2016.or.y==2024.or.y==2028.or.y==2032)then
      mon=mon2(m)
   else
      mon=mon1(m)
   endif
end subroutine


subroutine monarray(sy,sm,m,monthnumber)
   implicit none
   integer sy,sm,iy,im
   integer monthnumber
   integer i,n
   integer m(monthnumber)
   iy=sy
   im=sm 
   do i=1,monthnumber
      call calmondays(iy,im,n)
      m(i)=n
      im=im+1
      if(im>12)then
         iy=iy+1
         im=1
      endif
   enddo
end subroutine

subroutine readnc(fname,var, co2,nx,ny,nt)
include 'netcdf.inc'
integer nx, ny,nt
real co2(nx,ny,nt)  
integer i, j, ncid
character(len=100) fname
character(len=30) var

   status=nf_open(trim(fname),nf_nowrite,ncid)
   if ( status/=nf_noerr ) then
   write (*,*) nf_strerror(status)
   write (*,*) '!!Open NetCDF file Error!!'
      stop
   end if
call get_3d_var(ncid,var,co2,nx,ny,nt)
status = nf_close(ncid)

end subroutine

subroutine nextday(id, im, iy, mon)
implicit none
integer id, im, iy, mon(12)
id=id+1
   if(id==mon(im)+1)then
         id=1
         im=im+1
         if(im==13)then
               im=1
               iy=iy+1
         endif
   endif
end subroutine

subroutine readncRegion(fname,var,region,nx,ny)
   include 'netcdf.inc'
   integer nx, ny,nt
   integer region(nx,ny)   
   integer i, j, ncid
   character(len=100) fname
   character(len=30) var

   status=nf_open(trim(fname),nf_nowrite,ncid)
   if ( status/=nf_noerr ) then
      write (*,*) nf_strerror(status)
      write (*,*) '!!Open NetCDF file Error!!'
   end if
   call get_2d_var(ncid,var,region,nx,ny)
   status = nf_close(ncid)
end subroutine
