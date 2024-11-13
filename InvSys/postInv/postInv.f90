program  postInv
  use module_rc
  use module_global
  implicit none
  integer, parameter :: nx=360, ny=180
  integer region(nx, ny),region_national(nx,ny)
  real region1(nx, ny),region_national1(nx,ny)
  real, allocatable, dimension(:,:,:) :: flux, uncert, bio, ocean,priflux,priunc,flux_national,prior_national
  real, allocatable, dimension(:,:,:,:)  :: flux_1d, uncert_1d, bio_1d, ocean_1d,prior_1d
  integer i, j,k,k1,k2,k3,s,m,y
  character(len=10) datetime
  type(TrcFile)                           ::  rcF
  integer status,regCluNum
  character(len=150) :: region_dir,filein,fileout
  character(len=50) :: region_file,var_region
  character(len=30) national_name(10)
  character(len=4) ystr
  real, allocatable, dimension(:) :: qprior,qregpri
  real, allocatable, dimension(:,:) :: regunc,regunc2,flux_national_ave,prior_national_ave
  real,allocatable,dimension(:,:,:) :: scale ,scaleo,unco
  real,allocatable,dimension(:) :: regarea
  real, allocatable, dimension(:,:) :: ur,qapri
  real, external :: dxyp !?
  data national_name/'EU','Australia','India','Argentina','Canada','USA','China','Russia','Brazil','Kazakhstan'/
  if (iargc() .lt. 1) then
    write(6,*) ' usage: preInv.exe  inv.rc '
    stop
  endif
  call getarg(1,rcfile)

  print*, 'init ...' 
  call inv_init()

  call Init( rcF, rcfile, status )
  call ReadRc( rcF, 'region.dir', region_dir, status)
  call ReadRc( rcF, 'region.file', region_file, status)
  call ReadRc( rcF, 'region.cluster.number', regCluNum, status)
  call Done( rcF ,status)

  filein=trim(region_dir)//'/'//trim(region_file)
  print*,filein
  var_region='region'
  call readncRegion(filein, var_region, region1, nx, ny)
  do i = 1,nx
    do j=1,ny
        region(i,j)=int(region1(i,j))
    enddo
  enddo
  filein='postInv/region_top10_0-360_nc3.nc'
  print*,filein
  var_region='region'
  call readncRegion(filein, var_region, region_national1, nx, ny)
  do i = 1,nx
    do j=1,ny
        region_national(i,j)=int(region_national1(i,j))
    enddo
  enddo
  allocate(regarea(regN))
  do i=1,regN
    call cal_area(region, i, regarea(i), nx, ny)
  enddo
  regarea=regarea*1000000

  allocate(flux(inv_end-inv_start+1,12,regN))
  allocate(uncert(inv_end-inv_start+1,12,regN))

  allocate(flux_1d(nx,ny,inv_end-inv_start+1,12))
  allocate(prior_1d(nx,ny,inv_end-inv_start+1,12))
  allocate(flux_national(10,inv_end-inv_start+1-spin,12))
  allocate(prior_national(10,inv_end-inv_start+1-spin,12))
  allocate(flux_national_ave(10,12))
  allocate(prior_national_ave(10,12))

  allocate(uncert_1d(nx,ny,inv_end-inv_start+1,12))

  print*, 'read posterior ...'
  allocate(regunc(regCluNum,inv_end-inv_start+1))
  allocate(regunc2(regCluNum,inv_end-inv_start+1))
  allocate(ur(regN,inv_end-inv_start+1))
  allocate(qapri(regN,inv_end-inv_start+1))

  call Read_Posterior(flux, flux_1d, region, nx, ny,regCluNum)
  
  ! subtract the conc of ff, fire, bio, ocean, from obs conc
  allocate(bio(inv_end-inv_start+1,12,regN))
  allocate(ocean(inv_end-inv_start+1,12,regN))
  allocate(bio_1d(nx,ny,inv_end-inv_start+1,12))
  allocate(ocean_1d(nx,ny,inv_end-inv_start+1,12))
  allocate(priflux(inv_end-inv_start+1,12,regN))
  allocate(priunc(inv_end-inv_start+1,12,regN))
  allocate(qprior(M_inv))
  allocate(qregpri(regCluNum))
  allocate(unco(nx,ny,1))
  print*, 'read priori ...'

  call Read_Prior2(prior_1d,bio,ocean,bio_1d, ocean_1d, region, nx, ny)

  if(job_type == 2 ) then
    flux=flux+bio+ocean
    flux_1d=flux_1d+bio_1d+ocean_1d
  endif

  print*, 'output ...'

  allocate(scale(inv_end-inv_start+1-spin,12,regN))
  allocate(scaleo(nx,ny,1))

  call Output_Posterior(flux,flux_1d,bio,bio_1d,ocean,ocean_1d,nx,ny,scale,regCluNum)

  open(1,file=trim(output_dir)//'/'//'post-pri.txt')
  do i=1,inv_end-inv_start+1-spin
    do j=1,12
      do k=1,regN
        write(1,*) scale(i,j,k)
      enddo
    enddo
  enddo
  close(1)

  open(1,file=trim(output_dir)//'/'//'area.txt')
  do i=1,regN
  write(1,*) regarea(i)
  enddo
  close(1)
  print*,'SUCCEFFULLY!!!!!!!'

  flux_national=0
  prior_national=0
  do i=1,nx
    do j=1,ny
      if (region_national(i,j)>=1 .and. region_national(i,j)<=10)then
        do y=1+spin,inv_end-inv_start+1
          do m=1,12
            flux_national(region_national(i,j),y-spin,m)=flux_national(region_national(i,j),y-spin,m)+flux_1d(i,j,y,m)*dxyp(j-90.5,1.0)*10e6/10e15
            prior_national(region_national(i,j),y-spin,m)=prior_national(region_national(i,j),y-spin,m)+prior_1d(i,j,y,m)*dxyp(j-90.5,1.0)*10e6/10e15
          enddo
        enddo
      else
        cycle
      endif
    enddo
  enddo
  do k=1,10
    do y=1,inv_end-inv_start+1-spin
      write(ystr,'(i4)') inv_start+spin+y-1
      open(1,file=trim(output_dir)//'/national_'//trim(adjustl(ystr))//trim(job_name)//'_monthly_'//trim(national_name(k))//'.txt')
        write(1,*) 'Month     Pri_flux       Post_flux'
        do j=1, 12
          write(1,'(i3, 2f10.3)') j, prior_national(k,y,j), flux_national(k,y,j)
        enddo
      close(1)
    enddo
  enddo

  prior_national_ave=0
  flux_national_ave=0
  do i=1,inv_end-inv_start+1-spin
    prior_national_ave=prior_national_ave+prior_national(:,i,:)
    flux_national_ave=flux_national_ave+flux_national(:,i,:)
  enddo
  prior_national_ave=prior_national_ave/(inv_end-inv_start-spin+1)
  flux_national_ave=flux_national_ave/(inv_end-inv_start-spin+1)
  do k=1,10
    !do y=1,inv_end-inv_start+1-spin
    !  write(ystr,'(i4)') inv_start+spin+y-1
      open(1,file=trim(output_dir)//'/national_ave_'//trim(job_name)//'_monthly_'//trim(national_name(k))//'.txt')
        write(1,*) 'Month     Pri_flux       Post_flux'
        do j=1, 12
          write(1,'(i3, 2f10.3)') j, prior_national_ave(k,j), flux_national_ave(k,j)
        enddo
      close(1)
    !enddo
  enddo
  
!do y=1,inv_end-inv_start+1-spin
!  write(ystr,'(i4)') inv_start+spin+y-1
  open(1,file=trim(output_dir)//'/national_ave_'//trim(job_name)//'_annual'//'.txt')
    write(1,*) 'Name     Pri_flux       Post_flux'
    do k=1,10
      !do j=1, 12
        write(1,'(a20 , 2f12.5)') national_name(k), sum(prior_national_ave(k,1:12)), sum(flux_national_ave(k,1:12))
      !enddo
    enddo
  close(1)
!enddo
  
end program

subroutine Output_Posterior(flux,flux_1d, bio, bio_1d, ocean, ocean_1d, nx, ny,scale,regCluNum)
  use module_rc
  use module_global
  implicit none
  integer nx, ny
  real flux(inv_end-inv_start+1,12,regN)
  real uncert(inv_end-inv_start+1,12,regN)
  real priunc(inv_end-inv_start+1,12,regN)
  real flux_1d(nx,ny,inv_end-inv_start+1,12)
  real bio(inv_end-inv_start+1,12,regN)
  real ocean(inv_end-inv_start+1,12,regN)
  real bio_1d(nx,ny,inv_end-inv_start+1,12)
  real ocean_1d(nx,ny,inv_end-inv_start+1,12)
  real pri(inv_end-inv_start+1,12,regN), pri_1d(nx,ny,inv_end-inv_start+1,12)
  real aflux(regN), mflux(12,regN), aflux_1d(nx,ny), mflux_1d(nx,ny,12),apostunc(regN),apriunc(regN)  
  real annual_flux(inv_end-inv_start+1-spin,regN),month_flux(inv_end-inv_start+1-spin,12,regN)
  real annual_pri(inv_end-inv_start+1-spin,regN),month_pri(inv_end-inv_start+1-spin,12,regN)
  real annual_postunc(inv_end-inv_start+1-spin,regN),annual_priunc(inv_end-inv_start+1-spin,regN) 
  real apri(regN), mpri(12,regN), apri_1d(nx,ny), mpri_1d(nx,ny,12),mpostunc(12,regN),mpriunc(12,regN)
  real aflux_y(regN,inv_end-inv_start+1), aflux_1d_y(nx,ny,inv_end-inv_start+1)
  real annual_flux1d(inv_end-inv_start+1-spin,nx,ny)
  real scale(inv_end-inv_start+1-spin,12,regN) !
  real, allocatable, dimension(:) :: spc_aflux, spc_apri,spc_apostunc,spc_apriunc,spc_afluxave,spc_apriave
  real, allocatable, dimension(:,:) :: spc_mflux, spc_mpri,spc_mpostunc,spc_mpriunc,spc_mfluxave,spc_mpriave
  integer regCluNum, Num
  integer:: EuropeId(6)=(/19,20,21,22,25,26/) 
  integer:: SouthAmericaId(5)=(/14,15,16,17,18/) 

  type Region
    character(len=100) regname
    integer id(69)
    integer num
  end type
  type(Region), allocatable, dimension(:) :: spcreg
  
  integer analysis_st, analysis_ed
  integer as, ae, na
  type(TrcFile)                           ::  rcF
  integer status
  character(len=1000) region_str
  character(len=4) ystr
  integer y, m, i, j, jj,mm,k

  call Init( rcF, rcfile, status )
  call ReadRc( rcF, 'region.cluster.name', region_str, status)
  call ReadRc( rcF, 'inv.analysis.start', analysis_st, status)
  call ReadRc( rcF, 'inv.analysis.end', analysis_ed, status)
  allocate(spcreg(regCluNum)) 
  allocate(spc_aflux(regCluNum))
  allocate(spc_apri(regCluNum))
  allocate(spc_mflux(12,regCluNum))
  allocate(spc_mpri(12,regCluNum))
  allocate(spc_mfluxave(12,regCluNum))
  allocate(spc_afluxave(regCluNum))
  allocate(spc_mpriave(12,regCluNum))
  allocate(spc_apriave(regCluNum))

  read(region_str,*) spcreg(1:regCluNum)%regname
  print*, spcreg(1:regCluNum)%regname

  do i=1, regCluNum
    call ReadRc( rcF, 'region.'//trim(spcreg(i)%regname)//'.number', Num, status)
    call ReadRc( rcF, 'region.'//trim(spcreg(i)%regname)//'.ids', region_str, status)
!    print*, region_str, Num 
    read(region_str,*) spcreg(i)%id(1:Num)
      spcreg(i)%num=num
    print*, spcreg(i)%regname
    print*, spcreg(i)%num
    print*, spcreg(i)%id(1:Num)
  enddo
  call Done( rcF ,status)

  pri=bio+ocean
  !pri_1d=bio_1d+ocean_1d

  do i=1,inv_end-inv_start+1-spin
    do j=1,12
      do k=1,regN
        scale(i,j,k)=flux(i+spin,j,k)-pri(i+spin,j,k)
      enddo
    enddo
  enddo

  open(1,file=trim(output_dir)//'/'//'post_analysis.txt')
  do i=1,inv_end-inv_start+1-spin
    do j=1,12
      do k=1,regN
        write(1,*) flux(i+spin,j,k)
      enddo
    enddo
  enddo
  close(1)

  open(1,file=trim(output_dir)//'/'//'pri_analysis.txt')
  do i=1,inv_end-inv_start+1-spin
    do j=1,12
      do k=1,regN
        write(1,*) pri(i+spin,j,k)
      enddo
    enddo
  enddo
  close(1)

  open(1,file=trim(output_dir)//'/post_annual.txt')
  do k=1,regN
    write(1,*) sum(flux(spin+1:inv_end-inv_start+1,1:12,k))/(spin+1)
    write(*,*) sum(flux(spin+1:inv_end-inv_start+1,1:12,k))/(spin+1)

  enddo
  close(1)
  open(1,file=trim(output_dir)//'/prior_annual.txt')
  do k=1,regN
    write(1,*) sum(pri(spin+1:inv_end-inv_start+1,1:12,k))/(spin+1)
    write(*,*) sum(pri(spin+1:inv_end-inv_start+1,1:12,k))/(spin+1)
  enddo
  close(1)
  aflux=0
  mflux=0
  aflux_1d=0
  mflux_1d=0
  apri=0
  mpri=0
  aflux_1d=0
  mflux_1d=0
  aflux_y=0
  aflux_1d_y=0
  as=analysis_st-inv_start+1 
  ae=analysis_ed-inv_start+1  
  na=analysis_ed-analysis_st+1
  month_flux=0
  month_pri=0
  annual_flux=0
  annual_pri=0

  do y=spin+1,inv_end-inv_start+1 
    do i=1,regN
      do m=1, 12
          annual_flux(y-spin,i)=annual_flux(y-spin,i)+flux(y,m,i)
          annual_pri(y-spin,i)=annual_pri(y-spin,i)+pri(y,m,i)
          annual_flux1d(y-spin,:,:)=annual_flux1d(y-spin,:,:)+flux_1d(:,:,y,m)
      enddo
    enddo
  enddo

  !print*,priunc(2,:,1)
  do y=spin+1,inv_end-inv_start+1 
    do m=1, 12
        month_flux(y-spin,m,:)=month_flux(y-spin,m,:)+flux(y,m,:)
        month_pri(y-spin,m,:)=month_pri(y-spin,m,:)+pri(y,m,:)
    enddo
  enddo

spc_apri=0
spc_aflux=0
spc_mpri=0
spc_mflux=0
spc_mpriave=0
spc_mfluxave=0
spc_apriave=0
spc_afluxave=0
do y=1,inv_end-inv_start+1-spin
  spc_apri=0
  spc_aflux=0
  spc_mpri=0
  spc_mflux=0
  mm=inv_start+spin-1+y
  write(ystr,'(i4)') mm
  do i=1, regCluNum   !6
    do j=1, spcreg(i)%num  
        jj=spcreg(i)%id(j)
        spc_apri(i)=spc_apri(i)+annual_pri(y,jj)
        spc_aflux(i)=spc_aflux(i)+annual_flux(y,jj)
        spc_apriave(i)=spc_apriave(i)+annual_pri(y,jj)
        spc_afluxave(i)=spc_afluxave(i)+annual_flux(y,jj)
        spc_mpri(:,i)=spc_mpri(:,i)+month_pri(y,:,jj)
        spc_mflux(:,i)=spc_mflux(:,i)+month_flux(y,:,jj)
        spc_mpriave(:,i)=spc_mpriave(:,i)+month_pri(y,:,jj)
        spc_mfluxave(:,i)=spc_mfluxave(:,i)+month_flux(y,:,jj)
    enddo
  enddo
  !print*,spc_aflux
  
  open(1,file=trim(output_dir)//'/'//trim(adjustl(ystr))//trim(job_name)//'_post_spc_region_annual.txt')
  write(1,*) 'regName     Pri_flux       Post_flux'
  do i=1, regCluNum
    write(1,'(a30, 2f10.5)') spcreg(i)%regname, spc_apri(i), spc_aflux(i)
  write(*,*) spcreg(i)%regname, spc_apri(i), spc_aflux(i)
  enddo
  close(1)

    open(1,file=trim(output_dir)//'/'//trim(adjustl(ystr))//trim(job_name)//'_post_monthly_EuropeIds.txt')
    write(1,*) 'regionid  Month     Pri_flux       Post_flux'
    do k=1,6
      do j=1, 12
        write(1,'(i3,i4, 2f10.3)') j,EuropeId(k), month_pri(y,j,EuropeId(k)), month_flux(y,j,EuropeId(k))
      enddo
    enddo
    close(1)

    open(1,file=trim(output_dir)//'/'//trim(adjustl(ystr))//trim(job_name)//'_post_monthly_SouthAmericaIds.txt')
    write(1,*) 'regionid  Month     Pri_flux       Post_flux'
    do k=1,5
      do j=1, 12
        write(1,'(i3,i4, 2f10.3)') j,SouthAmericaId(k), month_pri(y,j,SouthAmericaId(k)), month_flux(y,j,SouthAmericaId(k))
      enddo
      write(1,'(i3,i4, 2f10.3)') 0,SouthAmericaId(k), sum(month_pri(y,1:12,SouthAmericaId(k))), sum(month_flux(y,1:12,SouthAmericaId(k)))
    enddo
    close(1)

  do i=1, regCluNum
    open(1,file=trim(output_dir)//'/'//trim(adjustl(ystr))//trim(job_name)//'_post_monthly_'//trim(spcreg(i)%regname)//'.txt')
    write(1,*) 'Month     Pri_flux       Post_flux'
    do j=1, 12
      write(1,'(i3, 2f10.3)') j, spc_mpri(j,i), spc_mflux(j,i)
    enddo
    close(1)
  enddo
enddo

do i=1, regCluNum
  open(1,file=trim(output_dir)//'/'//'Ave'//trim(job_name)//'_post_monthly_'//trim(spcreg(i)%regname)//'.txt')
  write(1,*) 'Month     Pri_flux       Post_flux'
  do j=1, 12
    write(1,'(i3, 2f10.3)') j, spc_mpriave(j,i)/(inv_end-inv_start-spin+1), spc_mfluxave(j,i)/(inv_end-inv_start-spin+1)
  enddo
  close(1)
enddo

open(1,file=trim(output_dir)//'/'//'Ave'//trim(job_name)//'_post_spc_region_annual.txt')
write(1,*) 'regName     Pri_flux       Post_flux'
do i=1, regCluNum
  write(1,'(a30, 2f10.5)') spcreg(i)%regname, spc_apriave(i)/(inv_end-inv_start-spin+1), spc_afluxave(i)/(inv_end-inv_start-spin+1)
write(*,*) spcreg(i)%regname, spc_apriave(i)/(inv_end-inv_start-spin+1), spc_afluxave(i)/(inv_end-inv_start-spin+1)
enddo
close(1)
end subroutine

subroutine Read_Posterior(flux, flux_1d, region, nx, ny,regCluNum)
use module_rc
use module_global
implicit none
integer nx, ny,inv_start_mon,inv_end_mon,date,regCluNum
integer region(nx, ny) 
real flux(inv_end-inv_start+1,12,regN)
real uncert(inv_end-inv_start+1,12,regN)
real flux_1d(nx,ny,inv_end-inv_start+1,12)
real fluxo(nx,ny,1)
real glb(inv_end-inv_start+1), ocean(inv_end-inv_start+1), land(inv_end-inv_start+1), chn(inv_end-inv_start+1), na(inv_end-inv_start+1), eur(inv_end-inv_start+1)
real sa(inv_end-inv_start+1),af(inv_end-inv_start+1),aus(inv_end-inv_start+1),tro(inv_end-inv_start+1),trosa(inv_end-inv_start+1),troa(inv_end-inv_start+1)
real troaf(inv_end-inv_start+1),naf(inv_end-inv_start+1),saf(inv_end-inv_start+1),asia(inv_end-inv_start+1)
character(len=30) flux_file, uncert_file,out_uncert_file
real, allocatable, dimension(:) :: postf, u
real, allocatable, dimension(:,:) :: uncertf
real ur(regN,inv_end-inv_start+1)
type(TrcFile)                           ::  rcF
  integer status
integer yy, i, j, k, s,m, y, j1, i1, j2, i2, k2, ii, jj, k1,n1,n2
real narea
character(len=10) datetime
character(len=150) fileout, filein
call Init( rcF, rcfile, status )
call ReadRc( rcF, 'inv.output.flux.file', flux_file, status)
call ReadRc( rcF, 'inv.output.uncert.file', uncert_file, status)
call ReadRc( rcF, 'inv.start.month.only', inv_start_mon, status)
call ReadRc( rcF, 'inv.end.month.only', inv_end_mon, status)
call ReadRc( rcF, 'out.postuncert.file', out_uncert_file, status)

call Done( rcF ,status)
allocate(postf(M_inv))
allocate(uncertf(M_inv,M_inv))
allocate(u(M_inv))

open(1,file=trim(output_dir)//'/'//trim(flux_file))
  print*,output_dir,flux_file
  open(2,file=trim(output_dir)//'/'//trim(uncert_file))
    print*,trim(output_dir)//'/'//trim(uncert_file)
    
    read(1,*)postf
    print*,'postffff========================================='

    do i=1, M_inv
      read(2,*) (uncertf(i,m),m=1,M_inv)
    enddo  
    do i=1, M_inv
      u(i)=uncertf(i,i)
    enddo
  close(1)
close(2)
! flux
k=0
do i=1, inv_end-inv_start+1
  do j=1,12
    if((i==1).and.j<inv_start_mon)then
      flux(i,j,:)=0
    elseif(i==inv_end-inv_start+1.and.j>12)then
      flux(i,j,:)=0
    else
      do s=1,regN
        k=k+1
        flux(i,j,s)=postf(k) !flux(year,month,region)
      enddo
    endif
  enddo
enddo

flux_1d=-999
do k=1, regN
  call cal_area(region, k, narea, nx, ny)
  do i=1, nx
    do j=1, ny
      if(region(i,j)==k)then  
        do s=1,inv_end-inv_start+1
          do m=1, 12
            flux_1d(i,j,s,m)=flux(s,m,k)/narea/1.0e6*1.0e15   
          enddo
        enddo
      endif
    enddo
  enddo
enddo

fileout=trim(output_dir)//'/post1d.nc'
call createnc(fileout, nx, ny, 12*(inv_end-inv_start-spin+1))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
k=1
fluxo=-999
do i=inv_start+spin,inv_end
  do j=1,12
    write(datetime,'(i4.4,i2.2)') i,j
    print*,datetime
    do n1=1,nx
      do n2=1,ny
        fluxo(n1,n2,1)=flux_1d(n1,n2,i-inv_start+1,j)
      enddo
    enddo
    call writenc(fileout, fluxo, nx, ny, 1, k, datetime)
    k=k+1
    fluxo=-999
  enddo
enddo

end subroutine

subroutine Read_Prior2(flux_1d,fbio, focean, bio_1d, ocean_1d, region, nx, ny)
  use module_rc
  use module_global
  implicit none
  include 'netcdf.inc'
  integer nx, ny
  integer, parameter :: nt=8
  integer N_month, N_reg
  integer nx1, ny1    !grid number of output
  real co2(nx,ny,nt),lat(ny), lon(nx)
  real fossil(nx,ny,nt), bio(nx,ny,nt), ocn(nx,ny,nt), fire(nx,ny,nt)
  integer region(nx,ny)
  real, allocatable, dimension(:,:,:) :: co2o
  real, allocatable, dimension(:) :: lat1, lon1
  real flux_1d(nx,ny,inv_end-inv_start+1,12)
  real fluxo(nx,ny,1)
  integer syear, smonth, sday, rundays,year,month
  integer eyear, emonth, eday
  real fbio(inv_end-inv_start+1,12,regN), focean(inv_end-inv_start+1,12,regN)
  real flux(inv_end-inv_start+1,12,regN)
  real bio_1d(nx,ny,inv_end-inv_start+1,12), ocean_1d(nx,ny,inv_end-inv_start+1,12)
  real, allocatable, dimension(:,:,:) :: bio_1d2,ocean_1d2
  character(len=150) dir_fossil, dir_bio, dir_ocn, dir_fire,dir_region,file_region,region_file,region_dir
  character(len=30)  var_fossil, var_bio, var_ocn, var_fire,var_region,fprior_file
  character(len=50) prefix_fossil, prefix_bio, prefix_ocn, prefix_fire
  character(len=50)   output_prefix, output_var
  character(len=10) datetime
  integer mon(12), mon1(12), mon2(12)
  integer it, i, j,flag,k,iii,jjj,s,m,n1,n2
  integer iy, im, id,imd,imn,yy,mm,dd
  character(len=150) fileout, filein,land_idstr,ocean_idstr
  data mon1/31,28,31,30,31,30,31,31,30,31,30,31/
  data mon2/31,29,31,30,31,30,31,31,30,31,30,31/
  integer,allocatable, dimension(:) :: mons
  real, allocatable, dimension(:,:) :: xbio,xocn
  real  :: fpri(M_inv)
  real,parameter :: na=6.022e23 !阿伏伽德罗常数
  integer oceanN,landN
  real f, of, lf, gf,p
  real, external :: dxyp !?
  real narea
  type(TrcFile)                           ::  rcF
  integer status
  call Init( rcF, rcfile, status )
  call ReadRc( rcF, 'inv.months', N_month, status)
  call ReadRc( rcF, 'region.number', N_reg, status)
  call ReadRc( rcF, 'prior.bio.dir', dir_bio, status)
  call ReadRc( rcF, 'prior.ocn.dir', dir_ocn, status)
  call ReadRc( rcF, 'region.land.number', landN, status)
  call ReadRc( rcF, 'region.land.ids', land_idstr, status)
  call ReadRc( rcF, 'region.dir', dir_region, status)
  call ReadRc( rcF, 'region.file', file_region, status)
  call ReadRc( rcF, 'prior.start', syear, status)
  call ReadRc( rcF, 'prior.start.month', smonth, status)
  call ReadRc( rcF, 'prior.end', eyear, status)
  call ReadRc( rcF, 'prior.end.month', emonth, status)
  call ReadRc( rcF, 'prior.start.day', sday, status)
  call ReadRc( rcF, 'prior.end.day', eday, status)
  call ReadRc( rcF, 'region.var', var_region, status)
  call ReadRc( rcF, 'prior.bio.var', var_bio, status)
  call ReadRc( rcF, 'prior.ocn.var', var_ocn, status)
  call ReadRc( rcF, 'prior.bio.prefix', prefix_bio, status)
  call ReadRc( rcF, 'prior.ocn.prefix', prefix_ocn, status)
  call ReadRc( rcF, 'region.dir', region_dir, status)
  call ReadRc( rcF, 'region.file', region_file, status)
  call ReadRc( rcF, 'inv.prior.flux.file', fprior_file, status)
  call Done( rcF ,status)
  
  allocate(xbio(N_month,landN))
  allocate(mons(N_month))
  allocate(bio_1d2(nx,ny,N_month))
  allocate(ocean_1d2(nx,ny,N_month))

  fbio=0
  focean=0
  fpri=0
  open(13,file=trim(output_dir)//'/'//trim(fprior_file))
  k=0
  do i=1,N_month
    do j=1,N_reg
        k=k+1
        read(13,*) p
        fpri(k)=p
    enddo
  enddo
  close(13)
  flag=0
  do i=1,inv_end-inv_start+1
    do j=1,12
      if(i==1.and.j<smonth)then
        cycle
      else
        do k=1,N_reg
          flag=flag+1
          if(k<=51)then
            fbio(i,j,k)=fpri(flag)
          else
            focean(i,j,k)=fpri(flag)
          endif
        enddo
      endif
    enddo
  enddo

  flux=fbio+focean

  
  flux_1d=-999
  do k=1, regN
    call cal_area(region, k, narea, nx, ny)
    do i=1, nx
      do j=1, ny
        if(region(i,j)==k)then  
          do s=1,inv_end-inv_start+1
            do m=1, 12
              flux_1d(i,j,s,m)=flux(s,m,k)/narea/1.0e6*1.0e15   
            enddo
          enddo
        endif
      enddo
    enddo
  enddo

  fileout=trim(output_dir)//'/prior1d.nc'
  call createnc(fileout, nx, ny, 12*(inv_end-inv_start-spin+1))
  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
  k=1
  fluxo=-999
  do i=inv_start+spin,inv_end
    do j=1,12
      write(datetime,'(i4.4,i2.2)') i,j
      print*,datetime
      do n1=1,nx
        do n2=1,ny
          fluxo(n1,n2,1)=flux_1d(n1,n2,i-inv_start+1,j)
        enddo
      enddo
      call writenc(fileout, fluxo, nx, ny, 1, k, datetime)
      k=k+1
      fluxo=-999
    enddo
  enddo


  print*,'--fprior calculate Successfully--'
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

subroutine cal_area(basemap, id, narea, nx, ny)
  implicit none 
  integer nx, ny
  real dxy(ny)
  real narea
  integer basemap(nx, ny), id
  integer i, j
  real, external :: dxyp

  narea=0
  do i=1, nx  
      do j=1, ny
        if(basemap(i,j)==id)then
            narea=narea+dxyp(j-90.5,1.0)
        endif
      enddo
  enddo

end subroutine cal_area

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
     !print*,iy,im,m(i)
  enddo
  !print*,m
end subroutine
subroutine readnc(fname,var, co2,nx,ny,nt)
include 'netcdf.inc'
integer nx, ny,nt
real co2(nx,ny,nt)   !, ocn(nx,ny,nt)   !, zm(nx,ny,nz,nt)   !, zi(nx,ny,nz,nt)
integer i, j, ncid
character(len=150) fname
character(len=30) var

!  print*, trim(fname)
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
  integer region(nx,ny)   !, ocn(nx,ny,nt)   !, zm(nx,ny,nz,nt)   !,zi(nx,ny,nz,nt)
  integer i, j, ncid
  character(len=150) fname
  character(len=30) var


  status=nf_open(trim(fname),nf_nowrite,ncid)
  if ( status/=nf_noerr ) then
    write (*,*) nf_strerror(status)
    write (*,*) '!!Open NetCDF file Error!!'
  end if
  call get_2d_var(ncid,var,region,nx,ny)
  status = nf_close(ncid)
end subroutine

subroutine createnc(fname, nx, ny, nt)
  include 'netcdf.inc'
  integer nx, ny,nt
  integer dims(3), stepid, latid, lonid
  character(len=150) fname
  character(len=30) var
  integer varid, fid, latvarid, lonvarid
  real lat(ny), lon(nx)
  integer i

  do i=1, ny
      lat(i)=i-90.5
  enddo
  do i=1, nx
    lon(i)=i-0.5
  enddo

  !print*, lat
  !print*, lon

  rcode = nf_create(trim(fname),NF_CLOBBER,fid)
  if (rcode /= NF_NOERR) then
      write(6,*) "ERROR: creating file ",trim(fName)
      write(6,*) "ERROR: ",nf_strerror(rcode)
      stop
  end if
  !... begin to define your dimentions
        status=nf_def_dim(fid, 'time', nf_unlimited, stepid)  ! the time is unlimited in this program
        if (status .NE. nf_noerr) call handle_err(status)
  !      print*,'stepid',stepid,nf_unlimited

        status=nf_def_dim(fid, 'lat', ny, latid)
        if (status .NE. nf_noerr) call handle_err(status)
  !     print*,'latid',latid,ny

        status=nf_def_dim(fid, 'lon', nx, lonid)
        if (status .NE. nf_noerr) call handle_err(status)
  !     print*,'lonid',lonid,nx

  !    print*,'dimentions defining end'
      dims(3)=stepid
      dims(2)=latid
      dims(1)=lonid
    var='CO2'
    status=nf_def_var(fid, trim(var), nf_float, 3, dims, varid)
    if (status .NE. nf_noerr) call handle_err(status)
    status=nf_put_att_text(fid, varid, 'long_name', 21, 'Posterior carbon flux')
    if (status .NE. nf_noerr) call handle_err(status)
    status=nf_put_att_text(fid, varid, 'units', 11, 'gC/m2/month')
    if (status .NE. nf_noerr) call handle_err(status)

    status=nf_def_var(fid, 'lat', nf_float, 1, latid, latvarid)
    if (status .NE. nf_noerr) call handle_err(status)
    status=nf_put_att_text(fid, latvarid, 'long_name', 8, 'Latitude')
    if (status .NE. nf_noerr) call handle_err(status)

    status=nf_def_var(fid, 'lon', nf_float, 1, lonid, lonvarid)
    if (status .NE. nf_noerr) call handle_err(status)
    status=nf_put_att_text(fid, lonvarid, 'long_name', 9, 'Longitude')
    if (status .NE. nf_noerr) call handle_err(status)

    status=nf_def_var(fid, 'date', nf_int, 1, stepid, varid)
    if (status .NE. nf_noerr) call handle_err(status)
    !status=nf_def_var(fid, 'datesec', nf_int, 1, stepid, varid)
    !if (status .NE. nf_noerr) call handle_err(status)
    status=nf_def_var(fid, 'time', nf_int, 1, stepid, varid)
    if (status .NE. nf_noerr) call handle_err(status)
    status=nf_enddef(fid)

    status=nf_put_vara_real(fid, latvarid, 1, ny, lat)
    if (status .NE. nf_noerr) call handle_err(status)
    status=nf_put_vara_real(fid, lonvarid, 1, nx, lon)
    if (status .NE. nf_noerr) call handle_err(status)
    status=nf_close(fid)
    
endsubroutine

subroutine writenc(fname, co2, nx, ny, nt, it, datetime)
  include 'netcdf.inc'
  integer nx, ny,nt, it, fid, varid
  real co2(nx,ny,nt)
  character(len=10) datetime
  character(len=30) var
  character(len=150) fname
  integer count(3), start(3)
  integer datesec(1), time(1), date(1)
  integer i,j
  do i=1, nt
    read(datetime,*) date(i)
    read(datetime,'(i4,i2.2)') j, time(i)
  enddo
  ! print*, it, nt
  var='CO2'
  ! print*, time(1:nt)
  
  status=nf_open(trim(fname),nf_write,fid)
    if ( status/=nf_noerr ) then
        write (*,*) nf_strerror(status)
        write (*,*) '!!Open NetCDF file Error!!'
        stop
      end if
  count(3)=nt
  count(2)=ny
  count(1)=nx
  start(3)=1*(it-1)+1
  start(2)=1
  start(1)=1
  ! print*, starssst
  ! print*, count
  status= nf_inq_varid(fid, trim(var),varid)
  status=nf_put_vara_real(fid, varid, start, count, co2)
  status= nf_inq_varid(fid, 'time',varid)
  status=nf_put_vara_int(fid, varid, 1*(it-1)+1, nt, time(1:nt)) 
  status= nf_inq_varid(fid, 'date',varid)
  status=nf_put_vara_int(fid, varid, 1*(it-1)+1, nt, date(1:nt))  
  status=nf_close(fid)
  
  end subroutine
