
module module_rc

  implicit none

  ! --- in/out ---------------------

  private

  public  ::  TrcFile
  public  ::  Init, Done
  public  ::  ReadRc


  ! --- const ---------------------------------
  
  character(len=*), parameter  ::  mname = 'Read_Rc'

  ! maximum line length in rc file:
  integer, parameter     ::  buflen = 1000

  ! --- types ---------------------------------

  type TrcFile
    character(len=80)      ::  fname
  end type TrcFile


  ! --- interfaces -------------------------------------

  interface Init
    module procedure rcfile_Init
  end interface

  interface Done
    module procedure rcfile_Done
  end interface

  interface ReadRc
    module procedure ReadRc_i
    module procedure ReadRc_i1
    module procedure ReadRc_r
    module procedure ReadRc_r1
    module procedure ReadRc_l
    module procedure ReadRc_s
  end interface


contains


  ! ================================================================
  ! ===
  ! === init, done
  ! ===
  ! ================================================================


  subroutine rcfile_Init( rcfile, fname, status )

  use module_go_print
    !, only : gol, goErr
    ! --- in/out ---------------------------

    type(TrcFile), intent(out)    ::  rcfile
    character(len=*), intent(in)  ::  fname
    integer, intent(out)          ::  status
    
    ! --- const ---------------------------
    
    character(len=*), parameter  ::  rname = mname//'/rcfile_Init'
    
    ! --- local --------------------------
    
    logical          ::  exist

    ! --- begin ---------------------------

    ! file not present ?
    inquire( file=trim(fname), exist=exist )
    if ( .not. exist ) then
      write (*,'("rcfile not found :")'); call goErr
      write (*,'("  ",a)') trim(fname); call goErr
      write (*,'("in ",a)') rname; call goErr; status=1; return
    end if

    ! store file name:
    rcfile%fname = trim(fname)
    
    ! ok
    status = 0

  end subroutine rcfile_Init


  ! ***


  subroutine rcfile_Done( rcfile,  status )

    ! --- in/out ---------------------------

    type(TrcFile), intent(inout)    ::  rcfile
    integer, intent(out)            ::  status

    ! --- const ---------------------------
    
    character(len=*), parameter  ::  rname = mname//'/rcfile_Done'
    
    ! --- begin ---------------------------

    ! nothing to be done ...
    
    ! ok
    status = 0

  end subroutine rcfile_Done


  ! ================================================================
  ! ===
  ! === general read
  ! ===
  ! ================================================================


  ! Searches the file <filenameResource> for the string
  !  "<key> : "
  ! and save all characters behind the equal sign in <buffer>.
  ! The Resource file may contain comment lines starting with a "!"

  subroutine ReadRcItem( rcfile, key, buffer, status )

    use module_go_print , only : gol, goErr
    use module_go_string, only : goSplitLine, goTab2Space
    use module_go_file  , only : TTextFile, Init, Done, ReadLine

    ! --- in/out ----------------------

    type(TrcFile), intent(in)         ::  rcfile
    character(len=*), intent(in)      ::  key
    character(len=*), intent(out)     ::  buffer
    integer, intent(out)              ::  status

    ! --- const ---------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadRcItem'
    
    ! --- local -----------------------

    type(TTextFile)      ::  file
    integer              ::  iostat
    Integer              ::  nfound
    character(len=1000)   ::  s, skey, sdata
    integer              ::  l

    ! --- begin --------------------------

    ! open commented text file:
    call Init( file, rcfile%fname, status, status='old', comment='!' )
!   IF_ERROR_RETURN(status=1)

    ! no matching lines found yet ...    
    nfound = 0
    
    ! scan all lines 
    do

      ! read next non empty, non comment line:
      call ReadLine( file, s, status )
      if (status<0) exit  ! end of file
 !     IF_ERROR_RETURN(status=1)
      
      ! Andy Jacobson, 10 Apr 2006.  Allows tabs in rc file.
      call goTab2Space( s )  

      ! split at colon:
      call goSplitLine( s, skey, ':', sdata, status )
 !     IF_ERROR_RETURN(status=1)
      
      ! starts with requested key, and no extra text between key and colon ? then found!
      if ( (index(skey,key)==1) .and. (len_trim(key)==len_trim(skey))) then
        buffer = sdata
        nfound = nfound + 1
      end if

    end do

    ! close:
    call Done( file, status )
 !   IF_ERROR_RETURN(status=1)
    
    ! not found ? warning status
    if ( nfound == 0 ) then
      status=-1; return
    end if

    ! multiple matches ?
    if ( nfound > 1 ) then
      write (gol,'("found more than one matching keys in rcfile:")'); call goErr
      write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
      write (gol,'("  key    : ",a)') trim(key); call goErr
      write (gol,'("  found  : ",i4," times")') nfound
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if

    ! ok
    status = 0

  end subroutine ReadRcItem
  
  
  ! ================================================================
  ! ===
  ! === type specific read
  ! ===
  ! ================================================================


  subroutine ReadRc_i( rcfile, key, i, status, default )

    use module_go_print, only : gol, goErr

    ! --- in/out ----------------------------

    type(TrcFile), intent(in)                   ::  rcfile
    character(len=*), intent(in)                ::  key
    integer, intent(out)                        ::  i
    integer, intent(out)                        ::  status
    
    integer, intent(in), optional               ::  default
    
    ! --- const ----------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadRc_i'

    ! --- local -----------------------------

    character(len=buflen)     ::  buffer

    ! --- begin -----------------------------

    ! search key line in rcfile:
    call ReadRcItem( rcfile, key, buffer, status )
    if ( status < 0 ) then
      ! not found; set to default or leave with error:
      if ( present(default) ) then
        i = default
      else
        write (gol,'("key not found and no default specified ...")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else if ( status == 0 ) then
      ! key found; set value:
      read (buffer,*,iostat=status) i
      if ( status /= 0 ) then
        write (gol,'("while reading integer:")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("  value  : ",a)') trim(buffer); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else
      ! some error ...
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
    
    ! ok
    status = 0

  end subroutine ReadRc_i


  ! ***
  subroutine ReadRc_i1( rcfile, key, i, status, default )

    use module_go_print, only : gol, goErr

    ! --- in/out ----------------------------

    type(TrcFile), intent(in)                   ::  rcfile
    character(len=*), intent(in)                ::  key
    integer, intent(out)                        ::  i(:)
    integer, intent(out)                        ::  status
    
    integer, intent(in), optional               ::  default
    
    ! --- const ----------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadRc_i1'

    ! --- local -----------------------------

    character(len=buflen)     ::  buffer

    ! --- begin -----------------------------

    ! search key line in rcfile:
    call ReadRcItem( rcfile, key, buffer, status )
    if ( status < 0 ) then
      ! not found; set to default or leave with error:
      if ( present(default) ) then
        i = default
      else
        write (gol,'("key not found and no default specified ...")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else if ( status == 0 ) then
      ! key found; set value:
      read (buffer,*,iostat=status) i
      if ( status /= 0 ) then
        write (gol,'("while reading integer:")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("  value  : ",a)') trim(buffer); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else
      ! some error ...
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
    
    ! ok
    status = 0

  end subroutine ReadRc_i1


  ! ***


  subroutine ReadRc_r( rcfile, key, r, status, default )

    use module_go_print, only : gol, goErr

    ! --- in/out ----------------------------

    type(TrcFile), intent(in)                   ::  rcfile
    character(len=*), intent(in)                ::  key
    real, intent(out)                           ::  r
    integer, intent(out)                        ::  status
    
    real, intent(in), optional                  ::  default
    
    ! --- const ----------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadRc_r'

    ! --- local -----------------------------

    character(len=buflen)     ::  buffer

    ! --- begin -----------------------------

    ! search key line in rcfile:
    call ReadRcItem( rcfile, key, buffer, status )
    if ( status < 0 ) then
      ! not found; set to default or leave with error:
      if ( present(default) ) then
        r = default
      else
        write (gol,'("key not found and no default specified ...")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else if ( status == 0 ) then
      ! key found; set value:
      read (buffer,*,iostat=status) r
      if ( status /= 0 ) then
        write (gol,'("while reading real :")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("  value  : ",a)') trim(buffer); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else
      ! some error ...
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
    
    ! ok
    status = 0

  end subroutine ReadRc_r
  

  ! ***


  subroutine ReadRc_r1( rcfile, key, r, status, default )

    use module_go_print, only : gol, goErr

    ! --- in/out ----------------------------

    type(TrcFile), intent(in)                   ::  rcfile
    character(len=*), intent(in)                ::  key
    real, intent(out)                           ::  r(:)
    integer, intent(out)                        ::  status
    
    real, intent(in), optional                  ::  default
    
    ! --- const ----------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadRc_r1'

    ! --- local -----------------------------

    character(len=buflen)     ::  buffer
    integer                   ::  k

    ! --- begin -----------------------------

    ! search key line in rcfile:
    call ReadRcItem( rcfile, key, buffer, status )
    if ( status < 0 ) then
      ! not found; set to default or leave with error:
      if ( present(default) ) then
        r = default
      else
        write (gol,'("key not found and no default specified ...")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else if ( status == 0 ) then
      ! key found; set value:
      read (buffer,*,iostat=status) r
      if ( status /= 0 ) then
        write (gol,'("while reading real :")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("  value  : ",a)') trim(buffer); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else
      ! some error ...
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
    
    ! ok
    status = 0

  end subroutine ReadRc_r1


  subroutine ReadRc_l( rcfile, key, l, status, default )

    use module_go_print, only : gol, goErr

    ! --- in/out ----------------------------

    type(TrcFile), intent(in)                   ::  rcfile
    character(len=*), intent(in)                ::  key
    logical, intent(out)                        ::  l
    integer, intent(out)                        ::  status
    
    logical, intent(in), optional               ::  default
    
    ! --- const ----------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadRc_l'

    ! --- local -----------------------------

    character(len=buflen)     ::  buffer

    ! --- begin -----------------------------

    ! search key line in rcfile:
    call ReadRcItem( rcfile, key, buffer, status )
    if ( status < 0 ) then
      ! not found; set to default or leave with error:
      if ( present(default) ) then
        l = default
      else
        write (gol,'("key not found and no default specified ...")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else if ( status == 0 ) then
      ! key found; set value:
      read (buffer,*,iostat=status) l
      if ( status /= 0 ) then
        write (gol,'("while reading logical :")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("  value  : ",a)') trim(buffer); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else
      ! some error ...
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
    
    ! ok
    status = 0

  end subroutine ReadRc_l


  ! ***


  subroutine ReadRc_s( rcfile, key, s, status, default )
  
    use module_go_print, only : gol, goErr

    ! --- in/out ----------------------------

    type(TrcFile), intent(in)                   ::  rcfile
    character(len=*), intent(in)                ::  key
    character(len=*), intent(out)               ::  s
    integer, intent(out)                        ::  status
    
    character(len=*), intent(in), optional      ::  default
    
    ! --- const ----------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadRc_s'

    ! --- local -----------------------------

    character(len=buflen)     ::  buffer

    ! --- begin -----------------------------

    ! search key line in rcfile:
    call ReadRcItem( rcfile, key, buffer, status )
    if ( status < 0 ) then
      ! not found; set to default or leave with error:
      if ( present(default) ) then
        s = trim(default)
      else
        write (gol,'("key not found and no default specified ...")'); call goErr
        write (gol,'("  rcfile : ",a)') trim(rcfile%fname); call goErr
        write (gol,'("  key    : ",a)') trim(key); call goErr
        write (gol,'("in ",a)') rname; call goErr; status=1; return
      end if
    else if ( status == 0 ) then
      ! key found; set value:
      s = trim(buffer)
    else
      ! some error ...
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
    
    ! ok
    status = 0

  end subroutine ReadRc_s



end module module_rc
