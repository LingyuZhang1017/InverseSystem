!###############################################################################
!
! NAME
!   GO_String - general objects for character strings
!
! PROCEDURES
!
!   call goSplitLine( 'ab#cd', s1, '#', s2 )
!
!     Splits a string like 'ab#cd' at the first '#', and returns
!     the leading part in s1, and the rest in s2.
!     One or both of s1 and s2 might be empty.
!
!   call goReadFromLine( line, x [,sep=','] )
!    
!     Splits the string "line" at the first komma (or the character
!     specified by the optional argument "sep"), 
!     fills the integer|real|logical|character variable "x" with the
!     leading part, and returns the remainder in "line".
!
!   s = gonum2str( i [,fmt='(i6)'] )
!
!     Returns a 6-character string with the representation of the
!     integer value i in the first characters.
!     Use 
!       trim(gonum2str(i))
!     to obtain a string of smallest size.
!
!   s = gonum2str( r [,fmt='(f7.4)'] )
!
!     Returns a 6-character string with the representation of the
!     integer value i in the first characters.
!     Use 
!       trim(gonum2str(i))
!     to obtain a string of smallest size.
!
!   s2 = goUpCase( s1 )
!   s2 = goLoCase( s1 )
!
!     Convert to upper or lower case
!
!   call goTab2Space( s )
!
!     Replaces each tab-character in s by a space.
!
!###############################################################################
!
!#define IF_ERROR_RETURN(action) if (status/=0) then; write (gol,'("in ",a)') rname; call goErr; action; return; end if
!
!###############################################################################

module module_go_string

  implicit none

  ! --- in/out -----------------------------

  private

  public  ::  goSplitLine
  public  ::  goReadFromLine
  public  ::  goVarValue
  public  ::  goNum2str
  public  ::  goUpCase, goLoCase
  public  ::  goWriteKeyNum
  public  ::  goTab2Space


  ! --- const ---------------------------------
  
  character(len=*), parameter  ::  mname = 'GO_String'

  
  ! --- interfaces -------------------------------------

  interface goReadFromLine
    module procedure ReadFromLine_i
    module procedure ReadFromLine_r
    module procedure ReadFromLine_l
    module procedure ReadFromLine_s
  end interface

  interface goVarValue
    module procedure goVarValue_s
    module procedure goVarValue_i
    module procedure goVarValue_l
  end interface

  interface goNum2str
    module procedure num2str_i
    module procedure num2str_r
  end interface

  interface goUpCase
    module procedure UpCase
  end interface

  interface goLoCase
    module procedure LoCase
  end interface

  interface goWriteKeyNum
    module procedure WriteKeyNum
  end interface


contains


  !**********************************************************************


  subroutine goSplitLine( line, s1, c, s2, status )

    ! --- in/out ----------------------------

    character(len=*), intent(in)      ::  line
    character(len=*), intent(out)     ::  s1
    character(len=1), intent(in)      ::  c
    character(len=*), intent(out)     ::  s2
    integer, intent(out)              ::  status

    ! --- local -----------------------------

    integer                     ::  l, l1,l2, pos
    character(len=len(line))    ::  s

    ! --- begin -----------------------------

    s = line
    l = len_trim(s)

    pos = scan(s,c)
    if (pos<1) then
      ! s='abcd'  -> s1='abcd', s2=''
      !call AdjustLeft( s1, s(1:l) )
      s1 = AdjustL( s(1:l) )
      s2 = ''
    else if (pos==1) then
      ! s=',' or s=',abcd'  ->  s1='', s2='' or 'abcd'
      s1 = ''
      if (l==1) then
        ! s=','
        s2 = ''
      else
        !call AdjustLeft( s2, s(pos+1:l) )
        s2 = AdjustL( s(pos+1:l) )
      end if
    else
      ! s='ab,' or s='ab,cd'
      !call AdjustLeft( s1, s(1:pos-1) )
      s1 = AdjustL( s(1:pos-1) )
      if (pos==l) then
        ! s='ab,'
        s2 = ''
      else
        ! s='ab,cd'
        !call AdjustLeft( s2, s(pos+1:l) )
        s2 = AdjustL( s(pos+1:l) )
      end if
    end if
    
    ! ok
    status = 0

  end subroutine goSplitLine


  ! ***


!  subroutine AdjustLeft( t, s )
!
!    ! --- in/out ----------------------
!
!    character(len=*), intent(out)   ::  t
!    character(len=*), intent(in)    ::  s
!
!    ! --- local -----------------------
!
!    integer     ::  is,ls, lt
!
!    ! --- local -----------------------
!
!    lt = len(t)
!
!    ls = len_trim(s)
!    if (ls==0) then
!      t = ''
!    else
!      is = 0
!      do
!        is = is + 1
!        if (s(is:is)/=' ') exit
!        if (is==ls) exit
!      end do
!      if (ls-is+1 > lt) then
!        print *, 'AdjustLeft : error : target is to small ', &
!           '(',lt,') to contain "'//s//'".'
!        stop
!      end if
!      t = s(is:ls)
!    end if
!
! end subroutine AdjustLeft


 ! *****************************************************


  subroutine ReadFromLine_i( s, i, status, sep )

    use module_go_print, only : gol, goPr, goErr

    ! --- in/out --------------------------

    character(len=*), intent(inout)         ::  s
    integer, intent(inout)                  ::  i
    integer, intent(out)                    ::  status
    character(len=1), intent(in), optional  ::  sep

    ! --- const ----------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadFromLine_i'
    
    ! --- local ----------------------------

    character(len=len(s))     ::  s1, s2
    character(len=1)          ::  thesep

    ! --- begin ----------------------------
    
    ! default seperation character provided as argument:
    thesep = ','
    if (present(sep)) thesep = sep

    call goSplitLine( s, s1, thesep, s2, status )
 !   IF_ERROR_RETURN(status=1)

    if (len_trim(s1) == 0) then
      !write (gol,'("empty field in input : `",a,"`")') trim(s); call goErr
      !write (gol,'("in ",a)') rname; call goErr; status=1; return
      i = 0 ; return
    end if

    read (s1,*,iostat=status) i
    if ( status /= 0 ) then
      write (gol,'(a," while reading integer out of `",a,"`")') trim(s); call goErr
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
   
    ! return remainder
    s = s2
    
    ! ok
    status = 0

  end subroutine ReadFromLine_i


  ! ***


  subroutine ReadFromLine_r( s, r, status, sep )

    use module_go_print, only : gol, goPr, goErr

    ! --- in/out --------------------------

    character(len=*), intent(inout)         ::  s
    real, intent(out)                       ::  r
    integer, intent(out)                    ::  status

    character(len=1), intent(in), optional  ::  sep

    ! --- const ------------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadFromLine_r'
    
    ! --- local ----------------------------

    character(len=len(s))     ::  s1, s2
    character(len=1)          ::  thesep

    ! --- begin ----------------------------

    ! default seperation character provided as argument:
    thesep = ','
    if (present(sep)) thesep = sep

    call goSplitLine( s, s1, thesep, s2, status )
  !  IF_ERROR_RETURN(status=1)

    if ( len_trim(s1) == 0 ) then
      !print *, 'ReadFromLine_r : warning : empty field in '''//trim(s)//''''
      r = 0.0
    end if
    
    read (s1,*,iostat=status) r
    if ( status /= 0 ) then
      write (gol,'("error while reading real out `",a,"`")') trim(s); call goErr
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if

    ! return remainder
    s = s2

    ! ok
    status = 0

  end subroutine ReadFromLine_r


  ! ***


  subroutine ReadFromLine_l( s, l, status, sep )

    use module_go_print, only : gol, goPr, goErr

    ! --- in/out --------------------------

    character(len=*), intent(inout)         ::  s
    logical, intent(out)                    ::  l
    integer, intent(out)                    ::  status

    character(len=1), intent(in), optional  ::  sep

    ! --- const ------------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadFromLine_l'
    
    ! --- local ----------------------------

    character(len=len(s))     ::  s1, s2
    character(len=1)          ::  thesep

    ! --- begin ----------------------------

    ! default seperation character provided as argument:
    thesep = ','
    if (present(sep)) thesep = sep

    call goSplitLine( s, s1, thesep, s2, status )
    !IF_ERROR_RETURN(status=1)

    read (s1,*,iostat=status) l
    if ( status /= 0 ) then
      write (gol,'("while reading logical out `",a,"`")') trim(s); call goErr
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if

    ! return remainder
    s = s2

    ! ok
    status = 0

  end subroutine ReadFromLine_l


  ! ***


  subroutine ReadFromLine_s( s, ss, status, sep )

    use module_go_print, only : gol, goPr, goErr

    ! --- in/out --------------------------

    character(len=*), intent(inout)         ::  s
    character(len=*), intent(out)           ::  ss
    integer, intent(out)                    ::  status

    character(len=1), intent(in), optional  ::  sep

    ! --- const ------------------------------
    
    character(len=*), parameter  ::  rname = mname//'/ReadFromLine_s'
    
    ! --- local ----------------------------

    character(len=len(s))     ::  s1, s2
    character(len=1)          ::  thesep
    integer                   ::  l, ll
    integer                   ::  iostat

    ! --- begin ----------------------------

    ! default seperation character provided as argument:
    thesep = ','
    if (present(sep)) thesep = sep

    call goSplitLine( s, s1, thesep, s2, status )
    !IF_ERROR_RETURN(status=1)
    l = len_trim(s1)
    ll = len(ss)
    if ( ll < l ) then
      write (gol,'("size of output string not sufficient:")'); call goErr
      write (gol,'("  first part of input : ",a )') trim(s1) ; call goErr
      write (gol,'("  output length       : ",i4)') ll       ; call goErr
      write (gol,'("in ",a)') rname; call goErr; status=1; return
    end if
    ss = trim(s1)

    ! return remainder
    s = s2

    ! ok
    status = 0

  end subroutine ReadFromLine_s


  ! *****************************************************
  
  !
  !  Read value from line:
  !
  !    bb = 'default'
  !    call goVarValue( 'aa=1;bb=xyz;cc=U123', ';', 'bb', '=', bb, status )
  !
  !  Return status:
  !    <0  :  variable not found, val remains the same
  !     0  :  variable found, val reset
  !    >0  :  error
  !
  
  subroutine goVarValue_s( line, sep, var, is, val, status )
  
    use module_go_print, only : gol, goPr, goErr

    ! --- in/out ---------------------------------
    
    character(len=*), intent(in)     ::  line
    character(len=1), intent(in)     ::  sep
    character(len=*), intent(in)     ::  var
    character(len=1), intent(in)     ::  is
    character(len=*), intent(inout)  ::  val
    integer, intent(out)             ::  status
    
    ! --- const ------------------------------
    
    character(len=*), parameter  ::  rname = mname//'/goVarValue_s'
    
    ! --- local ----------------------------------
    
    character(len=len(line))    ::  line2
    character(len=len(line))    ::  varval
    character(len=16)           ::  var2
    character(len=256)          ::  val2
    
    ! --- begin ----------------------------------
    
    ! copy of input line:
    line2 = line

    ! loop over var=val keys :
    do
      ! no keys left ? then leave
      if ( len_trim(line2) == 0 ) exit
      ! remove leading var=value from line2 :
      call goReadFromLine( line2, varval, status, sep=sep )
      !IF_ERROR_RETURN(status=1)
      ! split in var and value:
      call goSplitLine( varval, var2, is, val2, status )
      !IF_ERROR_RETURN(status=1)
      ! store ?
      if ( trim(var2) == trim(var) ) then
        val = trim(val2)
        status = 0 ; return
      end if
    end do

    ! not found ...
    status = -1
    
  end subroutine goVarValue_s


  ! ***
      

  subroutine goVarValue_i( line, sep, var, is, val, status )
  
    use module_go_print, only : gol, goPr, goErr

    ! --- in/out ---------------------------------
    
    character(len=*), intent(in)     ::  line
    character(len=1), intent(in)     ::  sep
    character(len=*), intent(in)     ::  var
    character(len=1), intent(in)     ::  is
    integer, intent(inout)           ::  val
    integer, intent(out)             ::  status
    
    ! --- const ------------------------------
    
    character(len=*), parameter  ::  rname = mname//'/goVarValue_i'
    
    ! --- local ----------------------------------
    
    character(len=len(line))    ::  line2
    character(len=len(line))    ::  varval
    character(len=16)           ::  var2
    character(len=256)          ::  val2
    
    ! --- begin ----------------------------------
    
    ! copy of input line:
    line2 = line

    ! loop over var=val keys :
    do
      ! no keys left ? then leave
      if ( len_trim(line2) == 0 ) exit
      ! remove leading var=value from line2 :
      call goReadFromLine( line2, varval, status, sep=sep )
      !IF_ERROR_RETURN(status=1)
      ! split in var and value:
      call goSplitLine( varval, var2, is, val2, status )
      !IF_ERROR_RETURN(status=1)
      ! store ?
      if ( trim(var2) == trim(var) ) then
        read (val2,'(i6)') val
        status = 0 ; return
      end if
    end do

    ! not found ...
    status = -1
    
  end subroutine goVarValue_i
    

  ! ***
      

  subroutine goVarValue_l( line, sep, var, is, val, status )
  
    use module_go_print, only : gol, goPr, goErr

    ! --- in/out ---------------------------------
    
    character(len=*), intent(in)     ::  line
    character(len=1), intent(in)     ::  sep
    character(len=*), intent(in)     ::  var
    character(len=1), intent(in)     ::  is
    logical, intent(inout)           ::  val
    integer, intent(out)             ::  status
    
    ! --- const ------------------------------
    
    character(len=*), parameter  ::  rname = mname//'/goVarValue_l'
    
    ! --- local ----------------------------------
    
    character(len=len(line))    ::  line2
    character(len=len(line))    ::  varval
    character(len=16)           ::  var2
    character(len=256)          ::  val2
    
    ! --- begin ----------------------------------
    
    ! copy of input line:
    line2 = line

    ! loop over var=val keys :
    do
      ! no keys left ? then leave
      if ( len_trim(line2) == 0 ) exit
      ! remove leading var=value from line2 :
      call goReadFromLine( line2, varval, status, sep=sep )
      !IF_ERROR_RETURN(status=1)
      ! split in var and value:
      call goSplitLine( varval, var2, is, val2, status )
      !IF_ERROR_RETURN(status=1)
      ! store ?
      if ( trim(var2) == trim(var) ) then
        read (val2,'(l1)') val
        status = 0 ; return
      end if
    end do

    ! not found ...
    status = -1
    
  end subroutine goVarValue_l
    

  ! *****************************************************

  !---
  ! NAME
  !   gonum2str - prints number into character string
  !
  ! INTERFACE
  !   character(len=20) function gonum2str( x, fmt )
  !     integer [or real]  , intent(in)            ::  x
  !     character(len=*)   , intent(in), optional  ::  fmt
  !
  ! ARGUMENTS
  !   x
  !     Number to be converted.
  !   fmt
  !     Optional format, following the formats provided
  !     to the 'write' command.
  !     Default values:
  !
  !       type  x             fmt         example  (_ is space)
  !       ------------------  ----------  ---------------------
  !       integer             '(i6)'      123___
  !       real                '(g10.3)'   -1.073e-12
  !
  ! CHANGES
  !   01/09/1999  Arjo Segers
  !   7 Nov 2006  Andy Jacobson - added num2str_r
  !---

  character(len=6) function num2str_i( i, fmt )

    ! --- in/out ----------------------

    integer, intent(in)                     ::  i
    character(len=*), intent(in), optional  ::  fmt

    ! --- local -----------------------

    character(len=6)   ::  s

    ! --- begin -----------------------

    if (present(fmt)) then
      write (s,fmt=fmt) i
    else
      write (s,'(i6)') i
    end if
    num2str_i=adjustl(s)

  end function num2str_i

  character(len=12) function num2str_r( r, fmt )

    ! --- in/out ----------------------

    real, intent(in)                     ::  r
    character(len=*), intent(in), optional  ::  fmt

    ! --- local -----------------------

    character(len=12)   ::  s

    ! --- begin -----------------------

    if (present(fmt)) then
      write (s,fmt=fmt) r
    else
      write (s,'(g10.3)') r
    end if
    num2str_r=adjustl(s)

  end function num2str_r


  ! *** UpCase, LoCase ***


  function UpCase( s )

    ! --- in/out -----------------

    character(len=*), intent(in)    ::  s
    character(len=len(s))           ::  UpCase

    ! --- local ------------------

    integer         ::  i

    ! --- begin ------------------

    do i = 1, len_trim(s)
      select case (s(i:i))
        case ('a') ;  UpCase(i:i) = 'A'
        case ('b') ;  UpCase(i:i) = 'B'
        case ('c') ;  UpCase(i:i) = 'C'
        case ('d') ;  UpCase(i:i) = 'D'
        case ('e') ;  UpCase(i:i) = 'E'
        case ('f') ;  UpCase(i:i) = 'F'
        case ('g') ;  UpCase(i:i) = 'G'
        case ('h') ;  UpCase(i:i) = 'H'
        case ('i') ;  UpCase(i:i) = 'I'
        case ('j') ;  UpCase(i:i) = 'J'
        case ('k') ;  UpCase(i:i) = 'K'
        case ('l') ;  UpCase(i:i) = 'L'
        case ('m') ;  UpCase(i:i) = 'M'
        case ('n') ;  UpCase(i:i) = 'N'
        case ('o') ;  UpCase(i:i) = 'O'
        case ('p') ;  UpCase(i:i) = 'P'
        case ('q') ;  UpCase(i:i) = 'Q'
        case ('r') ;  UpCase(i:i) = 'R'
        case ('s') ;  UpCase(i:i) = 'S'
        case ('t') ;  UpCase(i:i) = 'T'
        case ('u') ;  UpCase(i:i) = 'U'
        case ('v') ;  UpCase(i:i) = 'V'
        case ('w') ;  UpCase(i:i) = 'W'
        case ('x') ;  UpCase(i:i) = 'X'
        case ('y') ;  UpCase(i:i) = 'Y'
        case ('z') ;  UpCase(i:i) = 'Z'
        case default
          UpCase(i:i) = s(i:i)
      end select
    end do

  end function UpCase


  ! ***


  function LoCase( s )

    ! --- in/out -----------------

    character(len=*), intent(in)   ::  s
    character(len=len(s))          ::  LoCase

    ! --- local ------------------

    integer         ::  i

    ! --- begin ------------------

    do i = 1, len_trim(s)
      select case (s(i:i))
        case ('A') ;  LoCase(i:i) = 'a'
        case ('B') ;  LoCase(i:i) = 'b'
        case ('C') ;  LoCase(i:i) = 'c'
        case ('D') ;  LoCase(i:i) = 'd'
        case ('E') ;  LoCase(i:i) = 'e'
        case ('F') ;  LoCase(i:i) = 'f'
        case ('G') ;  LoCase(i:i) = 'g'
        case ('H') ;  LoCase(i:i) = 'h'
        case ('I') ;  LoCase(i:i) = 'i'
        case ('J') ;  LoCase(i:i) = 'j'
        case ('K') ;  LoCase(i:i) = 'k'
        case ('L') ;  LoCase(i:i) = 'l'
        case ('M') ;  LoCase(i:i) = 'm'
        case ('N') ;  LoCase(i:i) = 'n'
        case ('O') ;  LoCase(i:i) = 'o'
        case ('P') ;  LoCase(i:i) = 'p'
        case ('Q') ;  LoCase(i:i) = 'q'
        case ('R') ;  LoCase(i:i) = 'r'
        case ('S') ;  LoCase(i:i) = 's'
        case ('T') ;  LoCase(i:i) = 't'
        case ('U') ;  LoCase(i:i) = 'u'
        case ('V') ;  LoCase(i:i) = 'v'
        case ('W') ;  LoCase(i:i) = 'w'
        case ('X') ;  LoCase(i:i) = 'x'
        case ('Y') ;  LoCase(i:i) = 'y'
        case ('Z') ;  LoCase(i:i) = 'z'
        case default
          LoCase(i:i) = s(i:i)
      end select
    end do

  end function LoCase
  
  
  ! ***
  
  
  subroutine WriteKeyNum( res, key, num )
  
    use module_go_print, only : gol, goPr, goErr

    ! --- in/out ------------------------------
    
    character(len=*), intent(out)    ::  res
    character(len=*), intent(in)     ::  key
    integer, intent(in)              ::  num
    
    ! --- local -------------------------------
    
    integer    ::  anum
    
    ! --- begin -------------------------------
    
    anum = abs(num)
    
    if ( anum <= 9 ) then
      write (res,'(a,i1)') trim(key), anum
    else if ( anum <= 99 ) then
      write (res,'(a,i2)') trim(key), anum
    else if ( anum <= 999 ) then
      write (res,'(a,i3)') trim(key), anum
    else if ( anum <= 9999 ) then
      write (res,'(a,i4)') trim(key), anum
    else if ( anum <= 99999 ) then
      write (res,'(a,i5)') trim(key), anum
    else
      write (res,'(a,i6)') trim(key), anum
    end if
    
  end subroutine WriteKeyNum
    

  ! ***
      

  subroutine goTab2Space( s )

    ! --- in/out -----------------

    character(len=*), intent(inout)    ::  s

    ! --- local ------------------

    integer         ::  pos

    ! --- begin ------------------

    do 
      pos = scan(s,char(9))
      if ( pos == 0 ) exit
      s(pos:pos) = ' '
    end do

  end subroutine goTab2Space


  ! ***
      

end module module_go_string

