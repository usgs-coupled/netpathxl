!
!
!
SUBROUTINE CLPART
  USE IFQWIN
  implicit none
  !
  ! The screen is cleared from the cursor 3 lines down. Works for VT100
  !
  !      INTRINSIC CHAR
  !      WRITE (*,'(a)') CHAR(27)//'[B'//CHAR(27)//'[B'//CHAR(27)//'[B'//
  !     $     CHAR(27)//'[K'//CHAR(27)//'[A'//CHAR(27)//'[K'//CHAR(27)//
  !     $     '[A'//CHAR(27)//'[K'//CHAR(27)//'[A'//CHAR(27)//'[K'//
  !     $     CHAR(27)//'[A'
  INTEGER(2) top, left, bottom, right
  TYPE (rccoord) textpos
  TYPE (rccoord) curpos

  CALL GETTEXTPOSITION (textpos)
  !  Save text window position
  CALL GETTEXTWINDOW (top, left, bottom, right)
  CALL SETTEXTWINDOW(textpos.row,textpos.col,textpos.row+int2(2),right)

  CALL clearscreen($GWINDOW)
  CALL SETTEXTWINDOW (top, left, bottom, right)
  CALL SETTEXTPOSITION (textpos.row, textpos.col, curpos)

  RETURN
END SUBROUTINE CLPART
!
!
!
SUBROUTINE moverelative(i)
  USE IFQWIN
  implicit none
  integer i
  !
  ! The screen is cleared from the cursor 3 lines down. Works for VT100
  !
  !      INTRINSIC CHAR
  !      WRITE (*,'(a)') CHAR(27)//'[B'//CHAR(27)//'[B'//CHAR(27)//'[B'//
  !     $     CHAR(27)//'[K'//CHAR(27)//'[A'//CHAR(27)//'[K'//CHAR(27)//
  !     $     '[A'//CHAR(27)//'[K'//CHAR(27)//'[A'//CHAR(27)//'[K'//
  !     $     CHAR(27)//'[A'
  INTEGER(2) top, left, bottom, right
  TYPE (rccoord) textpos
  TYPE (rccoord) curpos

  CALL GETTEXTPOSITION (textpos)
  CALL SETTEXTPOSITION (textpos.row + i, textpos.col, curpos)


  RETURN
END SUBROUTINE moverelative
!
!
!
SUBROUTINE CLS
  USE IFQWIN
  USE USER32
  implicit none
  integer i
  !
  ! The screen is cleared.  Works for VT100
  !
  !      CHARACTER*1 esc
  !      INTRINSIC CHAR
  !      esc = CHAR(27)
  !      WRITE (*,*) esc//'[2J'//esc//'[H'
  !      RETURN
  !      END
 
  CALL CLEARSCREEN($GWINDOW )
  i = SENDMESSAGE(GETHWNDQQ(GETACTIVEQQ()), WM_VSCROLL , SB_TOP, 0)
  return
end SUBROUTINE CLS

!
!
!
SUBROUTINE HOME
  USE IFQWIN
  implicit none
  !
  ! The screen is cleared.  Works for VT100
  !
  !      CHARACTER*1 esc
  !      WRITE (*,*) CHAR(27)//'[H'
  !      return
  !      end
  TYPE (rccoord) curpos

  CALL SETTEXTPOSITION (INT2(1), INT2(1), curpos)
  RETURN
END SUBROUTINE HOME


!
!
!
SUBROUTINE POSCUR(I)
  implicit none
  !
  ! The cursor is positioned at the bottom of a screen of information.
  !   I = -1  Cursor is at bottom of main screen
  !
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER I, j, k
  EXTERNAL CLPART
  INTRINSIC CHAR
  !
  !      WRITE (*,*) CHAR(27)//'[H'
  call home
  k = I
  IF (I.EQ.-1) k = Ilength+Iflag(1)+2
  DO j = 1, k+4
     WRITE (*,*)
  enddo
  IF (I.EQ.-1) CALL CLPART
  RETURN
end SUBROUTINE POSCUR
!C
!C
!C
!      SUBROUTINE CLS_DB
!      INTRINSIC CHAR
!      WRITE (*,*) CHAR(27)//'[H'//CHAR(27)//'[J'
!      RETURN
!      END
!
!
!
SUBROUTINE POSCUR_DB
  USE IFQWIN
  implicit none
  !      CHARACTER*1 esc
  !      INTRINSIC CHAR
  !      esc = CHAR(27)
  !      WRITE (*,9000) esc//'[H'
  !      WRITE (*,'(a)') 
  !     $     esc//'[B'//esc//'[B'//esc//'[K'//esc//'[A'//esc//'[K'
  !     $     //esc//'[A'//esc//'[K'//esc//'[A'
  TYPE (rccoord) curpos
  call settextposition(int2(19), int2(1), curpos)
  call clpart

  RETURN
  ! 9000 FORMAT (1X,A,//////////////////)
END SUBROUTINE POSCUR_DB
!
!
!
subroutine font
  USE IFQWIN
  USE screen_parameters
  USE version
  implicit none
  !TYPE QWINFO
  !    INTEGER(2) TYPE  ! request type
  !    INTEGER(2) X     ! x coordinate for upper left
  !    INTEGER(2) Y     ! y coordinate for upper left
  !    INTEGER(2) H     ! window height
  !    INTEGER(2) W     ! window width
  ! END TYPE QWINFO

  LOGICAL(4)     xresult, status
  integer*2 ih, iw
  TYPE (qwinfo)  winfo
  TYPE (windowconfig) wc
  REAL fheight, fwidth, f
  character*80 string
  
    ! font height
    fheight = 16
    WRITE (*,"(' Enter font height (8-24, Default 16): ')")
    READ (*,'(G20.0)', err=30) f
    if (f .lt. 8.0 .or. f .gt. 24.0) goto 30
    fheight = f
30  continue
    ih = nint(fheight)

    ! text intensity
    text_dimmer = text_dimmer_default
    WRITE (*,"(' Enter font brightness (0.3 to 1.0, default ', f5.2, '): ')") text_dimmer_default
    READ (*,'(G20.0)', err=10) f
    if (f .lt. 0.3 .or. f .gt. 1.0) goto 10
    text_dimmer = f

    ! background intensity
10  continue
    bk_dimmer = bk_dimmer_default
    WRITE (*,"(' Enter background brightness (0.0 to 1.0, default ', f5.2, '): ')") bk_dimmer_default
    READ (*,'(A)', err=20) string
    if (string == " ") goto 20
    read(string, "(G20.0)", err = 20) f
    if (f .lt. 0.0 .or. f .gt. 1.0) goto 20
    bk_dimmer = f

20  continue
    fwidth = real(ih)*0.6
    iw = nint(fwidth)
    wc%numxpixels  = -1
    wc%numypixels  = -1
    wc%numtextcols = -1
    wc%numtextrows = 500
    wc%mode = QWIN$SCROLLDOWN
    wc%title= TRIM(VersionNumber)//char(0)
    wc%fontsize = QWIN$EXTENDFONT
    wc%extendfontname = "Lucida Console"//char(0)
    wc%extendfontsize = ih + iw*65536 ! height is ih, width is iw
    wc%extendfontattributes = QWIN$EXTENDFONT_NORMAL
    status = SETWINDOWCONFIG(wc)  ! attempt to set configuration with above values
    ! if attempt fails, set with system estimated values
    if (.NOT.status) status = SETWINDOWCONFIG(wc)
    
 ! need display cursor after setwindowconfig
 call set_color_np
 xresult =     DISPLAYCURSOR ($GCURSORON)
 
return
end subroutine font

!
!
!
integer function fileopen_db(Dfile, path, typefile, suffix)
  USE IFwin
  implicit none
 INTEGER LENS 
 EXTERNAL LENS

  ! Declare structure used to pass and receive attributes
  !
  type(T_OPENFILENAME) ofn

  ! Declare filter specification.  This is a concatenation of
  ! pairs of null-terminated strings.  The first string in each pair
  ! is the file type name, the second is a semicolon-separated list
  ! of file types for the given name.  The list ends with a trailing
  ! null-terminated empty string.
  !
  character*(*),parameter :: filter_spec_lon = "DB files"C//"*.lon"C  
  character*(*),parameter :: filter_spec_xls = "Excel files"C//"*.xlsx;*.xls"C//"All files"C//"*.*"C
  
  ! Declare string variable to return the file specification.
  ! Initialize with an initial filespec, if any - null string
  ! otherwise
  !
  character*(*) typefile
  character*(*) suffix
  character*512 :: file_spec = ""C
  character*(*) dfile, path
  integer status,ilen, i, strcmp_nocase
  ofn%lStructSize = SIZEOF(ofn)
  ofn%hwndOwner = NULL  
  ! For non-console applications,
  ! set this to the Hwnd of the
  ! Owner window.  For QuickWin
  ! and Standard Graphics projects,
  ! use GETHWNDQQ(QWIN$FRAMEWINDOW)
  !
  ofn%hInstance = NULL  ! For Win32 applications, you
  ! can set this to the appropriate
  ! hInstance
  !

  ofn%lpstrCustomFilter = NULL
  ofn%nMaxCustFilter = 0
  ofn%nFilterIndex = 1 ! Specifies initial filter value
  ofn%lpstrFile = loc(file_spec)
  ofn%nMaxFile = sizeof(file_spec)
  ofn%nMaxFileTitle = 0
  ofn%lpstrInitialDir = NULL  ! Use Windows default directory
  ofn%lpstrTitle = loc(""C)
  ofn%lpstrDefExt = loc("txt"C)
  ofn%lpfnHook = NULL
  ofn%lpTemplateName = NULL

  ! Call GetOpenFileName and check status
  ! 
  if (typefile == "old_lon") then
     ofn%lpstrFilter = loc(filter_spec_lon)
     ofn%Flags = OFN_FILEMUSTEXIST
     status = GetOpenFileName(ofn)
	 suffix = '.lon'
  else if (typefile == "old_xls") then
     ofn%lpstrFilter = loc(filter_spec_xls)
     ofn%Flags = OFN_FILEMUSTEXIST
     status = GetOpenFileName(ofn) 
	 suffix = ".xls"
  else if (typefile == "new_xls") then
     ofn%lpstrFilter = loc(filter_spec_xls)
	 ofn%Flags = ior(OFN_PATHMUSTEXIST, OFN_OVERWRITEPROMPT)
     status = GetSaveFileName(ofn)
	 suffix = ".xls"
  endif
  if (status .eq. 0) then
     dfile = ' '
     path = ' '
  else
     ! Get length of file_spec by looking for trailing NUL
     ilen = INDEX(file_spec,CHAR(0))
	 if (ilen .gt. len(path)) then
	    write(*,*) "Path name is greater than ",len(path)," characters."
		write(*,*) "Stopping"
		stop
	 endif
     ! type *,'Filespec is ',file_spec(1:ilen-1)
     ! Example of how to see if user said "Read Only"
     !  
     path = file_spec(1:ilen-1)
     do i = LEN(path), 1, -1
        if (path(i:i) == '\') then
           dfile = path(i+1:len(path))
           path(i:len(path)) = ' '
           exit
        endif
     enddo
     do i = LEN(dfile), 1, -1
         if (dfile(i:i) .eq. '.') then
             suffix = dfile(i+1:LEN(dfile))
             dfile = dfile(1:i-1)
             exit
         endif
     enddo
     if (i .eq. 1) then
	    write(*,*) "No suffix (xls, xlsx, lon) found for file ", len(path)
		write(*,*) "Stopping"
		stop  
     endif
     
     !do i = LEN(dfile) - 3, 1, -1
     !   if (strcmp_nocase(dfile(i:i+3),suffix) .eq. 0) then
     !      dfile = dfile(1:i-1)
     !      exit
     !   endif
     !enddo
  endif
  fileopen_db = status
  return
end function fileopen_db
