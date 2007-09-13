!
!
!
SUBROUTINE CLPART
  USE DFLIB
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
  USE DFLIB
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
  USE DFLIB
  implicit none!
  ! The screen is cleared.  Works for VT100
  !
  !      CHARACTER*1 esc
  !      INTRINSIC CHAR
  !      esc = CHAR(27)
  !      WRITE (*,*) esc//'[2J'//esc//'[H'
  !      RETURN
  !      END

  CALL CLEARSCREEN($GCLEARSCREEN)
  return
end SUBROUTINE CLS

!
!
!
SUBROUTINE HOME
  USE DFLIB
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
  implicit none!
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
END SUBROUTINE POSCUR
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
 USE DFLIB
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



subroutine configdb
 USE DFLIB
 implicit none	
 !TYPE QWINFO
 !    INTEGER(2) TYPE  ! request type
 !    INTEGER(2) X     ! x coordinate for upper left
 !    INTEGER(2) Y     ! y coordinate for upper left
 !    INTEGER(2) H     ! window height
 !    INTEGER(2) W     ! window width
 ! END TYPE QWINFO

 LOGICAL(4)     result
 TYPE (qwinfo)  winfo
 INTEGER*2 i1, i2, i3, i4

 ! Maximize frame window
 winfo%TYPE = QWIN$SET
 winfo%x    = 0
 winfo%y    = 0
 winfo%h    = 600
 winfo%w    = 800
 result =     SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)
 winfo%TYPE = QWIN$RESTORE
 result =     SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)

 result = DISPLAYCURSOR ($GCURSORON)
 i1 = 1
 i2 = 1
 i3 = 35
 i4 = 80
 CALL SETTEXTWINDOW (i1, i2, i3, i4) 
 return
end subroutine configdb
!
!
!
SUBROUTINE WELLFILE_LON
 use filenames
 implicit none
 !
 ! The well data file to be used is selected here.
 !
 CHARACTER*80 files(100), line, UPCS80
 CHARACTER*10 yn
 LOGICAL isthere
 CHARACTER*1 UPCS
 !
 INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
 COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
 logical*4 result 
 !
 logical*4 CHANGEDIRQQ
 logical CheckOldExcel
 external CheckOldExcel
 INTEGER LENS, nfiles, i, ij, icount, fileopen_db, status
 EXTERNAL CLS, UPCS, LENS, UPCS80

 !
 icase = 0
10 continue 
 CALL CLS
 WRITE (*,9000)
 write(*,*) "Select an option:"
 write(*,*) "  (1) Create NetpathXL file from a .lon file."
 write(*,*) "  (2) Open existing NetpathXL file." 
 write(*,*) "  (3) Create new NetpathXL file."
 write(*,*) "  (4) Exit program"
 write(*,'(A,$)') "Select an option> "

 READ (*,'(a)') yn
 IF (yn.EQ.'1') then
    status = fileopen_db(root, path, "old_lon")
	CALL RDDB
	CALL NewExcel
	CALL DB2XL
	result = CheckOldExcel()	     
	CALL SaveExcel
	CALL VisibleExcel(.TRUE.)
	CALL cleanup_com(.FALSE.)
 else if (yn .eq. '2') then
    status = fileopen_db(root, path, "old_xls")
	CALL OldExcel
	result = CheckOldExcel()
	if (result == .false.) then
		write(*,*) "File is not a NetpathXL file."
		write(*,*) "One of the cells A1, A7-AU7 did not match template."
		write(*,*) "Press enter to continue."
		READ (*,'(a)') yn
		goto 10
	endif
	CALL VisibleExcel(.TRUE.)
	CALL cleanup_com(.FALSE.)

 else if (yn .eq. '3') then
    status = fileopen_db(root, path, "new_xls")
	if (status) then
		CALL NewExcel
		result = CheckOldExcel()
		CALL SaveExcel
		CALL VisibleExcel(.TRUE.)
		CALL cleanup_com(.FALSE.)
	endif
 else if (yn .eq. '4') then
    return
 else
    goto 10
 endif
 if (status == 0) goto 10
 result = CHANGEDIRQQ (path)

 RETURN
9000 FORMAT (' DB - Version 2.14.1 (September 14, 2007)',/,&
      ' ----------------')
END SUBROUTINE WELLFILE_LON
!	
!		
!		
integer function fileopen_db(Dfile, path, typefile)
  use dfwin
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
  character*(*),parameter :: filter_spec_xls = "Excel files"C//"*.xls"C
  
  ! Declare string variable to return the file specification.
  ! Initialize with an initial filespec, if any - null string
  ! otherwise
  !
  character*(*) typefile
  character*10 suffix
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
     do i = LEN(dfile) - 3, 1, -1
        if (strcmp_nocase(dfile(i:i+3),suffix) .eq. 0) then
           dfile = dfile(1:i-1)
           exit
        endif
     enddo
  endif
  fileopen_db = status
  return
end function fileopen_db
