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
  implicit none
  !
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
subroutine confignetpath
  USE IFQWIN
  implicit none
  !TYPE QWINFO
  !    INTEGER(2) TYPE  ! request type
  !    INTEGER(2) X     ! x coordinate for upper left
  !    INTEGER(2) Y     ! y coordinate for upper left
  !    INTEGER(2) H     ! window height
  !    INTEGER(2) W     ! window width
  ! END TYPE QWINFO

  LOGICAL(4)     result
  integer*2 i1, i2, i3, i4
  TYPE (qwinfo)  winfo

  ! Maximize frame window
  winfo%TYPE = QWIN$SET
  winfo%x    = 0
  winfo%y    = 0
  winfo%h    = 600
  winfo%w    = 800
  result =     SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)
  winfo%TYPE = QWIN$RESTORE
  result =     SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)
  result =     SETEXITQQ(QWIN$EXITNOPERSIST) 
  result =     DISPLAYCURSOR ($GCURSORON)
  i1 = 1
  i2 = 1
  i3 = 35
  i4 = 80
  CALL SETTEXTWINDOW (i1, i2, i3, i4)
  
  return
end subroutine confignetpath
!
!
!
SUBROUTINE WELLFILE_PAT(initial)
  use filenames
  use version
  implicit none
  integer status, result, CHANGEDIRQQ
  integer fileopen_np
  character*20 yn
  !
  ! The initial well data file to be used is selected here.  The program
  ! terminates if no well files have been prepared.
  !
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER db_Dbsfg, db_Idefault, db_Iu, db_Nwlls, db_Totwell, db_Tot
  COMMON /INT4DB/ db_Dbsfg(50,45), db_Idefault(5), db_Iu(50,4), db_Nwlls,  &
	   db_Totwell, db_Tot(50)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(50,50), &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(50), Nopha, Iedit, Iadd
  CHARACTER*140 files(100), fileone
  LOGICAL isthere
  INTEGER LENS, nfiles, i, ij, icount, j
  CHARACTER UPCS80*80, line*80, ans*1, UPCS*1
  EXTERNAL CLS, RDPATH, WELLS, INITVALS, MODELS, UPCS80, LENS, UPCS
  integer fileopen_db, CheckOldExcel
  external fileopen_db, CheckOldExcel
  logical initial
  !
   icase = 0
10 continue 
	CALL CLS
	WRITE (*,9000) versnam, datestr

	write(*,*)  "   (1) Open NetpathXL file"
	write(*,*)  "   (2) Open .pat file"
	write(*,*)  "   (3) Quit"
	write(*,'(/A,$)') "Select an option> "
	READ (*,'(a)') yn
	IF (yn.EQ.'1') then
		status = fileopen_db(root, path, "old_xls")
		if (status .eq. 0) then
			write(*,*) "Excel file not opened."
			write(*,*) "Press enter to continue."
			READ (*,'(a)') yn
			goto 10
		endif
		CALL OldExcel
		result = CheckOldExcel()
		if (result == .false.) then
			write(*,*) "File is not a NetpathXL file."
			write(*,*) "One of the cells A1, A7-AU7 did not match template."
			write(*,*) "Press enter to continue."
			READ (*,'(a)') yn
			goto 10
		endif 
		call xl2db
		call cleanup_com(.TRUE.)
		IF (db_NWlls .LE. 1) THEN
            WRITE(*,*) "Sorry, not enough wells for inverse modeling", db_NWlls
		    write(*,*) "Press enter to continue."
		    READ (*,'(a)') yn
            goto 10
        ENDIF  
		CALL RUNWATEQ(root, .false.)
		wfile = root
		fileone = root(1:lens(root))//'.pat'
		call rdpath(fileone)
		excel_file = .true. 
	else if (yn .eq. '2') then
		status = fileopen_np(wfile, path, fileone)
		if (status .ne. 1) then
			goto 10
		endif
		CALL RDPATH(fileone)
		IF (NWlls .LE. 1) THEN
            WRITE(*,*) "Sorry, not enough wells for inverse modeling", NWlls
			write(*,*) "Press enter to continue."
			READ (*,'(a)') yn
            goto 10
        ENDIF
        excel_file = .false.
	else if (yn .eq. '3') then
		if (initial) then
		    stop  
		else
		     return
		endif
	else
		goto 10
	endif


    result = CHANGEDIRQQ (path)
    
    ! Save for reread
    excel_filename = filename
    excel_path = path
    excel_root = root

  IF (Iedit.EQ.2) THEN
     CALL CLS
     WRITE (*,9035)
     READ (*,9040) ans
     Iedit = 0
     IF (UPCS(ans).EQ.'Y') THEN
        CALL WELLS
     ELSE
        CALL INITVALS(1)
        CALL MODELS
     END IF
  END IF

  return

9000 FORMAT (1x, A20, A30,/,&
       ' ----------------',/)
9005 FORMAT (I4,':  ',A40)
9010 FORMAT (/,' Enter number of file to use, or ''Q'' to quit:')
9015 FORMAT (/,' Enter number of file, or <ENTER> to see more', &
       ' files (''Q'' to quit): ')
9020 FORMAT (/,' Enter number of file to use or <ENTER> to keep ',A)
9025 FORMAT (/,' Enter number of file to use, ''M'' to see more ', &
       'choice, or <ENTER> to keep ',A)
9030 FORMAT (A80)
9035 FORMAT (' Do you want to keep the current model? <Enter for no>')
9040 FORMAT (A)
END SUBROUTINE WELLFILE_PAT
!
!
!
SUBROUTINE REREAD_EXCEL
    USE filenames
    USE IFwin
    implicit none
    DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
    COMMON /DP4   / C14dat(13), Dbdata(0:50,0:50), P(3), Delta(40), &
       Disalong, Usera(5)
    INTEGER LENS 
    EXTERNAL LENS
    integer result, CHANGEDIRQQ
    integer fileopen_np
    character*20 yn  
    integer fileopen_db, CheckOldExcel
    external fileopen_db, CheckOldExcel
    INTEGER db_Dbsfg, db_Idefault, db_Iu, db_Nwlls, db_Totwell, db_Tot
    COMMON /INT4DB/ db_Dbsfg(50,45), db_Idefault(5), db_Iu(50,4), db_Nwlls,  &
    db_Totwell, db_Tot(50)
    CHARACTER*140 fileone
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
    double precision dbsave(0:50,0:50)

    ! Declare string variable to return the file specification.
    ! Initialize with an initial filespec, if any - null string
    ! otherwise
    !
    character*10 suffix
    character*512 :: file_spec = ""C
    integer status,ilen, i, strcmp_nocase, j
    
    ! Save for reread
    filename = excel_filename
    path = excel_path 
    root = excel_root 

    ! Save user defined values
    ! 44: C-13 of CH4
    ! 45: C-13 of DOC
    ! 46: C-14 of CH4
    ! 47: C-14 of DOC
    ! Not user defined48: RS of DOC
    ! 49: User-entered RS of DOC
    do i = 0, 50
        do j = 44,49
            if (j == 48) cycle
            dbsave(i, j) = dbdata(i, j)
        enddo
    enddo    

	CALL OldExcel
	result = CheckOldExcel()
    if (result == .false.) then
	    write(*,*) "File is not a NetpathXL file."
	    write(*,*) "One of the cells A1, A7-AU7 did not match template."
	    write(*,*) "Press enter to continue."
	    READ (*,'(a)') yn
	    return
    endif 
	call xl2db
	call cleanup_com(.TRUE.)
	IF (db_NWlls .LE. 1) THEN
        WRITE(*,*) "Sorry, not enough wells for inverse modeling", db_NWlls
	    write(*,*) "Press enter to continue."
	    READ (*,'(a)') yn
        return
    ENDIF  
	CALL RUNWATEQ(root, .true.)
	fileone = root(1:lens(root))//'.pat'
	call rdpath(fileone)
	excel_file = .true. 
	do i = 0, 50
        do j = 44,49
            if (j == 48) cycle
            dbdata(i, j) = dbsave(i, j) 
        enddo
    enddo   
  return
END SUBROUTINE REREAD_EXCEL
!
!
!
integer function fileopen_np(wfile,path, fileone)
  USE IFwin
  implicit none
  integer strcmp_nocase
  ! Declare structure used to pass and receive attributes
  !
  type(T_OPENFILENAME) ofn

  ! Declare filter specification.  This is a concatenation of
  ! pairs of null-terminated strings.  The first string in each pair
  ! is the file type name, the second is a semicolon-separated list
  ! of file types for the given name.  The list ends with a trailing
  ! null-terminated empty string.
  !
  character*(*),parameter :: filter_spec = &
       "Netpath files"C//"*.pat"C


  ! Declare string variable to return the file specification.
  ! Initialize with an initial filespec, if any - null string
  ! otherwise
  !

  character*512 :: file_spec = ""C
  character*(*) wfile, path, fileone
  integer status,ilen, i
  ofn%lStructSize = SIZEOF(ofn)
  ofn%hwndOwner = NULL  ! For non-console applications,
  ! set this to the Hwnd of the
  ! Owner window.  For QuickWin
  ! and Standard Graphics projects,
  ! use GETHWNDQQ(QWIN$FRAMEWINDOW)
  !
  ofn%hInstance = NULL  ! For Win32 applications, you
  ! can set this to the appropriate
  ! hInstance
  !
  ofn%lpstrFilter = loc(filter_spec)
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
  ofn%Flags = OFN_FILEMUSTEXIST
  status = GetOpenFileName(ofn)

  if (status .eq. 0) then
     wfile = ' '
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
        if (path(i:i) .eq. '\') then
           wfile = path(i+1:len(path))
           path(i:len(path)) = ' '
           exit
        endif
     enddo
     fileone = wfile
     do i = LEN(wfile) - 3, 1, -1
        if (strcmp_nocase(wfile(i:i+3), '.pat') .eq. 0) then
           wfile = wfile(1:i-1)
           exit
        endif
     enddo
  endif
  fileopen_np = status
  return
end function fileopen_np
!
!
!
integer function fileopen_db(Dfile, path, typefile)
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
