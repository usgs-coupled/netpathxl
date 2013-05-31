!
!
!
SUBROUTINE WELLFILE_PAT(initial)
  USE max_size
  use filenames
  use version
  implicit none
  integer status
  logical xresult, CheckOldExcel, CHANGEDIRQQ
  integer fileopen_np
  character*20 yn
  !
  ! The initial well data file to be used is selected here.  The program
  ! terminates if no well files have been prepared.
  !
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER db_Dbsfg, db_Idefault, db_Iu, db_Nwlls, db_Totwell, db_Tot
  COMMON /INT4DB/ db_Dbsfg(MAXWELLS,45), db_Idefault(5), db_Iu(MAXWELLS,4), db_Nwlls,  &
	   db_Totwell, db_Tot(MAXWELLS)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50), &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  CHARACTER*140 files(100), fileone
  LOGICAL isthere
  INTEGER LENS, nfiles, i, ij, icount, j
  CHARACTER UPCS80*80, line*80, ans*1, UPCS*1
  EXTERNAL CLS, RDPATH, WELLS, INITVALS, MODELS, UPCS80, LENS, UPCS
  integer fileopen_db
  external fileopen_db, CheckOldExcel
  logical initial
  !
   icase = 0
10 continue 
	CALL CLS
	WRITE (*,9000) TRIM(ProgramName)//" "//TRIM(VersionNumber)//"   "//datestr
	write(*,*)  "   (1) Open NetpathXL file"
	write(*,*)  "   (2) Open .pat file"
	write(*,*)  "   (3) Quit"
	write(*,'(/A,$)') " Select an option> "
	READ (*,'(a)') yn
	IF (yn.EQ.'1') then
		status = fileopen_db(root, path, "old_xls", file_suffix)
		if (status .eq. 0) then
			write(*,*) "Excel file not opened."
			write(*,*) "Press enter to continue."
			READ (*,'(a)') yn
			goto 10
		endif
		CALL OldExcel
		xresult = CheckOldExcel()
		if (xresult == .false.) then
			write(*,*) "File is not a NetpathXL file."
			write(*,*) "One of the cells A1, A7-AU7 did not match template."
			write(*,*) "Press enter to continue."
			READ (*,'(a)') yn
			call cleanup_com(.TRUE.)
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


    xresult = CHANGEDIRQQ (path)
    
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
        CALL MODELS214
     END IF
  END IF

  return

9000 FORMAT (1x, A, /)
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
  USE max_size
    USE filenames
    USE IFwin
    implicit none
    DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
    COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40), &
       Disalong, Usera(5)
    INTEGER LENS 
    EXTERNAL LENS
    logical xresult, CHANGEDIRQQ, CheckOldExcel
    integer fileopen_np
    character*20 yn  
    integer fileopen_db
    external fileopen_db, CheckOldExcel
    INTEGER db_Dbsfg, db_Idefault, db_Iu, db_Nwlls, db_Totwell, db_Tot
    COMMON /INT4DB/ db_Dbsfg(MAXWELLS,45), db_Idefault(5), db_Iu(MAXWELLS,4), db_Nwlls,  &
    db_Totwell, db_Tot(MAXWELLS)
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
    character*(*),parameter :: filter_spec_xls = "Excel files"C//"*.xls;*.xlsx"C//"All files"C//"*.*"C
    double precision dbsave(0:MAXWELLS,0:50)

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
    do i = 0, MAXWELLS
        do j = 44,49
            if (j == 48) cycle
            dbsave(i, j) = dbdata(i, j)
        enddo
    enddo    

	CALL OldExcel
	xresult = CheckOldExcel()
    if (xresult == .false.) then
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
	do i = 0, MAXWELLS
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
