!
!
!
SUBROUTINE WELLFILE_LON
 use filenames
 use version
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
 WRITE (*,9000) TRIM(DBXLName)//" "//TRIM(VersionNumber)//"  "//datestr
 write(*,*) "  (1) Create NetpathXL file from a .lon file."
 write(*,*) "  (2) Open existing NetpathXL file." 
 write(*,*) "  (3) Create new NetpathXL file."
 write(*,*) "  (4) Exit program"
 write(*,'(/A,$)') "Select an option> "

 READ (*,'(a)') yn
 IF (yn.EQ.'1') then
    status = fileopen_db(root, path, "old_lon")
    if (status == .false.) then
		write(*,*) "File open failed."
		write(*,*) "Press enter to continue."
		READ (*,'(a)') yn
		goto 10
	endif 
	CALL RDDB
	CALL NewExcel
	CALL DB2XL
	result = CheckOldExcel()	     
	CALL SaveExcel
	CALL VisibleExcel(.TRUE.)
	CALL cleanup_com(.FALSE.)
 else if (yn .eq. '2') then
    status = fileopen_db(root, path, "old_xls")
    if (status == .false.) then
		write(*,*) "File open failed."
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
	else
		write(*,*) "File open failed."
		write(*,*) "Press enter to continue."
		READ (*,'(a)') yn
		goto 10
	endif
 else if (yn .eq. '4') then
    return
 else
    goto 10
 endif
 if (status == 0) goto 10
 result = CHANGEDIRQQ (path)

 RETURN
9000 FORMAT (1x, A, /)
END SUBROUTINE WELLFILE_LON