PROGRAM DBXL
  USE max_size
  use filenames
  implicit none
  !
  !     Program DBXL Revision 2.14 June 12, 2005
  !
  !     Program DB (Data Base Editor) is used to enter chemical and isotopic
  !     data to be used by NETPATH.  The speciation portion is a subroutine
  !     to DB and is executed after <S>ave is selected.
  !     The speciation output can be examined within DB.  DB was written
  !     by Anthony Baughman, David A. Stanley, and Eric C. Prestemon,
  !     under the direction of L. Niel Plummer.
  !
  REAL Dbdata
  COMMON /DB    / Dbdata(MAXWELLS,45)
  INTEGER Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
  COMMON /INT4DB/ Dbsfg(MAXWELLS,45), Idefault(5), Iu(MAXWELLS,4), Nwlls, &
       Totwell, Tot(MAXWELLS)
  CHARACTER Wllnms*80, Address*40, Lat*40, Formation*17
  COMMON /CHAR1 / Wllnms(MAXWELLS), Address(MAXWELLS,5), Lat(MAXWELLS), Formation(MAXWELLS)
  INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  !
  EXTERNAL WELLFILE_LON
  !
  Wllnms(1) = '@(#)DBED - Data Base editor for chemical and isotopc data used by NETPATH.'
  Wllnms(1) = '@(#)DBED - Written by A. Baughman, D.A. Stanley, and E.C. Prestemon'
  Wllnms(1) = '@(#)DBED - Contact: h2osoft@usgs.gov'
  Wllnms(1) = '$Id: db.f 306 2005-05-13 22:44:42Z dlpark $'
  !
  ! --------------------------
  ! SYSTEM-DEPENDENT VARIABLES
  ! --------------------------
  ! Unit numbers:
  ! These are the unit numbers for files to be read and written.
  ! These numbers should all be different.
  ! For most systems, numbers between 7 and 15 will work.
  ! (This is true for Unix, DOS, and PRIMOS)
  Ir = 7
  Iw1 = 8
  Iw2 = 9
  Iex1 = 10
  Io1 = 11
  Iscr2 = 13
  !
  ! Case sensitivity of file names:
  ! Use 0 if your system does not distinguish between upper and
  ! lower case in file names. Use 1 if these are different to your
  ! system.
  ! DOS and PRIMOS treat upper and lower case in file names as
  ! identical. (0) Unix does not. (1)
  Icase = 1
  !
  ! End of system-dependent variables
  !
  CALL configdb
  CALL WELLFILE_LON

END PROGRAM DBXL
!
!
!
SUBROUTINE GETNO(INPT,RESULT,SIGFIG,ERROR)
  implicit none
  CHARACTER*10 INPT
  REAL RESULT, a
  INTEGER ERROR, SIGFIG, i, j
  !
  ERROR = 0
  IF (INPT.EQ.'          ' .OR. INPT(1:3).EQ.'***') THEN
     RESULT = 0.0
     SIGFIG = -1
  ELSE IF (INPT(1:1).EQ.'<') THEN
     READ (INPT(2:10),'(F9.0)',ERR=40) RESULT
     SIGFIG = -2
  ELSE
     READ (INPT,'(F10.0)',ERR=40) a
     DO i = 1, 10
        IF (INPT(i:i).EQ.'.') THEN
           DO j = 10, i, -1
              IF (INPT(j:j).NE.' ') GO TO 30
           enddo
           ERROR = ERROR+2
           GO TO 40
        END IF
     enddo
     j = i
30   RESULT = a
     SIGFIG = j-i
  END IF
  GO TO 50
40 ERROR = ERROR+1
  SIGFIG = -1
  RESULT = 0.0
50 RETURN
end subroutine GETNO
!
!
!
SUBROUTINE GETNO214(INPT,RESULT,SIGFIG,ERROR)
  implicit none
  CHARACTER*15 INPT
  REAL RESULT, a
  INTEGER ERROR, SIGFIG, i, j
  !
  ERROR = 0
  IF (INPT.EQ.'               ' .OR. INPT(1:3).EQ.'***') THEN
     RESULT = 0.0
     SIGFIG = -1
  ELSE IF (INPT(1:1).EQ.'<') THEN
     READ (INPT(2:15),'(F14.0)',ERR=40) RESULT
     SIGFIG = -2
  ELSE
     READ (INPT,'(F15.0)',ERR=40) a
     DO i = 1, 15
        IF (INPT(i:i).EQ.'.') THEN
           DO j = 15, i, -1
              IF (INPT(j:j).NE.' ') GO TO 30
           enddo
           ERROR = ERROR+2
           GO TO 40
        END IF
     enddo
     j = i
30   RESULT = a
     SIGFIG = j-i
  END IF
  GO TO 50
40 ERROR = ERROR+1
  SIGFIG = -1
  RESULT = 0.0
50 RETURN
end subroutine GETNO214
!
!
!
SUBROUTINE PUTDATA(I)
  USE max_size
  implicit none
  INTEGER I
  CHARACTER*80 line
  CHARACTER*16 words(45), carb(0:3), tempwd
  CHARACTER*11 units(0:4), tempun
  CHARACTER*10 mat
  CHARACTER*9 eleunits(0:8)
  CHARACTER*8 fname, tempvl
  CHARACTER*7 report(45)
  INTEGER iunits(45), ic, j, k, ioff, LENS, num(45)
  !
  REAL Dbdata
  COMMON /DB    / Dbdata(MAXWELLS,45)
  INTEGER Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
  COMMON /INT4DB/ Dbsfg(MAXWELLS,45), Idefault(5), Iu(MAXWELLS,4), Nwlls,  &
       Totwell, Tot(MAXWELLS)
  CHARACTER Wllnms*80, Address*40, Lat*40, Formation*17
  COMMON /CHAR1 / Wllnms(MAXWELLS), Address(MAXWELLS,5), Lat(MAXWELLS), Formation(MAXWELLS)
  INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  !
  EXTERNAL LENS
  INTRINSIC ICHAR, CHAR, INT, MOD
  !
  DATA num/1, 23, 27, 24, 2, 38, 8, 25, 28, 17, 3, 37, 4, 15, 7, 26,  &
       9, 45, 10, 35, 11, 29, 18, 30, 20, 31, 21, 32, 22, 36, 19,  &
       40, 12, 41, 13, 39, 6, 33, 14, 34, 16, 5, 3*0/
  DATA words/'Temperature     ', 'pH              ',  &
       'Dissolved Oxygen', 'Alkalinity      ', 'Tritium         ',  &
       'H2S as S        ', 'Calcium         ', 'Eh              ',  &
       'Magnesium       ', 'Sodium          ', 'Potassium       ',  &
       'Chloride        ', 'Sulfate         ', 'Fluoride        ',  &
       'Silica          ', 'Bromide         ', 'Boron           ',  &
       'Barium          ', 'Lithium         ', 'Strontium       ',  &
       'Iron            ', 'Manganese       ', 'Nitrate         ',  &
       'Ammonium        ', 'Phosphate       ', 'DOC             ',  &
       'Sp. Cond.       ', 'Density         ', 'Delta C-13 TDIC ',  &
       'Carbon 14 TDIC  ', 'Delta S-34 (SO4)', 'Delta S-34 (H2S)',  &
       'Delta Deuterium ', 'Delta O-18      ', 'CH4 (aq)        ',  &
       'Sr 87/86        ', 'Aluminum        ', 'N2 (aq)         ',  &
       'N-15 of N2 (aq) ', 'N-15 of Nitrate ', 'N-15 of Ammonium',  &
       'Depth           ', 'Casing          ', 'Elevation       ',  &
       'RS of DOC       '/
  DATA report/14*'       ', 'as SiO2', 7*'       ', 2*'as N   ',  &
       'as P   ', 'as C   ', 11*'       ', 'as N   ', 7*'       '/
  DATA eleunits/'         ', 'degrees C', 'mg/l     ', 'o/oo     ',  &
       '% modern ', 'TU       ', 'volts    ', 'feet     ',  &
       'g/cm3    '/
  DATA iunits/1, 0, -1, -1, 5, -1, -1, 6, 18*-1, 0, 8, 3, 4, 4*3,  &
       -1, 0, -1, -1, 3*3, 3*7, 0/
  DATA units/'mmoles/l   ', 'meq/l      ', 'mg/l       ',  &
       'ppm        ', 'mmol/kgw   '/
  DATA carb/'Alkalinity #    ', 'Alkalinity #    ',  &
       'TDIC            ', 'Alkalinity #    '/
  !
  fname = 'output'
  ic = ICHAR('0')
  fname(7:8) = CHAR(INT(I/10)+ic)//CHAR(MOD(I,10)+ic)
  OPEN (Iw2,FILE=fname)
  WRITE (Iw2,9000) Wllnms(I)(5:36)
  WRITE (Iw2,9005) Address(I,1)
  DO j = 2, 5
     WRITE (Iw2,9010) Address(I,j)
  enddo
  WRITE (Iw2,9015) Wllnms(I)(47:61)
  WRITE (Iw2,9020) Lat(I)
  WRITE (Iw2,9025) Wllnms(I)(64:78)
  DO j = 1, 42, 2
     line = ' '
     DO k = 0, 1
        IF (num(j+k).NE.0) THEN
           tempwd = words(num(j+k))
           IF (num(j+k).EQ.4) tempwd = carb(Iu(I,3))
           line(k*38+1:k*38+16) = tempwd
           IF (num(j+k).EQ.8 .AND. Iu(I,2).NE.1) THEN
              line(18+38*k:25+38*k) = '    N.D.'
           ELSE IF (Dbsfg(I,num(j+k)).LT.0) THEN
              IF (Dbsfg(I,num(j+k)).EQ.-2) THEN
                 mat = '(''<'',F7.3)'
                 WRITE (tempvl,mat) Dbdata(I,num(j+k))
                 line(18+38*k:25+38*k) = tempvl
              ELSE
                 line(18+38*k:25+38*k) = '    N.D.'
              END IF
           ELSE
              mat = '(F8.'//CHAR(Dbsfg(I,num(j+k))+48)//')'
              WRITE (tempvl,mat) Dbdata(I,num(j+k))
              line(18+38*k:25+38*k) = tempvl
           END IF
           tempun = units(Iu(I,1))
           IF (iunits(num(j+k)).NE.-1) &
                tempun = eleunits(iunits(num(j+k)))
           line(27+38*k:37+38*k) = tempun
           IF (k.NE.0) THEN
              IF (iunits(num(j+k)).EQ.-1) THEN
                 ioff = LENS(units(Iu(I,1)))
                 line(66+ioff:) = report(num(j+k))
              END IF
           END IF
        END IF
     enddo
     IF (line.NE.' ') WRITE (Iw2,9030) line
  enddo
  WRITE (Iw2,9030) '-----------------------------------------'
  WRITE (Iw2,9030) 'N.D. = not determined'
  WRITE (Iw2,9030) 'TDIC = Total Dissolved Inorganic Carbon'
  WRITE (Iw2,9030) 'DOC = Dissolved Organic Carbon'
  WRITE (Iw2,9030) 'Sp. Cond. = Specific Conductivity (uS/cm)'
  WRITE (Iw2,9030) 'RS = Redox State'
  IF (Iu(I,3).EQ.0) THEN
     WRITE (Iw2,9030) '# = uncorrected, reported as HCO3-'
  ELSE IF (Iu(I,3).EQ.1) THEN
     WRITE (Iw2,9030) '# = corrected, reported as HCO3-'
  ELSE IF (Iu(I,3).EQ.3) THEN
     WRITE (Iw2,9030) '# = uncorrected, reported as CaCO3'
  END IF
  CLOSE (Iw2)
  RETURN
9000 FORMAT (/' Well name          : ',A/)
9005 FORMAT (' Owner              : ',A)
9010 FORMAT ('                      ',A)
9015 FORMAT (' Site ID            : ',A)
9020 FORMAT (' Latitude/longitude : ',A)
9025 FORMAT (' Date/time sampled  : ',A/)
9030 FORMAT (A)
end subroutine PUTDATA
!
!
!
SUBROUTINE RDDB
  use filenames
  implicit none

  CHARACTER*80 line, funame
  INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  integer LENS 
  LOGICAL exists
  external LENS

  funame = root(1:LENS(root))//'.lon'
  INQUIRE (FILE=funame, EXIST=exists)
  if (exists) then
	OPEN (UNIT=Ir,FILE=funame,STATUS='old',ERR=60)
	READ (Ir,'(A4)',ERR=60) line
  else
	OPEN (UNIT=Ir,FILE=funame,STATUS='new',ERR=60)
	line = "2.14"
  endif	
  if (line .eq. "2.14") then
     call rddb214
  else
     rewind(Ir)
     call rddb2
  endif
  CLOSE (Ir)
  RETURN
60 continue
  write(*,*) "Error opening file ", root
  write(*,*) "In directory ", path
  stop

END SUBROUTINE RDDB
!
!
!
SUBROUTINE RDDB2
  USE max_size
  implicit none
  CHARACTER*10 inpt
  CHARACTER*80 line, funame
  INTEGER err, LENS, jk, id, iadd, index, k, l
  !
  REAL Dbdata
  COMMON /DB    / Dbdata(MAXWELLS,45)
  INTEGER Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
  COMMON /INT4DB/ Dbsfg(MAXWELLS,45), Idefault(5), Iu(MAXWELLS,4), Nwlls,  &
       Totwell, Tot(MAXWELLS)
  CHARACTER Wllnms*80, Address*40, Lat*40, Formation*17
  COMMON /CHAR1 / Wllnms(MAXWELLS), Address(MAXWELLS,5), Lat(MAXWELLS), Formation(MAXWELLS)
  INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  CHARACTER*256 Dfile, path
  COMMON /FILEC / Dfile, path
  !
  EXTERNAL GETNO, LENS
  !
  Idefault(1) = 2
  Idefault(2) = 2
  Idefault(3) = 0
  Idefault(4) = 0
  Idefault(5) = 0
  Nwlls = 0
  Totwell = 0
  !      funame = Dfile(1:LENS(Dfile))//'.lon'
  !      OPEN (UNIT=Ir,FILE=funame,STATUS='OLD',ERR=60)
10 READ (Ir,9005,ERR=60,END=60) line
  Nwlls = Nwlls+1
  IF (Nwlls.EQ.51) GO TO 90
  READ (line,9000,ERR=80) (Iu(Nwlls,jk),jk=1,4)
  DO id = 1, 4
     Idefault(id+1) = Iu(Nwlls,id)
  enddo
  Wllnms(Nwlls) = line
  READ (Ir,9005,END=70) line
  Lat(Nwlls) = line(1:40)
  READ (line(66:70),9010,ERR=80) Tot(Nwlls)
  IF (Nwlls.EQ.1) READ (line(75:79),9010,ERR=80) Totwell
  IF (Tot(Nwlls).EQ.0) Tot(Nwlls) = Nwlls
  IF (Totwell.LT.Tot(Nwlls)) Totwell = Tot(Nwlls)
  DO iadd = 1, 5
     READ (Ir,9005,END=70) line
     Address(Nwlls,iadd) = line(1:40)
  enddo
  index = 0
  k = 0
40 k = k+1
  READ (Ir,9005,END=70) line
  DO l = 5, 61, 14
     index = index+1
     inpt = line(l:l+9)
     CALL GETNO(inpt,Dbdata(Nwlls,index),Dbsfg(Nwlls,index),err)
     IF (err.GT.0) THEN
        IF (err.EQ.1) GO TO 80
        IF (err.EQ.2) THEN
           WRITE (*,9015) Nwlls, index, Wllnms(Nwlls)(1:79)
           STOP
        ELSE IF (err.EQ.3) THEN
           WRITE (*,9020) Nwlls, index, Wllnms(Nwlls)(1:79)
           STOP
        END IF
     END IF
  enddo
  IF (k.LT.9) GO TO 40
  READ (Ir,9005) line
  Formation(Nwlls) = line(1:17)
  GO TO 10
60 continue
  !   60 CLOSE (Ir)
  RETURN
70 WRITE (*,9025)
  STOP
80 WRITE (*,9030) Nwlls, index, Wllnms(Nwlls)(1:79)
  STOP
90 WRITE (*,9035)
  STOP
9000 FORMAT (4(I1))
9005 FORMAT (A80)
9010 FORMAT (I5)
9015 FORMAT (/' **** Error ****',/,' No decimal point in number.',/, &
       ' well#',I3,' index#',I3,/,1X,A79,/,' ***************'/)
9020 FORMAT (/' **** Error ****',/,' Unknown error at well#',I3, &
       ' index#',I3,/,1X,A79,/,' ***************'/)
9025 FORMAT (/' **** Error ****',/, &
       ' End of file before all data in last well was read.',/, &
       ' ***************'/)
9030 FORMAT (/' **** Error ****',/,' Reading error at well#',I3, &
       ' index#',I3,/,1X,A79,/,' ***************'/)
9035 FORMAT (/' **** Error ****',/, &
       ' Arrays not big enough to hold all of data base.',/, &
       ' ***************'/)
end subroutine RDDB2
!
!
!
SUBROUTINE RDDB214
  USE max_size
  implicit none
  INTEGER jcounter, i
  CHARACTER*15 inpt
  CHARACTER*80 line, funame
  INTEGER err, LENS, jk, id, iadd, index, k, l
  !
  REAL Dbdata
  COMMON /DB    / Dbdata(MAXWELLS,45)
  INTEGER Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
  COMMON /INT4DB/ Dbsfg(MAXWELLS,45), Idefault(5), Iu(MAXWELLS,4), Nwlls,  &
       Totwell, Tot(MAXWELLS)
  CHARACTER Wllnms*80, Address*40, Lat*40, Formation*17
  COMMON /CHAR1 / Wllnms(MAXWELLS), Address(MAXWELLS,5), Lat(MAXWELLS), Formation(MAXWELLS)
  INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
  CHARACTER*256 Dfile, path
  COMMON /FILEC / Dfile, path
  !
  EXTERNAL GETNO214, LENS
  !
  Idefault(1) = 2
  Idefault(2) = 2
  Idefault(3) = 0
  Idefault(4) = 0
  Idefault(5) = 0
  Nwlls = 0
  Totwell = 0
  !      funame = Dfile(1:LENS(Dfile))//'.lon'
  !      OPEN (UNIT=Ir,FILE=funame,STATUS='OLD',ERR=60)
  !
  !  Read wells to end of file
  !
10 continue
  READ (Ir,9005,ERR=60,END=60) line
  Nwlls = Nwlls+1
  IF (Nwlls.EQ.MAXWELLS + 1) then
     WRITE (*,9035)
     read (*,*)
     STOP
  endif
  !     Integers, wllnm
  READ (line,9000,ERR=80) (Iu(Nwlls,jk),jk=1,4)
  DO id = 1, 4
     Idefault(id+1) = Iu(Nwlls,id)
  enddo
  Wllnms(Nwlls) = line
  !     Lat/lon
  READ (Ir,9005,END=70) line
  Lat(Nwlls) = line(1:40)
  !     Well number
  READ (Ir,'(I15)',END=70,ERR=80) Tot(Nwlls)
  !     Total number of wells
  READ (Ir,'(I15)',END=70,ERR=80) i
  IF (Nwlls.EQ.1) Totwell = i
  IF (Tot(Nwlls).EQ.0) Tot(Nwlls) = Nwlls
  IF (Totwell.LT.Tot(Nwlls)) Totwell = Tot(Nwlls)
  !     Address
  DO iadd = 1, 5
     READ (Ir,9005,END=70) line
     Address(Nwlls,iadd) = line(1:40)
  enddo
  !     Data
  jcounter = 41
  do index = 1, jcounter 
     READ (Ir,9005,END=70,ERR=80) line
     inpt = line(1:15)
     CALL GETNO214(inpt,Dbdata(Nwlls,index),Dbsfg(Nwlls,index),err)
     IF (err.GT.0) THEN
        IF (err.EQ.1) GO TO 80
        IF (err.EQ.2) THEN
           WRITE (*,9015) Nwlls, index, Wllnms(Nwlls)(1:79)
           STOP
        ELSE IF (err.EQ.3) THEN
           WRITE (*,9020) Nwlls, index, Wllnms(Nwlls)(1:79)
           STOP
        END IF
     END IF
  enddo
  !     Formation
  READ (Ir,9005) line
  Formation(Nwlls) = line(1:17)
  GO TO 10
60 continue
  !      CLOSE (Ir)
  RETURN
70 WRITE (*,9025)
  STOP
80 WRITE (*,9030) Nwlls, index, Wllnms(Nwlls)(1:79)
  STOP
90 WRITE (*,9035)
  STOP
9000 FORMAT (4(I1))
9005 FORMAT (A80)
9010 FORMAT (I5)
9015 FORMAT (/' **** Error ****',/,' No decimal point in number.',/, &
       ' well#',I3,' index#',I3,/,1X,A79,/,' ***************'/)
9020 FORMAT (/' **** Error ****',/,' Unknown error at well#',I3, &
       ' index#',I3,/,1X,A79,/,' ***************'/)
9025 FORMAT (/' **** Error ****',/, &
       ' End of file before all data in last well was read.',/, &
       ' ***************'/)
9030 FORMAT (/' **** Error ****',/,' Reading error at well#',I3, &
       ' index#',I3,/,1X,A79,/,' ***************'/)
9035 FORMAT (/' **** Error ****',/, &
       ' Arrays not big enough to hold all of data base.',/, &
       ' ***************'/)
end subroutine RDDB214
!
!
!
CHARACTER*1 FUNCTION UPCS(CHR)
  CHARACTER*1 CHR
  !
  ! The character is changed to upper case.
  !
  INTEGER ich
  INTRINSIC ICHAR, CHAR
  !
  ich = ICHAR(CHR)
  IF (ich.GE.ICHAR('a') .AND. ich.LE.ICHAR('z')) THEN
     UPCS = CHAR(ich-ICHAR('a')+ICHAR('A'))
  ELSE
     UPCS = CHR
  END IF
  RETURN
END FUNCTION UPCS
!
!
!
CHARACTER*80 FUNCTION UPCS80(LINE)
  CHARACTER*(*) LINE
  !
  ! The first up to 80 characters in a line are changed to upper case.
  !
  INTEGER i, n, ich, ishft, lowera, lowerz, LENS
  EXTERNAL LENS
  INTRINSIC ICHAR, CHAR
  !
  lowera = ICHAR('a')
  lowerz = ICHAR('z')
  ishft = lowera-ICHAR('A')
  n = LENS(LINE)
  IF (n.GT.80) n = 80
  UPCS80 = '    '
  IF (n.GT.0) UPCS80 = LINE(:n)
  DO i = 1, n
     ich = ICHAR(LINE(i:i))
     IF (ich.GE.lowera .AND. ich.LE.lowerz) UPCS80(i:i) &
          = CHAR(ich-ishft)
  enddo
  RETURN
END function UPCS80
!
!
!
INTEGER FUNCTION STRCMP_NOCASE(LINE1, LINE2)
  implicit none
  CHARACTER*(*) LINE1, LINE2
  integer n1, n2, ich1, ich2
  !
  ! Compare strings disregarding case
  !
  INTEGER i, n, ich, ishft, lowera, lowerz, LENS
  EXTERNAL LENS
  INTRINSIC ICHAR, CHAR
  !
  lowera = ICHAR('a')
  lowerz = ICHAR('z')
  ishft = lowera-ICHAR('A')
  n1 = LENS(LINE1)
  n2 = LENS(LINE2)
  if (n1 > n2) then
     strcmp_nocase = -1
     return
  else if (n1 < n2) then
     strcmp_nocase = 1
     return
  endif
  DO i = 1, n1
     ich1 = ICHAR(LINE1(i:i))
     ich2 = ICHAR(LINE2(i:i))
     IF (ich1.GE.lowera .AND. ich1.LE.lowerz) ich1 = ich1 - ishft
     IF (ich2.GE.lowera .AND. ich2.LE.lowerz) ich2 = ich2 - ishft
     if (ich1 < ich2) then
        strcmp_nocase = -1
        return
     else if (ich1 > ich2) then
        strcmp_nocase = 1
        return
     endif
  enddo
  strcmp_nocase = 0
  RETURN 
END function STRCMP_NOCASE

!
!
!
      INTEGER FUNCTION LENS(STRING)
!
!     Determine length of character string minus trailing blanks
!
      CHARACTER*(*) STRING
      INTEGER max, i
      INTRINSIC LEN
!
      max = LEN(STRING)
      DO 10 i = max, 1, -1
        IF (STRING(i:i).NE.' ') THEN
          LENS = i
          GOTO 20
        END IF
   10 CONTINUE
      LENS = 0
!
   20 RETURN
      END

