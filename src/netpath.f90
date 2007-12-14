!
PROGRAM NETPATHXL
  USE max_size
  use filenames
  implicit none
  ! --------------------------------------
  !       NETPATHXL 2.14 June 12, 2005
  !                  by
  !           David Parkhurst
  !
  !    with technical assistance from
  !           L. Niel Plummert
  ! --------------------------------------
  ! --------------------------------------
  !       NETPATH 2.13 December 30, 1996
  !                  by
  !           Eric C. Prestemon
  !
  !    with technical assistance from
  ! L. Niel Plummer and David L. Parkhurst
  ! --------------------------------------
  !
  !
  ! Some of the variables in NETPATH
  !
  ! PNAME(39), PHASE(39)        : The names of the selected phases
  ! WLLNMS(0:50)                : The names of the wells
  ! EFILE,PFILE,WFILE           : Files used for input or output
  ! ENAME(39),PELT(39,39)       : Short names of constraints
  ! ELESHORT(39),ELELONG(0:28)  : The lists of short and long names of
  !                               the constraints
  ! FLINE(100)                  : Lines in netpath.dat - stored phases
  ! TRANSFER(39),IT(39)         : Transfer limitations on the phases
  ! FORCE(39),F(39)             : Forcing of phases
  ! WELL(0:5)                   : The well numbers for 0 - Final Well
  !                               1-5 Initial wells
  ! TOT(50)                     : Permanent well numbers (not affected
  !                               by deletions or moves in DB)
  ! PCOEFF(39,36)               : Coefficients of elements in phases
  ! JELE(39,36)                 : short names of elements in phases
  ! IFLAG(6)                    : 6 flags:
  !                               1 - Mixing  2 - Ion Exchange
  !                               3 - Rayleigh calcs  4 - A0 model
  !                               5 - Mook/Deines fract. factors
  !                               6 - Evaporation/Dilution
  ! P(3)                        : 3 parameters:
  !                               1 - fraction CO2 in CO2-CH4
  !                               2 - fraction calcium in exchange
  !                               3 - <not used>
  ! IELE(36)                    : The chosen constraints
  ! DBDATA(50,50)               : The well data, as follows:
  !    1: total Carbon            26: Deuterium
  !    2: total Sulfur            27: Oxygen-18
  !    3: total Calcium           28: Tritium
  !    4: total Aluminum          29: total Sulfides
  !    5: total Magnesium         30: total Sulfate
  !    6: total Sodium            31: S-34 of SO4 * Sulfate
  !    7: total Potassium         32: S-34 of H2S * Sulfides
  !    8: total Chloride          33: Nitrogen part of RS
  !    9: total Flouride          34: Carbon part of RS (except DOC)
  !   10: total Silica            35: Iron part of RS
  !   11: total Bromide           36: HCO3
  !   12: total Boron             37: pH
  !   13: total Barium            38: H2CO3
  !   14: total Lithium           39: CO3
  !   15: total Strontium         40: Manganese part of RS
  !   16: total Iron              41: total DIC
  !   17: total Manganese         42: total CH4
  !   18: total Nitrogen          43: total DOC
  !   19: total Phosphorous       44: C-13 of CH4
  !   20: Dissolved oxygen        45: C-13 of DOC
  !   21: C-13 of TDIC * TDIC     46: C-14 of CH4
  !   22: C-14 of TDIC * TDIC     47: C-14 of DOC
  !   23: S-34 of water * total S 48: RS of DOC
  !   24: Sr-87 of water * tot Sr 49: User-entered RS of DOC
  !   25: N-15 of total N         50: <not used>
  ! PARA(39,16)                 : Isotopic values for each phase
  ! NODATA(50,50)               : Stores whether each value has
  !                               been entered in DB
  ! C14DAT(13)                  : Parameters for the A0 models:
  !        1: C-14 activity in carbonate minerals
  !        2: C-14 activity in soil gas CO2
  !        3: C-13 activity in solution
  !        4: C-13 activity in carbonate minerals
  !        5: C-13 activity in soil gas CO2
  !        6: not used
  !        7: not used
  !        8: C-14 activity in dolomite
  !        9: C-14 activity in calcite
  !       10: Choice of method for C-13 of soil gas CO2
  !       11: Choice of method for C-13 of initial solution
  !       12: C-13 activity in dolomite
  !       13: C-13 activity in calcite
  !
  !
  ! This is the main loop of the program. Options are displayed and
  ! branches are made to the proper subroutines.  Exiting the program
  ! is also handled, including writing netpath.dat.
  !
  CHARACTER Pname*8, Ename*2, Force*1
  COMMON /CHAR3 / Pname(39), Ename(39), Force(39)
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1), &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  CHARACTER*360 Fline(100)
  COMMON /CHAR7 / Fline
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION Predat, Disdat, Cfinal, Cinit, Dinit, Result
  COMMON /DP2   / Predat(39,2), Disdat(39,2), Cfinal, Cinit, Dinit, &
       Result
  DOUBLE PRECISION Clmain, Cleach, Pdat, Res, Sfinal, Sinit, Maxdel, &
       Mindel, Dfinal
  COMMON /DP3   / Clmain(202,40), Cleach(202,40), Pdat(40,40), &
       Res(100), Sfinal(39), Sinit(5,59), Maxdel(39), &
       Mindel(39), Dfinal
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40), &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  DOUBLE PRECISION Pcoef
  COMMON /DP6   / Pcoef(39,39)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50), &
       Isdocrs(0:5)
  INTEGER Idis, Ipre, Rwunit
  COMMON /INT2  / Idis, Ipre, Rwunit
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone, &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39), &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  LOGICAL First, Quit
  COMMON /LOG3  / First, Quit
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  !
  CHARACTER*1 ans, UPCS
  INTEGER j, ij, LENS
  EXTERNAL INITVALS, WELLFILE_PAT, MODELS, SCREEN, POSCUR, CLS,   &
       DELETE, ADD, RUN, EDIT, SAVE, VIEW, UPCS, LENS
  ! ---------------------------------------------------------------------
  Efile = '@(#)NETPATH - Geochemical reaction-path program'
  Efile = '@(#)NETPATH - USGS WRIR 91-4078, L.N. Plummer, E.C. Prestemon, & D.L. Parkhurst'
  Efile = '@(#)NETPATH - Contact: h2osoft@usgs.gov'
  Efile =  &
       '$Id: netpath.f 306 2005-05-13 22:44:42Z dlpark $'
  !
  ! Below are the system-specific variables that need to be set according
  ! to your system type (before compiling)
  !
  ! -----
  ! TUNIT
  ! -----
  ! This is the unit number for terminal output.
  ! This is used when models are run to display the model results.
  ! Unix uses 6, PRIMOS uses 1, and DOS uses 0
  ! Standard Fortran should use 0.
  !
  Tunit = 6
  !
  ! ------------------
  ! RUNIT,WUNIT,RWUNIT
  ! ------------------
  ! These are three unit numbers that are used to write to files.
  ! Usually, any number between 7 and 15 will work for each.
  ! They must be different.
  !
  Runit = 7
  Wunit = 8
  Rwunit = 9
  !
  ! -----
  ! ICASE
  ! -----
  ! This flag determines whether upper and lower case will be treated
  ! as different.
  ! ICASE=0: All file names are converted to the same case before being
  !          compared. (Penn.pat is the same as penn.pat)
  ! ICASE=1: Case is significant in file names. You must type filenames
  !          exactly in the case you want.
  ! Unix should use 1, DOS and PRIMOS both should use 0.
  !
  Icase = 1
  !
  !
  ! END OF SYSTEM SPECIFIC VARIABLES
  !
  call confignetpath

  CALL WELLFILE_PAT(.true.)
  CALL INITVALS(0)
  CALL MODELS214
  CALL SCREEN
10 CALL POSCUR(-1)
  WRITE (*,9000)
  READ (*,'(A)') ans
  CALL POSCUR(-1)
  ans = UPCS(ans)
  IF (ans.EQ.'D') THEN
     CALL DELETE
  ELSE IF (ans.EQ.'A') THEN
     CALL ADD
  ELSE IF (ans.EQ.'R') THEN
     CALL RUN(1)
  ELSE IF (ans.EQ.'E') THEN
     CALL EDIT
  ELSE IF (ans.EQ.'S') THEN
     CALL SAVE
 ! ELSE IF (ans.EQ.'V') THEN
 !    CALL VIEW
  ELSE IF (ans.EQ.'F') THEN
      CALL FONT
      CALL SCREEN
  ELSE IF (ans.EQ.'Q') THEN
     CALL POSCUR(-1)
     WRITE (*,9005)
     READ (*,'(A)') ans
     IF (UPCS(ans).NE.'N') THEN
        OPEN (Runit,FILE='netpath.dat',STATUS='unknown')
        CLOSE (Runit,STATUS='DELETE')
        OPEN (Runit,FILE='netpath.dat',STATUS='new')
        DO j = 1, Flin
           WRITE (Runit,'(a)') Fline(j)(1:80)
           DO ij = 0, 3
              IF (LENS(Fline(j)).GT.80+70*ij) WRITE (Runit,'(10X,A70)') &
                   Fline(j)(81+ij*70:150+ij*70)
           enddo
        enddo
        CLOSE (Runit)
        CALL CLS
        WRITE (*,9010)
        GOTO 100
     END IF
  END IF
  GO TO 10
100 continue
9000 FORMAT (' Select: <A>dd, <D>elete, <E>dit, <F>ont, <R>un, <S>ave, ', &
		'or <Q>uit')
!       '<V>iew, or <Q>uit')
9005 FORMAT (' Do you really want to quit? <Enter> for yes.')
9010 FORMAT (' NETPATH finished - Thank You'/)
END PROGRAM NETPATHXL
!
!
!
BLOCK DATA
 USE max_size
  use filenames
!
! The initial values of arrays contained in the common blocks are set.
!

CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
     Ion(4), Ffact(0:1)
CHARACTER Elelong*12, Pelt*2
COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
     Disalong, Usera(5)
!
DATA Model/'Original Data       ', 'Mass Balance        ',  &
     'Vogel               ', 'Tamers              ',  &
     'Ingerson and Pearson', 'Mook                ',  &
     'Fontes and Garnier  ', 'Eichinger           ',  &
     'User-defined        '/
DATA Yes/'No ', 'Yes'/
DATA Ion/'Computed  ', '50/50     ', 'Ca/Na     ', 'Var. Ca/Mg'/
DATA Elelong/'            ', 'Carbon      ', 'Sulfur      ',  &
     'Calcium     ', 'Aluminum    ', 'Magnesium   ',  &
     'Sodium      ', 'Potassium   ', 'Chloride    ',  &
     'Fluoride    ', 'Silica      ', 'Bromide     ',  &
     'Boron       ', 'Barium      ', 'Lithium     ',  &
     'Strontium   ', 'Iron        ', 'Manganese   ',  &
     'Nitrogen    ', 'Phosphorus  ', 'Redox       ',  &
     'Carbon-13   ', 'C-14 (% mod)', 'Sulfur-34   ',  &
     'Strontium-87', 'Nitrogen-15 ', 'Deuterium   ',  &
     'Oxygen-18   ', 'Tritium     '/
DATA C14dat/0.0D0, 100.0D0, 0.0D0, 0.0D0, -25.0D0, 100.0D0,  &
     100.0D0, 6*0.0D0/
DATA Ffact/'Mook  ', 'Deines, et al.'/
END 
!
!
!
SUBROUTINE ADD
  USE max_size
  implicit none
  !
  ! The specific add routines are called
  !
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER i, nopha1
  EXTERNAL POSCUR, ADDCON, ADDPHA, SCREEN
  !
10 CALL POSCUR(-1)
  WRITE (*,9000)
  READ (*,'(I1)',ERR=10) i
  IF (i.NE.3) THEN
     IF (i.NE.1 .AND. i.NE.2) GO TO 10
     CALL POSCUR(-1)
     IF (i.EQ.1) CALL ADDCON
     Iedit = 0
     nopha1 = Nopha+1
     IF (i.EQ.2) CALL ADDPHA(nopha1)
     CALL SCREEN
  END IF
  RETURN
9000 FORMAT (' Add (1) constraints, (2) phases, or (3) neither?')
END SUBROUTINE ADD
!
!
!
SUBROUTINE ADDCON
  implicit none
  !
  ! Additional constraints can be added to the list of constraints to be
  ! used in the model
  !
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER i, j
  CHARACTER bans*2
  EXTERNAL CLS, CONLIST
  !
10 CALL CLS
  CALL CONLIST(28)
20 WRITE (*,9000)
  READ (*,'(A)',ERR=20) bans
  IF (bans.EQ.'  ') GOTO 40
  IF (bans.EQ.'? ') GO TO 10
  READ (bans,'(I2)',ERR=20) i
  IF (i.LE.28 .AND. i.GE.1) THEN
     j = 0
30   j = j+1
     IF (j.GT.Noele) THEN
        Noele = Noele+1
        Iele(Noele) = i
     ELSE
        IF (i.NE.Iele(j)) GO TO 30
        WRITE (*,9005)
        READ (*,'(A)',ERR=20) bans
     END IF
  END IF
  GO TO 20
40 RETURN
  !
9000 FORMAT (//, &
       ' Enter constraint to add (<Enter> to quit, ''?'' for list):')
9005 FORMAT (' Constraint already entered.  <Enter> to continue.')
END SUBROUTINE ADDCON
!
!
!
SUBROUTINE ADDPHA(IIOLD)
  USE max_size
  use filenames
  implicit none
  INTEGER IIOLD
  !
  ! This subroutine is called in two situations.  First, new phases are
  ! added to the list of phases to be considered in the model.  Second, it
  ! is called when a phase is to be replaced.  If the phase is edited by
  ! piece (changing of some constraints of the phase, rather than
  ! substituting a new phase), EDITPIEC is called.
  !

  CHARACTER*360 Fline(100)
  COMMON /CHAR7 / Fline
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  CHARACTER pelem(36)*2, UPCS*1, line*3
  INTEGER ij, j, i, jp, k, ii, istop
  EXTERNAL CLS, EDITPIEC, PHALIST, DONTHAVE, HAVE, TRANS, UPCS
  !
  CALL CLS
  ii = IIOLD
  istop = 0
10 IF (Flin.EQ.1) THEN
20   CALL EDITPIEC(ii,istop)
     IF (Iedit.EQ.0) THEN
        IF (istop.EQ.1) GO TO 90
        Nopha = Nopha+1
        ii = Nopha+1
     ELSE
        GO TO 90
     END IF
     GO TO 20
  END IF
30 WRITE (*,9000)
  IF (Iedit.EQ.0) WRITE (*,9005) Flin
  IF (Iedit.EQ.1) WRITE (*,9010) Flin, Phase(ii)
  READ (*,'(A3)') line
  IF (line.NE.' ' .OR. Iedit.NE.1) THEN
     IF (line.EQ.' ') GO TO 90
     IF (UPCS(line(1:1)).EQ.'L') THEN
        CALL PHALIST(ij)
        GO TO 40
     END IF
     READ (line,'(I3)',ERR=10) ij
     IF (Iedit.EQ.1 .AND. ij.EQ.0) GO TO 90
40   IF (ij.LE.0 .OR. ij.GT.Flin) GO TO 30
     IF (Iedit.EQ.0) Nopha = Nopha+1
     IF (Iedit.EQ.0) ii = Nopha
     IF (ij.EQ.Flin) THEN
        CALL EDITPIEC(ii,istop)
        IF (Iedit.EQ.0) GO TO 10
        GO TO 90
     END IF
     F(ii) = ' '
     READ (Fline(ij),9015) Phase(ii), It(ii),  &
          (pelem(j),Pcoeff(ii,j),j=1,35)
     CALL DONTHAVE(ii,1)
     Iedit = 0
     DO i = 1, 36
        Jele(ii,i) = 0
     enddo
     j = 0
     jp = 0
55   jp = jp+1
60   j = j+1
     IF (pelem(j).EQ.'  ') Jele(ii,jp) = 0
     IF (pelem(j).NE.'  ') THEN
        k = 0
70      k = k+1
        IF (pelem(j).NE.Eleshort(k)) THEN
           IF (k.LT.33) GO TO 70
           WRITE (*,'(//,A,1X,I2)') ' Bad constraint in phase #', ii
           STOP
        END IF
        !  Set default fractioation factor for Strontium minerals
        if (k.eq.15) then
           Para(ii, 10) = 0.0
           CALL HAVE(ii,10)
        endif
        IF (k.GT.20) THEN
           IF (k.LE.25 .OR. k.GE.29) THEN
              IF (k.LE.25) THEN
                 !  Sets isotopic composition for C13, C14, S34, Sr87, N15
                 Para(ii,k-19) = Pcoeff(ii,jp)
                 CALL HAVE(ii,k-19)
              ELSE IF (k.GE.29 .AND. k.LE.33) THEN
                 !  Sets fractionation factor for C13, C14, S34, Sr87, N15
                 Para(ii,k-22) = Pcoeff(ii,jp)
                 CALL HAVE(ii,k-22)
              END IF
              DO k = jp, 35
                 Pcoeff(ii,k) = Pcoeff(ii,k+1)
              enddo
              GO TO 60
           END IF
        END IF
        Jele(ii,jp) = k
        IF (j.LT.35) GO TO 55
     END IF
  END IF
  CALL TRANS(ii)
  IF (Iedit.EQ.0) GO TO 10
90 RETURN
9000 FORMAT (//,' Input phase number (type ''L'' to see phases and', &
       ' their corresponding numbers.)')
9005 FORMAT (I4,' to create new phase, <Enter> to stop entering phases' &
       )
9010 FORMAT (I4,' to edit phase composition, <Enter> for ''',A,'''')
9015 FORMAT (A8,1X,A1,35(A2,F8.4))
END SUBROUTINE ADDPHA
!
!
!
DOUBLE PRECISION FUNCTION C14(IWHICH,IWELL)
  USE max_size
  INTEGER IWHICH, IWELL
  !
  ! The result of a specific A0 model for the selected initial well
  ! is returned. The convention adopted is that total dissolved carbon
  ! is the sum of total dissolved inorganic carbon, dissolved methane,
  ! and dissolved organic carbon.
  !
  !  Values for IWHICH
  !      -1  RETURNS C-13 OF SOIL CO2 VALUE FOR CHECKING.
  !       1 'Original Data       '
  !       2 'Mass Balance        '
  !       3 'Vogel               '
  !       4 'Tamers              '
  !       5 'Ingerson and Pearson'
  !       6 'Mook                '
  !       7 'Fontes and Garnier  '
  !       8 'Eichinger           '
  !       9 'User-defined        '
  !

  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER i
  DOUBLE PRECISION aa, ah, ak1, ak2, b, c1, c13bex, c13seq, c2, c3,  &
       c4, cans, cm, ct, fract3, fract4, fract6,  &
       fract7, fraction, t
  DOUBLE PRECISION i10, i11
  double precision fgk
  COMMON /FontGarnier/ fgk
  EQUIVALENCE (C14dat(10),i10)
  EQUIVALENCE (C14dat(11),i11)
  EXTERNAL CFRACT
  INTRINSIC DABS
  !
  i = Well(IWELL)
  C14 = -999.0D0
  IF (Dbdata(i,41).LE.1.0D-10) GO TO 10
  !
  ! C1 is the value used for initial well C-13
  !
  c1 = Dbdata(i,21)/Dbdata(i,41)
  IF (i11.GT.0.0D0) c1 = C14dat(3)
  !
  ! C2 is the value for C-13 of soil gas CO2
  !
  c2 = C14dat(5)
  !
  ! Define C-13 of soil gas CO2 by mass balance:
  ! Pure Water + Calcite + Dolomite + Gypsum + CO2 (without fractionation)
  !     = Initial Well
  ! This case is comparable to piston injection of CO2 with subsequent
  ! closed-system reaction with calcite, dolomite and gypsum
  !
  IF (i10.GT.1.0D0 .OR. DABS(i10-1.0D0).LT.1.0D-6) &
       c2 = (Dbdata(i,21)-2.0D0*Dbdata(i,5)*C14dat(12) &
       -(Dbdata(i,3)-Dbdata(i,5)-Dbdata(i,30))*C14dat(13)) &
       /(Dbdata(i,41)+Dbdata(i,30)-Dbdata(i,3)-Dbdata(i,5))
  !
  ! Define C-13 of soil gas CO2 for open (equilibrium) system
  !
  CALL CFRACT(fraction,8,IWELL,Ierror)
  IF (DABS(i10-3.0D0).LT.1.0D-6) c2 = c1+fraction
  !
  ! Define C-13 of soil gas CO2 for partial open system
  ! This case is analogous to an open equilibrium system in a quartz
  ! sand aquifer with subsequent reaction with calcite, dolomite and
  ! gypsum in a closed system.
  !
  CALL CFRACT(fraction,3,IWELL,Ierror)
  IF (DABS(i10-2.D0).LT.1.D-6) c2 = c2-fraction
  !
  ! RETURNS C-13 OF SOIL CO2 VALUE FOR CHECKING...
  IF (IWHICH.EQ.-1) THEN
     C14 = c2
     GO TO 10
  END IF
  !
  c3 = C14dat(2)+0.2D0*fraction
  c4 = c2+fraction
  IF (Dbdata(i,1).GT.0.0D0 .OR. (IWHICH.EQ.9.OR.IWHICH.EQ.3)) THEN
     IF (IWHICH.EQ.1) THEN
        ! Original Data
        C14 = (Dbdata(i,22)+Dbdata(i,42)*Dbdata(i,46)+Dbdata(i,43) &
             *Dbdata(i,47))/Dbdata(i,1)
     ELSE IF (IWHICH.EQ.2) THEN
        ! Mass Balance model (calcite, dolomite, gypsum, co2 gas)
        C14 = ((Dbdata(i,5)*C14dat(8)+C14dat(9)*(Dbdata(i,3) &
             -Dbdata(i,5)-Dbdata(i,30))+C14dat(2) &
             *(Dbdata(i,41)+Dbdata(i,30)-Dbdata(i,5)-Dbdata(i,3))) &
             +Dbdata(i,42)*Dbdata(i,46)+Dbdata(i,43)*Dbdata(i,47)) &
             /Dbdata(i,1)
     ELSE IF (IWHICH.EQ.3) THEN
        ! Vogel model
        C14 = (85.0D0*Dbdata(i,41)+Dbdata(i,42)*Dbdata(i,46) &
             +Dbdata(i,43)*Dbdata(i,47))/Dbdata(i,1)
     ELSE IF (IWHICH.EQ.4) THEN
        ! Tamers
        C14 = ((((Dbdata(i,38)+0.5D0*Dbdata(i,36))*C14dat(2)+C14dat(1) &
             *0.5D0*Dbdata(i,36))/(Dbdata(i,38)+Dbdata(i,36))) &
             *Dbdata(i,41)+Dbdata(i,42)*Dbdata(i,46)+Dbdata(i,43) &
             *Dbdata(i,47))/Dbdata(i,1)
     ELSE IF (IWHICH.EQ.5) THEN
        ! Ingerson and Pearson
        IF (DABS(C14dat(4)-c2).GE.1.0D-8) &
             C14 = (((c1-C14dat(4))/(c2-C14dat(4)) &
             *(C14dat(2)-C14dat(1))+C14dat(1))*Dbdata(i,41) &
             +Dbdata(i,42)*Dbdata(i,46)+Dbdata(i,43)*Dbdata(i,47)) &
             /Dbdata(i,1)
     ELSE IF (IWHICH.EQ.6) THEN
        ! Mook
        ah = 10.0D0**(-Dbdata(i,37))
        t = Dbdata(i,50)+273.15D0
        ak1 = 10.0D0**(-3404.71D0/t-.032786D0*t+14.8435D0)
        ak2 = 10.0D0**(-2902.39D0/t-0.02379D0*t+6.4890D0)
        aa = ah*ah/(ah*ah+ah*ak1+ak1*ak2)
        b = ah*ak1/(ah*ah+ah*ak1+ak1*ak2)
        CALL CFRACT(fraction,4,IWELL,Ierror)
        C14 = (aa+0.5D0*b)*c3+0.5D0*b*C14dat(1) &
             +(C14dat(2)*(1.0D0-fraction/500.0D0) &
             -0.50D0*(c3+C14dat(1))) &
             *(c1-aa*c4-0.5D0*b*(c4+C14dat(4))) &
             /(c2-fraction*(1.0D0+c2/1000.0D0)-0.5D0*(c4+C14dat(4)))
        C14 = (C14*Dbdata(i,41)+Dbdata(i,42)*Dbdata(i,46)+Dbdata(i,43) &
             *Dbdata(i,47))/Dbdata(i,1)
     ELSE IF (IWHICH.EQ.7) THEN
        ! Fontes and Garnier
        cm = Dbdata(i,36)/2.0D0
        !   Use this instead of TDIC
        ct = Dbdata(i,38)+Dbdata(i,36)
        CALL CFRACT(fraction,4,IWELL,Ierror)
        fgk = (C14dat(2)-0.2D0*fraction-C14dat(1)) &
             *(c1-cm/ct*C14dat(4)-(1-cm/ct)*c2) &
             /(c2-fraction-C14dat(4))
        IF (fgk.LT.-1D-5) THEN
           CALL CFRACT(fraction,5,IWELL,Ierror)
           !      This should be the same as the above equation.
           fgk = (C14dat(2)-0.2D0*fraction-C14dat(1)) &
                *(c1-cm/ct*C14dat(4)-(1-cm/ct)*c2) &
                /(c2-fraction-C14dat(4))
        END IF
        C14 = (1-cm/ct)*C14dat(2)+cm/ct*C14dat(1)+fgk
        C14 = (C14*Dbdata(i,41)+Dbdata(i,42)*Dbdata(i,46)+Dbdata(i,43) &
             *Dbdata(i,47))/Dbdata(i,1)

     ELSE IF (IWHICH.EQ.8) THEN
        ! Eichinger
        CALL CFRACT(fract7,7,IWELL,Ierror)
        CALL CFRACT(fract6,6,IWELL,Ierror)
        CALL CFRACT(fract4,4,IWELL,Ierror)
        CALL CFRACT(fract3,3,IWELL,Ierror)
        !   Use CT instead of TDIC
        ct = Dbdata(i,38)+Dbdata(i,36)
        cm = Dbdata(i,36)/2.0D0
        !     Eqn. (6) of Eichinger is corrected (Fontes, written comm., 1991)
        c13seq = Dbdata(i,38)/ct*(C14dat(4)-fract6+fract4+fract3) &
             +Dbdata(i,36)/ct*(C14dat(4)-fract6)
        c13bex = Dbdata(i,38)/ct*(c2+fract3)+0.5D0*Dbdata(i,36) &
             /ct*(c2+fract3+C14dat(4))
        cans = (cm/ct*C14dat(1)+(1.0D0-cm/ct)*C14dat(2))*(c1-c13seq) &
             /(c13bex-c13seq)
        C14 = (cans*Dbdata(i,41)+Dbdata(i,42)*Dbdata(i,46) &
             +Dbdata(i,43)*Dbdata(i,47))/Dbdata(i,1)
     ELSE IF (IWHICH.EQ.9) THEN
        ! User-entered value
        C14 = Usera(IWELL)
     END IF
  END IF
10 RETURN
END FUNCTION C14
!
!
!
SUBROUTINE CFRACT(FRACTION,ITIME,IWELL,IERROR)
  USE max_size
  implicit none
  DOUBLE PRECISION FRACTION
  INTEGER ITIME, IWELL, IERROR
  !
  ! Fractionation factors for the inorganic carbon species are calculated
  ! from the equations of Mook and Deines et al.  Calculations are as
  ! Alpha, and the additive fractionation factor, Epsilon, is returned.
  !
  ! A specific frationation factor is calculated, based on some fraction
  ! of the initial and final waters.
  !
  ! Meaning of ITIME 1:  Calcite-solution at some point along the flowpath
  !                  2:  CO2(g)-solution at some point along the flowpath
  !                  3:  CO2(aq)-CO2(g) at the initial well (Eichinger)
  !                  4:  CO2(g)-HCO3 at the initial well (Mook,F&G)
  !                  5:  HCO3-Calcite at the initial well (F&G)
  !                  6:  Calcite-HCO3 at the initial well (Eichinger)
  !                  7:  CO2(aq)-Calcite at the initial well (Eichinger)
  !                  8:  CO2(g)-solution at the initial well
  !
  ! IWHICH selects what values should be used for the initial water.
  ! Meaning of IWELL   -1: Use equal portions of all the initial wells.
  !                     0: Use mixing ratio determined by delta(1-x)
  !                    >0: Use just the selected well.
  !
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  DOUBLE PRECISION ach4, adoc, ch4, co3, ctot, dch4, dco2, dco2bot,  &
       dco2top, ddoc, del13, dist13, dk0, dk1, dk2, dk3,  &
       doc, eb, eba, ebc, ebcal, ebgas, frac, h2co3,  &
       hco3, t, tdic
  INTEGER i, iwhich, loopnum
  INTRINSIC DABS, DEXP
  !
  iwhich = IWELL
  IF (Iflag(1).EQ.0) iwhich = 1
  IERROR = 0
  loopnum = 1
10 IF (loopnum.NE.1) THEN
     t = Dbdata(Well(0),50)+273.15D0
     h2co3 = Dbdata(Well(0),38)
     hco3 = Dbdata(Well(0),36)
     co3 = Dbdata(Well(0),39)
     ch4 = Dbdata(Well(0),42)
     doc = Dbdata(Well(0),43)
     dco2 = 0.0D0
     IF (DABS(Dbdata(Well(0),41)).GT.1.0D-20) &
          dco2 = Dbdata(Well(0),21)/Dbdata(Well(0),41)
     dch4 = Dbdata(Well(0),44)
     ddoc = Dbdata(Well(0),45)
     !
     !  SET UP CARBON AND TEMP DATA
     !
  ELSE IF (iwhich.GT.0) THEN
     t = Dbdata(Well(iwhich),50)+273.15D0
     h2co3 = Dbdata(Well(iwhich),38)
     hco3 = Dbdata(Well(iwhich),36)
     co3 = Dbdata(Well(iwhich),39)
     ch4 = Dbdata(Well(iwhich),42)
     doc = Dbdata(Well(iwhich),43)
     dco2 = 0.0D0
     IF (DABS(Dbdata(Well(iwhich),41)).GT.1.0D-20) &
          dco2 = Dbdata(Well(iwhich),21)/Dbdata(Well(iwhich),41)
     dch4 = Dbdata(Well(iwhich),44)
     ddoc = Dbdata(Well(iwhich),45)
  ELSE
     t = 0.0D0
     h2co3 = 0.0D0
     co3 = 0.0D0
     ch4 = 0.0D0
     doc = 0.0D0
     dco2top = 0.0D0
     dco2bot = 0.0D0
     dco2 = 0.0D0
     dch4 = 0.0D0
     ddoc = 0.0D0
     DO i = 1, Iflag(1)+1
        IF (iwhich.EQ.0) THEN
           frac = Delta(i)
        ELSE
           frac = 1.0D0/(Iflag(1)+1)
        END IF
        t = t+frac*(Dbdata(Well(i),50)+273.15D0)
        h2co3 = h2co3+frac*Dbdata(Well(i),38)
        hco3 = hco3+frac*Dbdata(Well(i),36)
        co3 = co3+frac*Dbdata(Well(i),39)
        dch4 = dch4+frac*Dbdata(Well(i),44)
        ddoc = ddoc+frac*Dbdata(Well(i),45)
        IF (DABS(Dbdata(Well(i),41)).GT.1.0D-20) THEN
           dco2top = dco2top+frac*Dbdata(Well(i),21)
           dco2bot = dco2bot+frac*Dbdata(Well(i),41)
        END IF
     enddo
     IF (DABS(dco2bot).GT.1.0D-10) dco2 = dco2top/dco2bot
  END IF
  !
  ! Calculate fractionation factors (alpha)
  !
  IF (Iflag(5).EQ.0) THEN
     !                   Mook (1980, 1986) numbers
     ! CO2(aq) - HCO3
     eba = (24.12D0-9866.0D0/t)/1000.0D0+1.0D0
     ! CO3 - HCO3
     ebc = (2.52D0-867.0D0/t)/1000.0D0+1.0D0
     ! Calcite - HCO3
     ebcal = (15.10D0-4232.0D0/t)/1000.0D0+1.0D0
     ! CO2(g) - HCO3
     ebgas = (23.89D0-9483.0D0/t)/1000.0D0+1.0D0
     ! CO2(ag) - CO2(g)
     dk0 = (0.19D0-373.0D0/t)/1000.0D0+1.0D0
  ELSE
     !                  Deines, et al. (1974) numbers
     ! CO2(aq) - CO2(g)
     dk0 = DEXP((-0.91D0+6300.0D0/(t*t))/1000.0D0)
     ! HCO3 - CO2(g)
     dk1 = DEXP((-4.54D0+1099000.0D0/(t*t))/1000.0D0)
     ! CO3 - CO2(g)
     dk2 = DEXP((-3.4D0+870000.0D0/(t*t))/1000.0D0)
     ! Calcite - CO2(g)
     dk3 = DEXP((-3.63D0+1194000.0D0/(t*t))/1000.0D0)
     ! CO2(aq) - HCO3
     eba = dk0/dk1
     ! CO3 - HCO3
     ebc = dk2/dk1
     ! Calcite - HCO3
     ebcal = dk3/dk1
     ! CO2(g) - HCO3
     ebgas = 1.0D0/dk1
  END IF
  IF (ITIME.EQ.3) FRACTION = dk0
  IF (ITIME.EQ.7) FRACTION = eba/ebcal
  IF (ITIME.EQ.6) FRACTION = ebcal
  IF (ITIME.EQ.5) FRACTION = 1.0D0/ebcal
  eb = ebcal
  IF (ITIME.EQ.2 .OR. ITIME.EQ.8) eb = ebgas
  IF (ITIME.EQ.4) FRACTION = ebgas
  IF (ITIME.GT.2 .AND. ITIME.LT.8) THEN
     FRACTION = (FRACTION-1.0D0)*1000.0D0
  ELSE
     dist13 = eba*h2co3+hco3+ebc*co3
     tdic = h2co3+hco3+co3
     ctot = tdic+ch4+doc
     del13 = tdic*(1000.0D0+dco2)/dist13-1000.0D0
     ! KINETIC ALPHA DOC - HCO3
     adoc = (1000.0D0+ddoc)/(1000.0D0+del13)
     ! KINETIC ALPHA CH4 - HCO3
     ach4 = (1000.0D0+dch4)/(1000.0D0+del13)
     dist13 = ctot/(dist13+ch4*ach4+doc*adoc)
     IF (loopnum.EQ.1) THEN
        FRACTION = eb*dist13
        FRACTION = (FRACTION-1.D0)*1000.0D0
        IF (ITIME.EQ.8) RETURN
        loopnum = 2
        GO TO 10
     END IF
     FRACTION = (1.0D0-Disalong)*FRACTION+Disalong*((eb*dist13)-1.D0) &
          *1000.0D0
  END IF
  RETURN
END SUBROUTINE CFRACT
!
!
!
SUBROUTINE CONLIST(LAST)
  implicit none
  !
  ! All the constraints considered by the program are displayed, up to
  ! LAST.  Compare this with LISTCON which lists only those constraints
  ! currently under consideration.
  !
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  INTEGER i, j, LAST
  !
  WRITE (*,9000)
  j = LAST
  WRITE (*,9005) (i,Elelong(i),i=1,j)
  RETURN
9000 FORMAT (//,21X,'List of constraints',/)
9005 FORMAT (9(/,1X,4(I2,': ',A12,2X)))
END SUBROUTINE CONLIST
!
!
!
SUBROUTINE DELECON
  implicit none
  !
  ! Some or all of the constraints currently considered in the model can
  ! be removed.
  !
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  CHARACTER*3 bans
  INTEGER inoele, i, j
  EXTERNAL POSCUR, LISTCON
  !
  inoele = Noele
10 IF (Noele.EQ.0) THEN
     CALL POSCUR(-1)
     WRITE (*,9005)
     READ (*,'(A)') bans
     GOTO 40
  END IF
  CALL LISTCON(inoele)
20 WRITE (*,9000)
  READ (*,'(A)') bans
  IF (bans.NE.' ') THEN
     READ (bans,'(I3)',ERR=20) i
     IF (i.GE.1 .AND. i.LE.inoele) THEN
        IF (Iele(i).EQ.0) GO TO 10
        Iele(i) = 0
        Noele = Noele-1
        IF (Noele.NE.0) GO TO 10
     END IF
  END IF
  j = 0
  DO i = 1, inoele
     IF (Iele(i).EQ.0) THEN
        j = j+1
     ELSE
        Iele(i-j) = Iele(i)
     END IF
  enddo
40 RETURN
9000 FORMAT (//,' Enter constraint to delete. <Enter> to quit.')
9005 FORMAT (' No constraints to delete.  Press <Enter> to continue.')
END SUBROUTINE DELECON
!
!
!
SUBROUTINE DELEPHA
  USE max_size
  use filenames
  implicit none
  !
  ! Some or all of the phases currently considered in the model can be
  ! removed from consideration.
  !

  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  CHARACTER*3 bans
  INTEGER left, ii, i, j, k
  EXTERNAL POSCUR, LISTPHA
  !
  IF (Nopha.EQ.0) THEN
     CALL POSCUR(-1)
     WRITE (*,9000)
     READ (*,'(A)') bans
     GOTO 80
  END IF
  left = Nopha
10 CALL LISTPHA(ii)
  IF (ii.GT.0 .AND. ii.LE.Nopha) GO TO 30
20 WRITE (*,9005)
  READ (*,'(A)') bans
  IF (bans.EQ.' ') GO TO 40
  READ (bans,'(I3)',ERR=20) ii
  IF (ii.LT.1 .OR. ii.GT.Nopha) GO TO 20
30 Phase(ii) = ' '
  left = left-1
  IF (left.GT.0) GO TO 10
40 j = 0
  DO i = 1, Nopha
     IF (Phase(i).EQ.' ') THEN
        j = j+1
     ELSE
        Phase(i-j) = Phase(i)
        It(i-j) = It(i)
        F(i-j) = F(i)
        DO k = 1, 36
           Jele(i-j,k) = Jele(i,k)
           Pcoeff(i-j,k) = Pcoeff(i,k)
        enddo
        DO k = 1, 16
           IF (k.GT.1) Dowehave(i-j,k) = Dowehave(i,k)
           Para(i-j,k) = Para(i,k)
        enddo
     END IF
  enddo
  Nopha = left
80 RETURN
9000 FORMAT (' No phases to delete.  Hit <Enter> to continue.')
9005 FORMAT (//,' Input phase to delete. <Enter> to quit')
END SUBROUTINE DELEPHA
!
!
!
SUBROUTINE DELETE
  implicit none
  !
  ! The specific delete routines are called from this subroutine.
  !
  INTEGER i
  EXTERNAL POSCUR, DELECON, DELEPHA, SCREEN
  !
10 WRITE (*,9000)
  READ (*,'(I1)',ERR=10) i
  CALL POSCUR(-1)
  IF (i.EQ.1) CALL DELECON
  IF (i.EQ.2) CALL DELEPHA
  IF (i.EQ.1 .OR. i.EQ.2) CALL SCREEN
  RETURN
9000 FORMAT (' Delete (1) constraints, (2) phases, or (3) neither?')
END SUBROUTINE DELETE
!
!
!
SUBROUTINE DONTHAVE(I,J)
  implicit none
  !
  ! The flag indicating that a particular isotopic value has been entered
  ! is turned off, if it is not off already.  If J=1, all the flags for
  ! the given phase are turned off.
  !
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  INTEGER I, J, k
  !
  IF (J.EQ.1) THEN
     DO k = 2, 16
        Dowehave(I,k) = .FALSE.
     enddo
     Para(I,1) = 0.0D0
  ELSE
     Dowehave(I,J) = .FALSE.
  END IF
  RETURN
END SUBROUTINE DONTHAVE
!
!
!
SUBROUTINE EDIT
  USE max_size
  use filenames
  implicit none
  !
  ! The various parameters of the model (everything except
  ! constraints and phases) can be edited.
  !

  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER iex, ico2, i, j, icount, LENS
  CHARACTER*4 line
  EXTERNAL CLS, POSCUR, CLPART, WELLFILE_PAT, MODELS, EDITPHA, &
       EDITMIX,  &
       EDITEVAP, EDITIONEX, EDITXCA, EDTXCO2, EDITRS, EDITCISO, &
       ISOTDATA, EDITC14, EDITFACT, SCREEN, WELLS, LENS
  integer choice(15), max
  character*20 yn
  !
10 iex = 0
  ico2 = 0
  DO i = 1, Nopha
     IF (Phase(i).EQ.'EXCHANGE') iex = 1
     IF (Phase(i).EQ.'CO2-CH4 ') ico2 = 1
  enddo
  CALL CLS
  WRITE (*,9005)
  i = 1
  ! Excel file = 15
  if (excel_file == .true.) then
    WRITE (*,9010) i, filename(1:LENS(filename))
    choice(i) = 15
    i = i + 1
  endif
  
  ! Well file = 1
  WRITE (*,9015) i, Wfile(1:LENS(Wfile))
  
  ! Entire model = 2
  choice(i) = 1
  i = i+1
  choice(i) = 2
  WRITE (*,9020) i
  
  ! Phases = 3
  i = i+1
  choice(i) = 3
  WRITE (*,9050) i
  
  ! Mixing = 4
  i = i+1
  choice(i) = 4
  IF (Iflag(1).GT.0) THEN
     WRITE (*,9025) i, 'Yes'
  ELSE
     WRITE (*,9025) i, 'No '
  END IF
  
  ! Wells = 5
  i = i+1
  choice(i) = 5
  IF (Iflag(1).LE.2) THEN
     WRITE (*,9030) i, Wllnms(Well(1))(5:LENS(Wllnms(Well(1))))
     DO icount = 2, Iflag(1)+1
        WRITE (*,9035) Wllnms(Well(icount)) &
             (5:LENS(Wllnms(Well(icount))))
     enddo
     WRITE (*,9040) Wllnms(Well(0))(5:LENS(Wllnms(Well(0))))
  ELSE
     WRITE (*,9045) i
  END IF
  
  ! EVAPORATION/DILUTION = 6
  i = i+1
  choice(i) = 6
  WRITE (*,9055) i, Yes(Iflag(6))
  
  ! ION EXCHANGE = 7
  ! Ca fraction = 8
  IF (iex.EQ.1) THEN
     i = i+1
     choice(i) = 7
     WRITE (*,9060) i, Ion(Iflag(2))
     IF (Iflag(2).EQ.4) THEN
        i = i+1
        choice(i) = 8
        WRITE (*,9065) i, P(2)
     END IF
  END IF
  
  ! Fraction CO2 = 9
  IF (ico2.EQ.1) THEN
     i = i+1
     choice(i) = 9
     WRITE (*,9070) i, P(1)
  END IF
  
  ! Redox state of DOC = 10
  i = i+1
  choice(i) = 10
  WRITE (*,9075) i
  
  ! EDIT CALCULATION OF RAYLEIGH ISOTOPIC NUMBERS = 11
  ! EDIT ISOTOPIC DATA = 12
  ! EDIT C14 MODEL = 13
  ! EDIT FRACTIONATION FACTORS = 14
  i = i+1 
  choice(i) = 11
  WRITE (*,9080) i, Yes(Iflag(3))
  IF (Iflag(3).EQ.1) THEN
     i = i+1
     choice(i) = 12
     WRITE (*,9085) i
     i = i+1
     choice(i) = 13
     WRITE (*,9090) i, Model(Iflag(4))
     i = i+1
     choice(i) = 14
     WRITE (*,9095) i, Ffact(Iflag(5))
  END IF
  max = i
  !  This line needed to have all the poscur calls work correctly
  IF (Iflag(1).LE.2) i = i+Iflag(1)+1
40 CALL POSCUR(i)
  CALL CLPART
  WRITE (*,9100)
  READ (*,'(A)') line
  IF (line.NE.' ') THEN
     READ (line,9000,ERR=40) j
     !IF (j.GT.6 .AND. iex.EQ.0) j = j+2
     !IF (j.GT.7 .AND. iex.EQ.1 .AND. Iflag(2).NE.4) j = j+1
     !IF (ico2.EQ.0 .AND. j.GT.8) j = j+1
     !IF (Iflag(3).EQ.0 .AND. j.GT.11) j = j+3
     if (j .lt. 1 .or. j .gt. max) then
        write(*,*) "Selection is not within the range of choices."
		write(*,*) "Press enter to continue."
		READ (*,'(a)') yn
		goto 10
     endif
     j = choice(j)
     IF (j.EQ.1) THEN
        ! EDIT WELL FILE
        Iedit = 2
        CALL WELLFILE_PAT(.false.)
     ELSE IF (j .EQ. 2) THEN
        ! EDIT MODEL
        Iedit = 2
        CALL MODELS214
     ELSE IF (j.EQ.3) THEN
        ! EDIT PHASES
        CALL EDITPHA(i+1)
     ELSE IF (j.EQ.4) THEN
        ! EDIT MIXING
        Iedit = 1
        CALL EDITMIX(i+1)
     ELSE IF (j.EQ.5) THEN
        ! CHANGE WELLS
        Iedit = 1
        CALL WELLS
     ELSE IF (j.EQ.6) THEN
        ! EDIT EVAPORATION/DILUTION
        CALL EDITEVAP(i+1)
     ELSE IF (j.EQ.7) THEN
        ! EDIT ION EXCHANGE
        CALL EDITIONEX(i+1)
     ELSE IF (j.EQ.8) THEN
        ! EDIT FRACTION CALCIUM IN USER-ENTERED EXCHANGE
        CALL EDITXCA(i+1)
     ELSE IF (j.EQ.9) THEN
        ! EDIT FRACITON OF CO2 IN CO2-CH4 PHASE
        CALL EDTXCO2(i+1)
     ELSE IF (j.EQ.10) THEN
        ! EDIT REDOX STATE OF DISSOLVED ORGANIC CARBON
        CALL EDITRS(i+1)
     ELSE IF (j.EQ.11) THEN
        ! EDIT CALCULATION OF RAYLEIGH ISOTOPIC NUMBERS
        CALL EDITCISO(i+1)
     ELSE IF (j.EQ.12) THEN
        ! EDIT ISOTOPIC DATA
        CALL ISOTDATA
     ELSE IF (j.EQ.13) THEN
        ! EDIT C14 MODEL
        CALL EDITC14
     ELSE IF (j.EQ.14) THEN
        ! EDIT FRACTIONATION FACTORS
        CALL EDITFACT(i+1)
     ELSE IF (j.EQ.15) THEN
        CALL REREAD_EXCEL
        ! REREAD EXCEL file
     ENDIF
     GO TO 10
  END IF
  CALL SCREEN
  RETURN
9000 FORMAT (I4)
9005 FORMAT ('  General')
9010 FORMAT (I5,') Reread Excel file       : ',A)
9015 FORMAT (I5,') Well file               : ',A)
9020 FORMAT (I5,') Entire model')
9025 FORMAT ('  Wells',/,I5,') Mixing                  : ',A)
9030 FORMAT (I5,') Initial well            : ',A)
9035 FORMAT (7X,'Initial well            : ',A)
9040 FORMAT (7X,'Final well              : ',A)
9045 FORMAT (I5,') Wells')
9050 FORMAT (I5,') Phases')
9055 FORMAT ('  Parameters',/,I5,') Evaporation/Dilution    : ',A)
9060 FORMAT (I5,') Ion exchange            : ',A)
9065 FORMAT (I5,') Fraction Ca             :',F6.3)
9070 FORMAT (I5,') Fraction CO2 in CO2-CH4 :',F6.3)
9075 FORMAT (I5,') Redox state of DOC')
9080 FORMAT ('  Isotope calculations',/,I5, &
       ') Rayleigh calculations   : ',A)
9085 FORMAT (I5,') Isotopic data')
9090 FORMAT (I5,') Model for initial C14   : ',A)
9095 FORMAT (I5,') Carbon fract. factors   : ',A)
9100 FORMAT (1X,20('-'),/,' Edit which? (<Enter> when done)')
END SUBROUTINE EDIT
!
!
!
SUBROUTINE EDITC14
  USE max_size
  implicit none
  !
  ! The model to be used for the initial Carbon-14 value is selected, and
  ! the parameters for it are modified.  Parameters for all the models
  ! may be entered because all the models may be run.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER idone, i, ierr, j, jj, ii11, ii10, LENS
  CHARACTER*38 c1words(0:3), c2words(0:3)
  CHARACTER ans*1
  DOUBLE PRECISION i10, i11
  EQUIVALENCE (C14dat(10),i10)
  EQUIVALENCE (C14dat(11),i11)
  DOUBLE PRECISION C14
  EXTERNAL CLS, INPTRL, INPTIN, C14, LENS
  INTRINSIC NINT, DBLE, DABS
  !
  DATA c1words/'Original Value                        ',  &
       'User-defined Value                    ',  &
       '                                      ',  &
       '                                      '/
  DATA c2words/'User-defined Value                    ',  &
       'Mass Balance - no fractionation       ',  &
       'Mass Balance - with fractionation     ',  &
       'Open System (gas-solution equilibrium)'/
  !
  idone = 0
10 CALL CLS
  WRITE (*,9000)
  IF (Iflag(1).EQ.0) THEN
     WRITE (*,9005)
  ELSE
     WRITE (*,9010) (j,j=1,Iflag(1)+1)
     WRITE (*,*)
  END IF
  ierr = 0
  DO j = 1, Iflag(1)+1
     IF (Dbdata(Well(j),1).LE.0D0) THEN
        WRITE (*,9015) Wllnms(Well(j))(5:36)
        ierr = 1
     END IF
  enddo
  IF (ierr.EQ.1) THEN
     WRITE (*,9020)
     READ (*,9070) ans
     RETURN
  END IF
  DO i = 1, 9
     WRITE (*,9025) i, Model(i), (C14(i,j),j=1,Iflag(1)+1)
  enddo
  IF (Iflag(4).LT.1 .OR. Iflag(4).GT.9) Iflag(4) = 1
40 IF (idone.EQ.0) WRITE (*,9035) Model(Iflag(4)) &
       (1:LENS(Model(Iflag(4))))
  IF (idone.EQ.1) WRITE (*,9030)
  READ (*,9070) ans
  IF (ans.EQ.' ' .AND. idone.EQ.1) RETURN
  IF (ans.NE.' ') THEN
     READ (ans,9040,ERR=40) i
     IF (i.EQ.0) THEN
        j = 0
        CALL CLS
        GO TO 60
     END IF
     IF (i.LE.0 .OR. i.GT.9) GO TO 40
     Iflag(4) = i
  END IF
  IF (Iflag(4).EQ.1 .OR. Iflag(4).EQ.3) THEN
     WRITE (*,9045)
     READ (*,9070) ans
     IF (ans.NE.' ') GO TO 50
     RETURN
  END IF
  WRITE (*,9050) Model(Iflag(4))(1:LENS(Model(Iflag(4))))
  idone = 1
  READ (*,9070) ans
50 CALL CLS
  j = 0
  IF (ans.EQ.' ') j = Iflag(4)
60 IF ((j.GE.4.AND.j.LE.7) .OR. j.EQ.0) THEN
     CALL INPTRL(C14dat(1), &
          'C-14 activity in carbonate minerals (% modern)')
     CALL INPTRL(C14dat(2),'C-14 activity in soil gas CO2 (% modern)' &
          )
  END IF
  IF (j.EQ.2 .OR. j.EQ.0) THEN
     CALL INPTRL(C14dat(8),'C-14 activity in dolomite (% modern)')
     CALL INPTRL(C14dat(9),'C-14 activity in calcite (% modern)')
     IF (j.EQ.2) CALL INPTRL(C14dat(2), &
          'C-14 activity in soil gas CO2 (% modern)' &
          )
  END IF
  IF ((j.GE.5.AND.j.LE.8) .OR. j.EQ.0) THEN
     ii11 = NINT(C14dat(11))
     CALL INPTIN(ii11,'C-13 (TDIC) in initial solution', &
          '   (Used only in A0 models)',c1words)
     i11 = DBLE(ii11)
     IF (DABS(i11-1.0D0).LT.1.0D-6) CALL INPTRL(C14dat(3), &
          'delta C-13 (per mil) in the solution')
     CALL INPTRL(C14dat(4), &
          'delta C-13 (per mil) in carbonate minerals')
     ii10 = NINT(C14dat(10))
     CALL INPTIN(ii10,'delta C-13 (per mil) in soil gas CO2',' ', &
          c2words)
     i10 = DBLE(ii10)
     IF (DABS(i10-0.D0).LT.1.D-6) CALL INPTRL(C14dat(5), &
          'delta C-13 (per mil) in soil gas CO2')
     IF (DABS(i10-1.D0).LT.1.D-6 .OR. DABS(i10-2.0D0).LT.1.0D-6) THEN
        CALL INPTRL(C14dat(12),'delta C-13 (per mil) in dolomite')
        CALL INPTRL(C14dat(13),'delta C-13 (per mil) in calcite')
     END IF
     IF (Iflag(1).EQ.0) THEN
        WRITE (*,9055) C14(-1,1)
     ELSE
        WRITE (*,9060) (j,C14(-1,j),j=1,Iflag(1)+1)
     END IF
     WRITE (*,9065)
     READ (*,9070) ans
  END IF
  IF (j.EQ.9) THEN
     DO jj = 1, Iflag(1)+1
        CALL INPTRL(Usera(jj), &
             'user-defined C-14 activity for '//Wllnms(Well(jj) &
             )(5:LENS(Wllnms(Well(jj)))))
     enddo
  END IF
  GO TO 10
9000 FORMAT (8X,'Initial Carbon-14, A0, (percent modern)',/,13X, &
       'for Total Dissolved Carbon',/)
9005 FORMAT (9X,'Model',13X,'Initial well',/)
9010 FORMAT (9X,'Model',13X,6(:,4X,'Init',I2))
9015 FORMAT (' Carbon not positive for ''',A32,'''.')
9020 FORMAT (/, &
       ' Carbon isotopes cannot be run. Hit <Enter> to continue.')
9025 FORMAT (I4,' : ',A,':',6(F10.2))
9030 FORMAT (/,' Enter number of model to use (<Enter> to quit, 0 to', &
       ' edit data for all models)')
9035 FORMAT (/,' Enter number of model to use (<Enter> for ''',A,''')')
9040 FORMAT (I3)
9045 FORMAT (/,' Hit <Enter> to quit or any other key to edit data', &
       ' for all models.')
9050 FORMAT (/,' Hit <Enter> to input data for ''',A,''',',/, &
       ' any other key to enter data for all models.')
9055 FORMAT (' C-13 of CO2 gas for initial well: ',F8.3)
9060 FORMAT (' C-13 of CO2 gas for initial well',I2,': ',F8.3)
9065 FORMAT (' Hit <Enter> to continue')
9070 FORMAT (A)
END SUBROUTINE EDITC14
!
!
!
SUBROUTINE EDITCISO(IPOS)
  USE max_size
  implicit none
  INTEGER IPOS
  !
  ! This subroutine is called as a result of Edit-Isotopes.
  ! Rayleigh calculations can be selected.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  CHARACTER*1 ans, UPCS
  EXTERNAL POSCUR, CLPART, UPCS
  !
  CALL POSCUR(IPOS)
  CALL CLPART
  WRITE (*,9000) Yes(Iflag(3))
  READ (*,'(A)') ans
  IF (UPCS(ans).EQ.'Y') Iflag(3) = 1
  IF (UPCS(ans).EQ.'N') Iflag(3) = 0
  RETURN
9000 FORMAT (' Do Rayleigh calculations? (<Enter> for ',A3,')')
END SUBROUTINE EDITCISO
!
!
!
SUBROUTINE EDITEVAP(IPOS)
  USE max_size
  implicit none
  !
  ! Evaporation can be considered.  This also includes dilution.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER IPOS, i
  LOGICAL YN
  EXTERNAL POSCUR, CLPART, YN
  !
  CALL POSCUR(IPOS)
  CALL CLPART
  WRITE (*,9000) Yes(Iflag(6))
  i = 0
  IF (YN(Yes(Iflag(6))(1:1))) i = 1
  Iflag(6) = i
  RETURN
9000 FORMAT (' Do you wish to consider evaporation?  <Enter> for ',A)
END SUBROUTINE EDITEVAP
!
!
!
SUBROUTINE EDITFACT(IPOS)
  USE max_size
  implicit none
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER IPOS, j
  CHARACTER*1 ans
  EXTERNAL POSCUR, CLPART
  !
10 CALL POSCUR(IPOS)
  CALL CLPART
  WRITE (*,9000) (Ffact(j),j=0,1), Ffact(Iflag(5))
  READ (*,'(A)') ans
  IF (ans.NE.' ') THEN
     READ (ans,'(I1)',ERR=10) j
     IF (j.LT.1 .OR. j.GT.2) GO TO 10
     Iflag(5) = j-1
  END IF
  RETURN
9000 FORMAT (' Enter fractionation factors to use: 1) ',A4,' or 2) ', &
       A14,/,' <Enter> for ',A)
END SUBROUTINE EDITFACT
!
!
!
SUBROUTINE EDITIONEX(IPOS)
  USE max_size
  implicit none
  !
  ! The specific exchange to be used under the general EXCHANGE phase is
  ! selected.  If Variable Ca/Mg is selected, the fraction of Ca in the
  ! exchange is entered.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER IPOS, i, i2
  CHARACTER*2 ans
  EXTERNAL POSCUR, CLPART, EDITXCA
  !
10 CALL POSCUR(IPOS)
  CALL CLPART
  WRITE (*,9000) (Ion(i),i=1,4), Ion(Iflag(2))
  READ (*,'(A)') ans
  IF (ans.NE.' ') THEN
     READ (ans,'(I2)',ERR=10) i2
     Iflag(2) = i2
  END IF
  IF (Iflag(2).EQ.4) CALL EDITXCA(IPOS)
  RETURN
9000 FORMAT (' Select exchange: (1) ',A8,', (2) ',A5,', (3) ',A5, &
       ', or (4) ',A10,/,' <Enter> for ',A10)
END SUBROUTINE EDITIONEX
!
!
!
SUBROUTINE EDITMIX(IPOS)
  USE max_size
  implicit none
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER IPOS, i, ino
  LOGICAL YN
  CHARACTER bans*3, yndef*1
  EXTERNAL POSCUR, CLPART, CLS, WLLIST, YN
  !
  CALL POSCUR(IPOS)
  CALL CLPART
  IF (Iflag(1).EQ.0) THEN
     WRITE (*,9000) Yes(0)
     yndef = 'N'
  ELSE
     WRITE (*,9000) Yes(1)
     yndef = 'Y'
  END IF
  IF (YN(yndef)) THEN
10   WRITE (*,9005)
     READ (*,'(A)',ERR=10) bans
     READ (bans,'(I3)',ERR=10) i
     IF (i.LT.2 .OR. i.GT.5) THEN
        WRITE (*,9010)
        GO TO 10
     END IF
     Iflag(1) = i-1
  ELSE
     Iflag(1) = 0
     DO ino = 2, 5
        Well(ino) = 0
     enddo
  END IF
  CALL CLS
  DO i = 1, Iflag(1)+1
     IF (Well(i).EQ.0) CALL WLLIST(i)
  enddo
  IF (Well(0).EQ.0) CALL WLLIST(0)
  RETURN
9000 FORMAT (' Is this a mixing problem? <Enter> for ',A3)
9005 FORMAT (' How many wells to mix?')
9010 FORMAT (' Mixing can use between 2 and 6 wells.')
END SUBROUTINE EDITMIX
!
!
!
SUBROUTINE EDITPHA(IPOS)
  USE max_size
  use filenames
  implicit none
  !
  ! The phase to be edited is selected and ADDPHA is called.  The actual
  ! editing occurs in either ADDPHA or EDITPIEC.
  !

  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER IPOS, ij, ie, istop
  CHARACTER*3 bans
  EXTERNAL POSCUR, LISTPHA, CLS, EDITPIEC, TRANS, ADDPHA
  !
  IF (Nopha.EQ.0) THEN
     CALL POSCUR(IPOS)
     WRITE (*,9000)
     READ (*,'(A3)') bans
     GOTO 30
  END IF
  istop = 0
  CALL LISTPHA(ij)
  IF (ij.EQ.0) THEN
10   WRITE (*,9005)
     READ (*,'(A3)') bans
     IF (bans.EQ.' ') GO TO 30
     READ (bans,'(I3)',ERR=10) ij
     IF (ij.LE.0 .OR. ij.GT.Nopha) GO TO 10
  END IF
  Iedit = 1
20 CALL CLS
  IF (Flin.EQ.1) THEN
     CALL EDITPIEC(ij,istop)
     GOTO 30
  END IF
  WRITE (*,9010) Phase(ij)
  READ (*,'(A3)') bans
  IF (bans.EQ.'   ') GOTO 30
  READ (bans,'(I1)',ERR=20) ie
  IF (ie.LT.1 .OR. ie.GT.3) GO TO 20
  IF (ie.EQ.2) THEN
     CALL EDITPIEC(ij,istop)
  ELSE IF (ie.EQ.3) THEN
     CALL TRANS(ij)
  ELSE
     CALL ADDPHA(ij)
  END IF
30 RETURN
9000 FORMAT (' No phases to edit.  Hit <Enter> to continue.')
9005 FORMAT (/,' Input phase to edit.  <Enter to abort>')
9010 FORMAT (' Phase editing choices for ',A,/,6X, &
       '1) Replace phase entirely',/,6X, &
       '2) Edit phase composition',/,6X, &
       '3) Edit forcing and dissolution/precipitation only',/, &
       ' Enter your choice, or <Enter> to abort the edit:')
END SUBROUTINE EDITPHA
!
!
!
SUBROUTINE EDITPIEC(IJ,ISTOP)
  USE max_size
  use filenames
  implicit none
  INTEGER IJ, ISTOP
  !
  !   A new phase is entered from scratch or the name and constraints of a
  !   current phase may be edited. ISTOP is only used when entering wells
  !   from scractch (no netpath.dat file).
  !

  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER LENS, i, num, ii, j, noldph
  DOUBLE PRECISION coef
  CHARACTER middle*8, bans*3, ans*1, UPCS*1
  EXTERNAL DONTHAVE, CLS, UPCS, CONLIST, TRANS, SAVEOTHER, LENS
  !
  IF (Iedit.EQ.0) THEN
     WRITE (*,9000)
     READ (*,9010) middle
     IF (middle.EQ.' ') THEN
        ISTOP = 1
        RETURN
     END IF
     Phase(IJ) = middle
     CALL DONTHAVE(IJ,1)
     Jele(IJ,1) = 0
  ELSE
     WRITE (*,9005) Phase(IJ)
     READ (*,9010) middle
     IF (middle.NE.' ') Phase(IJ) = middle
  END IF
10 i = 0
20 i = i+1
  IF (Jele(IJ,i).NE.0) GO TO 20
  num = i-1
  CALL CLS
  WRITE (*,9015) Phase(IJ)
  DO i = 1, num
     WRITE (*,9020) i, Elelong(Jele(IJ,i)), Pcoeff(IJ,i)
  enddo
  IF (num.NE.0) THEN
     WRITE (*,9025)
     READ (*,9010) ans
     ans = UPCS(ans)
     IF (ans.EQ.' ') THEN
        IF (Iedit.EQ.0) THEN
           It(IJ) = ' '
           F(IJ) = ' '
        END IF
        noldph = Nopha
        CALL TRANS(IJ)
        IF (noldph.EQ.Nopha) CALL SAVEOTHER(IJ)
        RETURN
     END IF
     IF (ans.EQ.'A' .AND. num.EQ.16) GO TO 10
     IF (ans.EQ.'A') GO TO 35
     IF (ans.EQ.'D') GO TO 80
     IF (ans.EQ.'E') GO TO 100
     GO TO 10
  END IF
35 CALL CONLIST(20)
40 IF (num.GE.16) GO TO 65
  WRITE (*,9030)
  READ (*,9010) bans
  IF (bans.EQ.'?') GO TO 35
  IF (bans.EQ.' ') GO TO 65
  READ (bans,'(I3)',ERR=40) ii
  IF (ii.LT.1 .OR. ii.GT.20) GO TO 35
  i = 0
50 i = i+1
  IF (Jele(IJ,i).EQ.ii) THEN
     WRITE (*,9035)
     GO TO 40
  END IF
  IF (Jele(IJ,i).NE.0) GO TO 50
  num = num+1
  Jele(IJ,num) = ii
  Jele(IJ,num+1) = 0
60 WRITE (*,9040) Elelong(ii)(:LENS(Elelong(ii))), Phase(IJ) &
       (:LENS(Phase(IJ)))
  READ (*,'(F20.0)',ERR=60) coef
  Pcoeff(IJ,i) = coef
  GO TO 40
65 i = 0
70 i = i+1
  IF (Jele(IJ,i).NE.0) GO TO 70
  num = i-1
  GO TO 10
80 WRITE (*,9045)
  READ (*,9010) bans
  IF (bans.EQ.' ') GO TO 10
  READ (bans,'(I3)',ERR=80) ii
  IF (ii.LT.1 .OR. ii.GT.num) GO TO 80
  DO i = ii, num-1
     Jele(IJ,i) = Jele(IJ,i+1)
     Pcoeff(IJ,i) = Pcoeff(IJ,i+1)
  enddo
  Jele(IJ,num) = 0
  num = num-1
  GO TO 10
100 WRITE (*,9050)
  READ (*,9010) bans
  IF (bans.EQ.' ') GO TO 10
  READ (bans,'(I3)',ERR=100) ii
  IF (ii.LT.1 .OR. ii.GT.num) GO TO 100
  CALL CONLIST(20)
110 WRITE (*,9055) Elelong(Jele(IJ,ii))
  READ (*,9010) bans
  IF (bans.NE.' ') THEN
     READ (bans,'(I3)',ERR=110) i
     IF (i.LT.1 .OR. i.GT.20) GO TO 110
     j = 0
120  j = j+1
     IF (j.EQ.ii) GO TO 120
     IF (Jele(IJ,j).EQ.i) THEN
        WRITE (*,9060)
        GO TO 110
     END IF
     IF (Jele(IJ,j).NE.0) GO TO 120
     Jele(IJ,ii) = i
  END IF
130 WRITE (*,9065) Elelong(Jele(IJ,ii)), Pcoeff(IJ,ii)
  READ (*,9010) middle
  IF (middle.NE.' ') THEN
     READ (middle,'(F8.0)',ERR=130) coef
     Pcoeff(IJ,ii) = coef
  END IF
  GO TO 10
9000 FORMAT (/,' Enter phase name. <Enter> to abort.')
9005 FORMAT (/,' Enter phase name. <Enter> for ''',A,'''.')
9010 FORMAT (A)
9015 FORMAT ('    Data for phase ''',A,'''.',/)
9020 FORMAT (I3,': ',A12,'............',F8.3)
9025 FORMAT (/, &
       ' <A>dd, <D>elete, or <E>dit a constraint, <Enter> to continue.')
9030 FORMAT (/,' Input number of constraint to add. <Enter> to stop,', &
       '''?'' for the list.')
9035 FORMAT (/,' ERROR - Already a coefficient.')
9040 FORMAT (/,' Input stoichiometric coefficient of ',A,' in ',A,'.')
9045 FORMAT (/,' Input number of constraint to delete. <Enter> to quit' &
       )
9050 FORMAT (/,' Which coefficient to edit? <Enter> to quit')
9055 FORMAT (/,' Which constraint to include? <Enter> keeps ''',A, &
       '''.')
9060 FORMAT (' ERROR - Already included in this phase.')
9065 FORMAT (' Input coefficient for ',A,/,' <Enter> for ',F7.2)
END SUBROUTINE EDITPIEC
!
!
!
SUBROUTINE EDITRS(IPOS)
  USE max_size
  implicit none
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER IPOS, LENS, j
  DOUBLE PRECISION adocrs
  CHARACTER*1 UPCS, ans
  CHARACTER*10 line
  EXTERNAL POSCUR, CLPART, LENS, UPCS
  !
  j = 0
10 j = j+1
20 CALL POSCUR(IPOS)
  CALL CLPART
  IF (Nodata(Well(j),48).EQ.0) THEN
     IF (Isdocrs(j).EQ.0) THEN
        WRITE (*,9000) Wllnms(Well(j))(5:LENS(Wllnms(Well(j)))),  &
             Dbdata(Well(j),48)
     ELSE
        WRITE (*,9005) Wllnms(Well(j))(5:LENS(Wllnms(Well(j)))),  &
             Dbdata(Well(j),48), Dbdata(Well(j),49)
     END IF
  ELSE IF (Isdocrs(j).EQ.0) THEN
     WRITE (*,9010) Wllnms(Well(j))(5:LENS(Wllnms(Well(j))))
  ELSE
     WRITE (*,9015) Wllnms(Well(j))(5:LENS(Wllnms(Well(j)))),  &
          Dbdata(Well(j),49)
  END IF
  READ (*,'(A1)') ans
  ans = UPCS(ans)
  IF (ans.NE.' ' .AND. ans.NE.'Y' .AND. ans.NE.'N') GO TO 20
  IF (ans.EQ.'N') THEN
     Isdocrs(j) = 0
     GO TO 40
  END IF
  IF (ans.NE.' ' .OR. Isdocrs(j).NE.0) THEN
30   CALL POSCUR(IPOS)
     CALL CLPART
     IF (Isdocrs(j).EQ.0) THEN
        WRITE (*,9020) Wllnms(Well(j))(5:)
     ELSE
        WRITE (*,9025) Wllnms(Well(j))(5:), Dbdata(Well(j),49)
     END IF
     READ (*,'(A10)') line
     IF (line.EQ.' ') THEN
        IF (Isdocrs(j).EQ.0) GO TO 30
        GO TO 40
     END IF
     READ (line,'(F10.0)',ERR=20) adocrs
     Dbdata(Well(j),49) = adocrs
     Isdocrs(j) = 1
  END IF
40 IF (j.GT.0 .AND. j.LT.Iflag(1)+1) GO TO 10
  IF (j.GT.0) THEN
     j = 0
     GO TO 20
  END IF
  RETURN
9000 FORMAT (' The original value for RS of DOC in ',A,/,'  is ',F8.2, &
       '. Do you want to enter a new value? (<Enter> = no)')
9005 FORMAT (' The original value for RS of DOC in ',A,/,'  is ',F8.2, &
       '. The user-entered value is ',F8.2,'.',/, &
       ' Do you want to use a user-entered value? (<Enter> = yes)')
9010 FORMAT (' The original value for RS of DOC in ',A,/,2X, &
       'is undefined. Do you want to enter a new value? (<Enter> = no)')
9015 FORMAT (' The original value for RS of DOC in ',A,/, &
       '  is undefined. The user-entered value is ',F8.2,'.',/, &
       ' Do you want to use a user-entered value? (<Enter> = yes)')
9020 FORMAT (' Enter value for redox state of DOC for ',A)
9025 FORMAT (' Enter value for redox state of DOC for ',A,/, &
       ' <Enter> for ',F8.2)
END SUBROUTINE EDITRS
!
!
!
SUBROUTINE EDITXCA(IPOS)
  USE max_size
  implicit none
  !
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER IPOS
  DOUBLE PRECISION p2
  CHARACTER*20 line
  EXTERNAL POSCUR, CLPART
  !
10 CALL POSCUR(IPOS)
  CALL CLPART
  IF (Iedit.EQ.0) THEN
     WRITE (*,9000)
  ELSE
     WRITE (*,9000) P(2)
  END IF
  READ (*,'(A20)') line
  IF (line.EQ.' ' .AND. Iedit.EQ.0) GO TO 10
  IF (line.NE.' ') THEN
     READ (line,'(F20.0)',ERR=10) p2
     IF (p2.LT.0.0D0 .OR. p2.GT.1.0D0) GO TO 10
     P(2) = p2
  END IF
  RETURN
9000 FORMAT (' Enter fraction Ca in Ca/Mg.',:,'  <Enter> for ',F6.3)
END SUBROUTINE EDITXCA
!
!
!
SUBROUTINE EDTXCO2(IPOS)
  USE max_size
  implicit none
  INTEGER IPOS
  !
  ! CO2 gas and CH4 gas can be considered as one phase in NETPATH.  Here,
  ! the fraction of CO2 in the mixture is entered.
  !
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION p1
  CHARACTER*20 line
  EXTERNAL POSCUR, CLPART
  !
10 CALL POSCUR(IPOS)
  CALL CLPART
  WRITE (*,9000) P(1)
  READ (*,'(A20)') line
  IF (line.NE.' ') THEN
     READ (line,'(F20.0)',ERR=10) p1
     IF (p1.LT.0.0D0 .OR. p1.GT.1.0D0) GO TO 10
     P(1) = p1
  END IF
  RETURN
9000 FORMAT (' Enter fraction CO2 in CO2-CH4.  <Enter> for ',F6.3)
END SUBROUTINE EDTXCO2
!
!
!
INTEGER FUNCTION FINDTOT(I)
  USE max_size
  !
  !  This function is needed because a reordering of wells in the well
  !  file would cause incorrect wells to be used for a model unless
  !  permanent numbers of wells are stored.  Here, the well that
  !  corresponds to a particular permanent number is located.
  !
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER I, j
  !
  j = 0
  FINDTOT = 0
10 j = j+1
  IF (Tot(j).EQ.I) THEN
     FINDTOT = j
     GOTO 20
  END IF
  IF (j.LT.Nwlls) GO TO 10
20 RETURN
END FUNCTION FINDTOT
!
!
!
SUBROUTINE GETNO(INPT,RESULT,SIGFIG,ERROR)
  implicit none
  CHARACTER*10 INPT
  DOUBLE PRECISION RESULT, a
  INTEGER ERROR, SIGFIG, i, j
  !
  ERROR = 0
  IF (INPT.EQ.' ' .OR. INPT(1:3).EQ.'***') GO TO 50
  IF (INPT(1:1).EQ.'<') THEN
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
  GO TO 60
40 ERROR = ERROR+1
50 SIGFIG = -1
  RESULT = 0.0D0
60 RETURN
END SUBROUTINE GETNO
!
!
!
SUBROUTINE GETNO214(INPT,RESULT,SIGFIG,ERROR)
  implicit none
  CHARACTER*15 INPT
  DOUBLE PRECISION RESULT, a
  INTEGER ERROR, SIGFIG, i, j
  !
  ERROR = 0
  IF (INPT.EQ.' ' .OR. INPT(1:3).EQ.'***') GO TO 50
  IF (INPT(1:1).EQ.'<') THEN
     READ (INPT(2:15),'(F9.0)',ERR=40) RESULT
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
  GO TO 60
40 ERROR = ERROR+1
50 SIGFIG = -1
  RESULT = 0.0D0
60 RETURN
END SUBROUTINE GETNO214
!
!
!
SUBROUTINE HAVE(I,J)
  implicit none
  !
  ! The flag indicating that a particular isotopic value has been entered
  ! is set, if it has not been set already.
  !
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  INTEGER I, J
  !
  Dowehave(I,J) = .TRUE.
  RETURN
END SUBROUTINE HAVE
!
!
!
SUBROUTINE ICCARBON
  USE max_size
  implicit none
  !
  ! The isotopic compositions of methane and DOC are edited for each of
  ! the currently chosen wells, but only if DOC and methane numbers were
  ! entered.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER ichave(0:5,2), ithis(6), jhave, i, j, LENS
  DOUBLE PRECISION ain
  CHARACTER*80 line
  EXTERNAL CLS, POSCUR, CLPART, LENS
  !
  jhave = 0
  DO i = 0, Iflag(1)+1
     DO j = 1, 2
        ichave(i,j) = 0
        IF (Dbdata(Well(i),41+j).GT.1.0D-6) ichave(i,j) = 1
        IF (ichave(i,j).EQ.1) jhave = 1
     enddo
  enddo
  IF (jhave.EQ.0) THEN
     IF (Nopha.LT.1) THEN
        CALL CLS
        WRITE (*,9000)
        READ (*,9045) line
     END IF
     RETURN
  END IF
30 CALL CLS
  WRITE (*,9005)
  WRITE (*,9010)
  j = 0
  DO i = 1, Iflag(1)+1
     IF (ichave(i,1).NE.0 .OR. ichave(i,2).NE.0) THEN
        j = j+1
        ithis(j) = i
        WRITE (line,9025) j, Wllnms(Well(i))(5:36)
        IF (ichave(i,1).EQ.1) WRITE (line(36:),9030) &
             Dbdata(Well(i),44),  &
             Dbdata(Well(i),46)
        IF (ichave(i,2).EQ.1) WRITE (line(58:),9030) &
             Dbdata(Well(i),45),  &
             Dbdata(Well(i),47)
        WRITE (*,9050) line(1:LENS(line))
     END IF
  enddo
  i = 0
  !  Above do loop is duplicated for final well.
  IF (ichave(i,1).NE.0 .OR. ichave(i,2).NE.0) THEN
     j = j+1
     ithis(j) = i
     WRITE (line,9025) j, Wllnms(Well(i))(5:36)
     IF (ichave(i,1).EQ.1) WRITE (line(36:),9030) &
          Dbdata(Well(i),44),  &
          Dbdata(Well(i),46)
     IF (ichave(i,2).EQ.1) WRITE (line(58:),9030) &
          Dbdata(Well(i),45),  &
          Dbdata(Well(i),47)
     WRITE (*,9050) line(1:LENS(line))
  END IF
  WRITE (*,9015)
  READ (*,9045) line
  IF (line.EQ.' ') RETURN
  READ (line,9020,ERR=30) i
  IF (i.GT.0 .AND. i.LE.j) THEN
     i = ithis(i)
     IF (ichave(i,1).EQ.1) THEN
50      CALL POSCUR(j+2)
        CALL CLPART
        WRITE (*,9035) 'Carbon-13', 'CH4', Dbdata(Well(i),44)
        READ (*,9045) line
        IF (line.NE.' ') THEN
           READ (line,9040,ERR=50) ain
           Dbdata(Well(i),44) = ain
        END IF
60      CALL POSCUR(j+2)
        CALL CLPART
        WRITE (*,9035) 'Carbon-14', 'CH4', Dbdata(Well(i),46)
        READ (*,9045) line
        IF (line.NE.' ') THEN
           READ (line,9040,ERR=60) ain
           Dbdata(Well(i),46) = ain
        END IF
     END IF
     IF (ichave(i,2).EQ.1) THEN
70      CALL POSCUR(j+2)
        CALL CLPART
        WRITE (*,9035) 'Carbon-13', 'DOC', Dbdata(Well(i),45)
        READ (*,9045) line
        IF (line.NE.' ') THEN
           READ (line,9040,ERR=70) ain
           Dbdata(Well(i),45) = ain
        END IF
80      CALL POSCUR(j+2)
        CALL CLPART
        WRITE (*,9035) 'Carbon-14', 'DOC', Dbdata(Well(i),47)
        READ (*,9045) line
        IF (line.NE.' ') THEN
           READ (line,9040,ERR=80) ain
           Dbdata(Well(i),47) = ain
        END IF
     END IF
  END IF
  GO TO 30
9000 FORMAT (/,' There are no phases for which isotipic data can', &
       ' be entered.',//,' Hit <Enter> to continue.')
9005 FORMAT (19X,'Isotopic compositions of Carbon in solution',/)
9010 FORMAT (36X,' Carbon-13  C14 %mod   Carbon-13  C14 %mod',/,' #  ', &
       'Well Name',23X,'  of CH4     of CH4     of DOC', &
       '     of DOC',/,1X,78('-'))
9015 FORMAT (/,' Enter number of well to change, <Enter> when done.')
9020 FORMAT (I10)
9025 FORMAT (I1,': ',A32)
9030 FORMAT (2F10.3)
9035 FORMAT (' Enter ',A,' value for ',A,', <Enter> for ',F10.3,'.')
9040 FORMAT (F20.0)
9045 FORMAT (A)
9050 FORMAT (1X,A)
END SUBROUTINE ICCARBON
!
!
!
SUBROUTINE INCLISO(JE,PC,I,J,IRUN)
  USE max_size
  use filenames
  implicit none
  !
  !  The isotopic values (isotopic composition and fractionation factors,
  !  where applicable) are included with a phase.  This is used to store
  !  the data with a phase when saving a model.  It is also used to
  !  include isotope data with the phases when the model is run.
  !

  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  INTEGER JE(36), I, J, IRUN, iend, k
  DOUBLE PRECISION PC(36), q
  !
  iend = J
  IF (IRUN.EQ.1 .AND. Phase(I).EQ.'CO2-CH4 ') THEN
     PC(3) = Para(I,2)*P(1)+Para(I,4)*(1.0D0-P(1))
     PC(4) = Para(I,3)*P(1)+Para(I,5)*(1.0D0-P(1))
     JE(3) = 21
     JE(4) = 22
     JE(5) = 0
     J = 4
     GOTO 20
  END IF
  DO k = 1, J
     IF (JE(k).EQ.1) THEN
        q = PC(k)
        IF (IRUN.EQ.0) q = 1
        IF (Dowehave(I,2)) THEN
           JE(iend+1) = 21
           PC(iend+1) = Para(I,2)*q
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,3)) THEN
           JE(iend+1) = 22
           PC(iend+1) = Para(I,3)*q
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,7) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 29
           PC(iend+1) = Para(I,7)
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,8) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 30
           PC(iend+1) = Para(I,8)
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,12) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 35
           PC(iend+1) = Para(I,12)
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,13) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 36
           PC(iend+1) = Para(I,13)
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Phase(I).EQ.'CO2-CH4 ') THEN
           IF (Dowehave(I,4)) THEN
              JE(iend+1) = 23
              PC(iend+1) = Para(I,4)
              iend = iend+1
              JE(iend+1) = 0
           END IF
           IF (Dowehave(I,5)) THEN
              JE(iend+1) = 24
              PC(iend+1) = Para(I,5)
              iend = iend+1
              JE(iend+1) = 0
           END IF
           IF (Dowehave(I,9)) THEN
              JE(iend+1) = 31
              PC(iend+1) = Para(I,9)
              iend = iend+1
              JE(iend+1) = 0
           END IF
           IF (Dowehave(I,10)) THEN
              JE(iend+1) = 32
              PC(iend+1) = Para(I,10)
              iend = iend+1
              JE(iend+1) = 0
           END IF
        END IF
     END IF
     IF (JE(k).EQ.2) THEN
        q = PC(k)
        IF (IRUN.EQ.0) q = 1
        IF (Dowehave(I,4)) THEN
           JE(iend+1) = 23
           PC(iend+1) = Para(I,4)*q
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,9) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 31
           PC(iend+1) = Para(I,9)
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,14) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 37
           PC(iend+1) = Para(I,14)
           iend = iend+1
           JE(iend+1) = 0
        END IF
     END IF
     IF (JE(k).EQ.15) THEN
        q = PC(k)
        IF (IRUN.EQ.0) q = 1.0D0
        IF (Dowehave(I,5)) THEN
           JE(iend+1) = 24
           PC(iend+1) = Para(I,5)*q
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,10) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 32
           PC(iend+1) = Para(I,10)
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,15) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 38
           PC(iend+1) = Para(I,15)
           iend = iend+1
           JE(iend+1) = 0
        END IF
     END IF
     IF (JE(k).EQ.18) THEN
        q = PC(k)
        IF (IRUN.EQ.0) q = 1
        IF (Dowehave(I,6)) THEN
           JE(iend+1) = 25
           PC(iend+1) = Para(I,6)*q
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,11) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 33
           PC(iend+1) = Para(I,11)
           iend = iend+1
           JE(iend+1) = 0
        END IF
        IF (Dowehave(I,16) .AND. IRUN.EQ.0) THEN
           JE(iend+1) = 39
           PC(iend+1) = Para(I,16)
           iend = iend+1
           JE(iend+1) = 0
        END IF
     END IF
  enddo
  J = iend
20 RETURN
END SUBROUTINE INCLISO
!
!
!
SUBROUTINE INITVALS(INEW)
  USE max_size
  USE IFWIN
  use filenames
  implicit none
  !
  ! Variables that need initial values are initialized (and probably some
  ! that don't).  Also, the data from netpath.dat, if any, are read in.
  !

  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  CHARACTER*360 Fline(100)
  COMMON /CHAR7 / Fline
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  DOUBLE PRECISION Pcoef
  COMMON /DP6   / Pcoef(39,39)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER i, iii, kkk, lll, jjj, INEW, k
  LOGICAL YN
  EXTERNAL YN
  character*200 dbname, exename
  character*1 ans
  !
  IF (INEW.EQ.1) GO TO 50
  OPEN (Runit,FILE='netpath.dat',STATUS='OLD',ERR=110)
  i = 1
10 READ (Runit,'(A)',END=30) Fline(i)
  IF (Fline(i)(1:10).EQ.' ') THEN
     i = i-1
     k = 0
20   k = k+1
     IF (Fline(i)(1+10*k:2+10*k).NE.' ') GO TO 20
     Fline(i) = Fline(i)(1:10*k)//Fline(i+1)(11:80)
  END IF
  i = i+1
  IF (i.LE.100) GO TO 10
30 Flin = i-1
40 IF (Flin.EQ.0) THEN
     Fline(1) = '*OTHER**'
     Flin = 1
  END IF
  IF (Fline(Flin)(1:8).NE.'*OTHER**') THEN
     Flin = Flin+1
     Fline(Flin) = '*OTHER**'
  END IF
  CLOSE (Runit)
!  Wfile = ' '
50 Iedit = 0
  Noele = 0
  Nopha = 0
  Iflag(1) = 0
  Iflag(2) = 3
  Iflag(3) = 0
  Iflag(4) = 1
  Iflag(5) = 0
  Iflag(6) = 0
  Usera(1) = 100.0D0
  Usera(2) = 100.0D0
  Usera(3) = 100.0D0
  Usera(4) = 100.0D0
  Usera(5) = 100.0D0
  Evap = 0.0D0
  DO iii = 1, 39
     DO kkk = 1, 16
        Para(iii,kkk) = 0.0D0
     enddo
     DO lll = 1, 36
        Jele(iii,lll) = 0
     enddo
     DO jjj = 1, 39
        Pcoef(iii,jjj) = 0.0D0
        Pelt(iii,jjj) = '  '
     enddo
  enddo
  Efile = ' '
  root = ' '
  P(1) = 1.0D0
  P(2) = 0.0D0
  P(3) = 0.0D0
  Disalong = 1.0D0
  DO i = 0, 5
     Well(i) = 0
     Isdocrs(i) = 0
  enddo
  Wllnms(0) = '    *UNDEFINED*'
  RETURN
  
  ! find netpath.dat file
110 continue
   write (*,*) "No Netpath.dat file found in working directory."

   ! Get name of executable
   i = GetModuleFileName (GetModuleHandle(NULL_CHARACTER), exename, 200)

   ! Strip netpathxl.exe off end
   i = index(exename, "\", .TRUE.)
   dbname = exename(1:i) // "..\database\netpath.dat"
   
   ! test if file exists
   OPEN (Runit,FILE=dbname,STATUS='OLD',ERR=200)
   
   ! copy file relative to executable  
   i = copyfile(dbname, "netpath.dat", 1)  
   write (*,*) "Netpath.dat file copied from ", trim(dbname)
   write(*,*) "Return to continue"
   read(*,"(A)") ans
   goto 10
   
   ! Exit, could not find netpath.dat
 200 continue
   write(*,*) "Could not find Netpath.dat in working directory or ", trim(dbname)
   write(*,*) "Execution terminating."
   read(*,"(A)") ans
   stop
END SUBROUTINE INITVALS
!
!
!
SUBROUTINE INPTIN(CNUM,PROMPT,PRMPT2,CHOICES)
  implicit none
  !
  ! Allows selection of one of a group of choices.  Called by EDITC14.
  !
  INTEGER CNUM, LENS, i, j, max
  CHARACTER*38 CHOICES(0:3)
  CHARACTER*(*) PROMPT, PRMPT2
  CHARACTER*1 ans
  EXTERNAL LENS
  !
10 WRITE (*,9000) PROMPT
  i = LENS(PRMPT2)
  IF (i.GT.0) WRITE (*,'(A)') PRMPT2(1:i)
  max = -1
20 max = max+1
  IF (max.LE.3) then
    if (LENS(CHOICES(max)).GT.0) GO TO 20
  endif
  max = max-1
  DO j = 0, max
     WRITE (*,9005) j, CHOICES(j)
  enddo
  WRITE (*,9010) PROMPT, CHOICES(CNUM)(1:LENS(CHOICES(CNUM)))
  READ (*,'(A1)') ans
  IF (ans.NE.' ') THEN
     READ (ans,'(I1)',ERR=10) i
     IF (i.LT.0 .OR. i.GT.max) GO TO 10
     CNUM = i
  END IF
  RETURN
9000 FORMAT (' Choices for ',A)
9005 FORMAT (I3,' : ',A)
9010 FORMAT (/' Choose method for defining ',A,',',/, &
       ' <Enter> to use ',A,'.')
END SUBROUTINE INPTIN
!
!
!
SUBROUTINE INPTRL(CNUM,PROMPT)
  implicit none
  DOUBLE PRECISION CNUM
  CHARACTER*(*) PROMPT
  !
  ! A real value is input following the display of the proper prompt.
  ! Called by EDITC14.
  !
  CHARACTER*20 line
  DOUBLE PRECISION c
10 WRITE (*,9000) PROMPT, CNUM
  READ (*,'(A)') line
  IF (line.NE.' ') THEN
     READ (line,'(F20.0)',ERR=10) c
     CNUM = c
  END IF
  RETURN
9000 FORMAT (/' Enter value of ',A,',',/,' <Enter> for',F10.2)
END SUBROUTINE INPTRL
!
!
!
SUBROUTINE ISOTDATA
  USE max_size
  use filenames
  implicit none
  !
  ! This subroutine is called as a result of Edit-Isotope Data
  ! Isotopic compositions and fractionation factors of applicable phases
  ! may be entered. In addition, C-13 and C-14 data for CH4 and DOC in the
  ! wells is also entered here.
  !

  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  INTEGER iccc(39), LENS, ISTATE, itime, ico2, i, j, jco2, ia1, ia2,  &
       ia3, iwhich, ist, isulf, elem(5), ia4, ibegin, iend, &
       imult, ij, k
  DOUBLE PRECISION fraction, aa, fract1, fract, cth, fmax, fmin
  CHARACTER*43 title(0:1)
  CHARACTER*29 title2(0:1)
  CHARACTER*10 subline
  CHARACTER*9 label(4)
  CHARACTER*3 name(4)
  CHARACTER*7 sulf(2)
  CHARACTER*80 line2, line, UPCS80, blank
  CHARACTER*4 line4
  EXTERNAL CLS, CFRACT, SFRACT, HAVE, ICCARBON, POSCUR, CLPART,  &
       UPCS80, DONTHAVE, ISTATE, LENS
  DATA elem/1, 1, 2, 15, 18/
  DATA title/'            Isotopic Compositions          ',  &
       'Additive Fractionation Factors (in per mil)'/
  DATA title2/'isotopic composition         ',  &
       'additive fractionation factor'/
  DATA label/'Carbon-13', 'Carbon-14', 'Carbon-13', 'Carbon-14'/
  DATA name/'CO2', 'CO2', 'CH4', 'CH4'/
  DATA sulf/'sulfide', 'sulfate'/
  !
  itime = 0
10 CALL CLS
  Inum = 0
  ibegin = 0
  iend = 0
  ico2 = 0
  DO i = 1, Nopha
     IF (itime.EQ.1 .AND. It(i).EQ.'+') cycle
     IF (Phase(i).NE.'CO2-CH4 ') THEN
        WRITE (line,9000) Inum+1, Phase(i)
        blank = line
        j = 0
20      j = j+1
        IF (Jele(i,j).NE.0) THEN
           DO k = 1, 5
              IF (Jele(i,j).EQ.elem(k)) THEN
                 subline = ' Undefined'
                 IF ((k.NE.3.OR.itime.EQ.0) .AND.  &
                      Dowehave(i,1+k+5*itime)) THEN
                    WRITE (subline,9005) Para(i,1+k+5*itime), ' '
                 ELSE IF (itime.EQ.1 .AND. k.LE.3) THEN
                    jco2 = 1
                    IF (Phase(i)(1:3).EQ.'CO2') jco2 = 2
                    IF (k.LE.2 .AND. (ISTATE(i,1).EQ.4.OR.jco2.EQ.2)) THEN
                       CALL CFRACT(fraction,jco2,-1,Ierror)
                       IF (Ierror.EQ.0) THEN
                          WRITE (subline,9005) k*fraction, '*'
                       ELSE IF (Ierror.EQ.2) THEN
                          WRITE (subline,9005) k*fraction, '#'
                          ibegin = 1
                       ELSE IF (Ierror.EQ.3) THEN
                          WRITE (subline,9005) k*fraction, '@'
                          iend = 1
                       END IF
                    ELSE IF (k.EQ.3 .AND. Nodata(Well(0),23).EQ.0 .AND.  &
                         Dbdata(Well(0),2).GT.0.0D0) THEN
                       IF ((ISTATE(i,2).EQ.-1.OR.ISTATE(i,2).EQ.-2) .AND.  &
                            Dbdata(Well(0),29).GT.0.0D0) THEN
                          CALL SFRACT(fraction,1,-1,Ierror)
                          IF (Ierror.NE.1) THEN
                             IF (.NOT.Dowehave(i,9)) THEN
                                CALL HAVE(i,9)
                                Para(i,9) = 0.0D0
                             END IF
                             IF (Ierror.EQ.0) THEN
                                WRITE (subline,9005) fraction+Para(i,9), '*'
                             ELSE IF (Ierror.EQ.2) THEN
                                WRITE (subline,9005) fraction+Para(i,9), '#'
                                ibegin = 1
                             ELSE IF (Ierror.EQ.3) THEN
                                WRITE (subline,9005) fraction+Para(i,9), '@'
                                iend = 1
                             END IF
                          END IF
                       ELSE IF (ISTATE(i,2).EQ.6 .AND. Dbdata(Well(0),30) &
                            .GT.0.0D0) THEN
                          CALL SFRACT(fraction,2,-1,Ierror)
                          IF (Ierror.NE.1) THEN
                             IF (.NOT.Dowehave(i,9)) THEN
                                CALL HAVE(i,9)
                                Para(i,9) = 0.0D0
                             END IF
                             IF (Ierror.EQ.0) THEN
                                WRITE (subline,9005) fraction+Para(i,9), '*'
                             ELSE IF (Ierror.EQ.2) THEN
                                WRITE (subline,9005) fraction+Para(i,9), '#'
                                ibegin = 1
                             ELSE IF (Ierror.EQ.3) THEN
                                WRITE (subline,9005) fraction+Para(i,9), '@'
                                iend = 1
                             END IF
                          END IF
                       END IF
                    ELSE IF (Dowehave(i,1+k+5*itime)) THEN
                       WRITE (subline,9005) Para(i,1+k+5*itime), ' '
                    END IF
                 END IF
                 line(3+11*k:12+11*k) = subline
              END IF
           enddo
           GO TO 20
        END IF
        IF (line.NE.blank) THEN
           Inum = Inum+1
           iccc(Inum) = i
           IF (Inum.EQ.1 .AND. itime.EQ.0) WRITE (*,9025) title(0)
           IF (Inum.EQ.1 .AND. itime.EQ.1) WRITE (*,9030) title(1)
           WRITE (*,9125) line(1:LENS(line))
        END IF
     ELSE
        ico2 = 1
        Inum = Inum+1
        iccc(Inum) = i
        IF (Inum.EQ.1 .AND. itime.EQ.0) WRITE (*,9025) title(0)
        IF (Inum.EQ.1 .AND. itime.EQ.1) WRITE (*,9030) title(1)
        WRITE (line,9000) Inum, 'CO2     '
        line2 = '    CH4'
        IF (itime.EQ.1) THEN
           CALL CFRACT(aa,2,-1,Ierror)
           IF (Dowehave(i,7)) THEN
              WRITE (line(14:23),9015) Para(i,7)
           ELSE
              IF (Ierror.EQ.0) WRITE (line(14:23),9020) aa, '*'
              IF (Ierror.EQ.1) WRITE (line(14:23),9010) ' Undefined'
              IF (Ierror.EQ.2) WRITE (line(14:23),9020) aa, '#'
              IF (Ierror.EQ.2) ibegin = 1
              IF (Ierror.EQ.3) iend = 1
              IF (Ierror.EQ.3) WRITE (line(14:23),9020) aa, '@'
           END IF
           IF (Dowehave(i,8)) THEN
              WRITE (line(25:34),9015) Para(i,8)
           ELSE
              IF (Ierror.EQ.0) WRITE (line(25:34),9020) 2*aa, '*'
              IF (Ierror.EQ.1) WRITE (line(25:34),9010) ' Undefined'
              IF (Ierror.EQ.2) WRITE (line(25:34),9020) 2*aa, '#'
              IF (Ierror.EQ.2) ibegin = 1
              IF (Ierror.EQ.3) iend = 1
              IF (Ierror.EQ.3) WRITE (line(25:34),9020) 2*aa, '@'
           END IF
           IF (Dowehave(i,9)) THEN
              WRITE (line2(14:23),9015) Para(i,9)
           ELSE
              WRITE (line2(14:23),9010) ' Undefined'
           END IF
           IF (Dowehave(i,10)) THEN
              WRITE (line2(25:34),9015) Para(i,10)
           ELSE
              WRITE (line2(25:34),9010) ' Undefined'
           END IF
           WRITE (*,9125) line(1:LENS(line))
           WRITE (*,9125) line2(1:LENS(line2))
        ELSE
           IF (Dowehave(i,2)) THEN
              WRITE (line(14:23),9015) Para(i,2)
           ELSE
              WRITE (line(14:23),9010) ' Undefined'
           END IF
           IF (Dowehave(i,3)) THEN
              WRITE (line(25:34),9015) Para(i,3)
           ELSE
              WRITE (line(25:34),9010) ' Undefined'
           END IF
           IF (Dowehave(i,4)) THEN
              WRITE (line2(14:23),9015) Para(i,4)
           ELSE
              WRITE (line2(14:23),9010) ' Undefined'
           END IF
           IF (Dowehave(i,5)) THEN
              WRITE (line2(25:34),9015) Para(i,5)
           ELSE
              WRITE (line2(25:34),9010) ' Undefined'
           END IF
           WRITE (*,9125) line(1:LENS(line))
           WRITE (*,9125) line2(1:LENS(line2))
        END IF
     END IF
  enddo
  IF (Inum.EQ.0) THEN
     CALL ICCARBON
     RETURN
  END IF
50 CALL POSCUR(Inum+ico2)
  CALL CLPART
  IF (itime.EQ.0) THEN
     WRITE (*,9035)
  ELSE
     WRITE (*,9040) Disalong
     IF (ibegin.EQ.1) WRITE (*,9045)
     IF (iend.EQ.1) WRITE (*,9050)
     WRITE (*,9055)
  END IF
  READ (*,9120,ERR=50) line
  IF (line.EQ.' ') THEN
     IF (itime.EQ.1) RETURN
     itime = 1
     CALL ICCARBON
     GO TO 10
  END IF
  READ (line,'(I5)',ERR=50) i
  IF (i.EQ.0 .AND. itime.EQ.1) GO TO 190
  IF (i.LT.1 .OR. i.GT.Inum) GO TO 50
  ia1 = 0
  ia2 = 0
  ia3 = 0
  ia4 = 0
  j = 0
60 j = j+1
  IF (Jele(iccc(i),j).EQ.0) THEN
     IF (ia1.EQ.0) GO TO 120
     iwhich = 1
  ELSE
     IF (Jele(iccc(i),j).EQ.1) ia1 = 1
     IF (Jele(iccc(i),j).EQ.2) ia2 = 1
     IF (Jele(iccc(i),j).EQ.15) ia3 = 1
     IF (Jele(iccc(i),j).EQ.18) ia4 = 1
     GO TO 60
  END IF
70 CALL POSCUR(Inum+ico2+1+itime)
  CALL CLPART
  IF (itime.NE.0) THEN
     IF (Phase(iccc(i))(1:3).EQ.'CO2' .AND. iwhich.LE.2) THEN
        IF ((iwhich/2)*2.EQ.iwhich) THEN
           imult = 2
        ELSE
           imult = 1
        END IF
        IF (Iflag(1).GT.0 .AND. Disalong.LT.1.0D0) THEN
           fmax = -999.0D0
           fmin = 999.0D0
           DO ij = 1, Iflag(1)+1
              CALL CFRACT(fract,2,ij,Ierror)
              IF (fract.LT.fmin) fmin = fract
              IF (fract.GT.fmax) fmax = fract
           enddo
           WRITE (*,9100) fmin*imult, fmax*imult
        ELSE
           CALL CFRACT(fract,2,1,Ierror)
           WRITE (*,9095) fract*imult
        END IF
     ELSE IF (ISTATE(iccc(i),1).EQ.4) THEN
        IF (Iflag(1).GT.0 .AND. Disalong.LT.1.0D0) THEN
           fmax = -999.0D0
           fmin = 999.0D0
           DO ij = 1, Iflag(1)+1
              CALL CFRACT(fract,1,ij,Ierror)
              IF (fract.LT.fmin) fmin = fract
              IF (fract.GT.fmax) fmax = fract
           enddo
           WRITE (*,9110) fmin*iwhich, fmax*iwhich
        ELSE
           CALL CFRACT(fract,1,1,Ierror)
           WRITE (*,9105) fract*iwhich
        END IF
     END IF
  END IF
100 IF (Phase(iccc(i)).EQ.'CO2-CH4 ') THEN
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          label(iwhich), name(iwhich)
  ELSE
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          label(iwhich), Phase(iccc(i))
  END IF
  IF (.NOT.((ISTATE(iccc(i),1).EQ.4.AND.itime.EQ.1).OR. &
       (Phase(iccc(i)).EQ.'CO2-CH4 '.AND.itime.EQ.1.AND.iwhich.LT.3)) &
       ) THEN
     IF (Dowehave(iccc(i),1+5*itime+iwhich)) WRITE (*,9070) &
          Para(iccc(i),1+5*itime+iwhich)
  ELSE IF (Dowehave(iccc(i),1+5*itime+iwhich)) THEN
     WRITE (*,9060) Para(iccc(i),1+5*itime+iwhich)
  ELSE
     WRITE (*,9065)
  END IF
  READ (*,9120) line
  line4 = UPCS80(line(:4))
  IF (line4.NE.' ' .OR. Phase(iccc(i)).NE.'CO2-CH4 ' .OR.  &
       iwhich.GE.3) THEN
     IF (line4.EQ.' ' .AND. .NOT.Dowehave(iccc(i),1+5*itime+iwhich) &
          .AND. ISTATE(iccc(i),1).NE.4) THEN
        CALL POSCUR(Inum+ico2+1+itime)
        CALL CLPART
        GO TO 100
     END IF
     IF (line4.EQ.' ' .AND. .NOT.Dowehave(iccc(i),1+5*itime+iwhich) &
          .AND. itime.EQ.0) THEN
        CALL POSCUR(Inum+ico2+1+itime)
        CALL CLPART
        GO TO 100
     END IF
     IF (line4.NE.' ') THEN
        IF (line4.EQ.'CALC' .AND. ISTATE(iccc(i),1).EQ.4 .AND.  &
             itime.EQ.1) THEN
           CALL DONTHAVE(iccc(i),1+5*itime+iwhich)
           GO TO 110
        END IF
        IF (line4.EQ.'CALC' .AND. Phase(iccc(i)).EQ.'CO2-CH4 ' .AND.  &
             itime.EQ.1 .AND. iwhich.LT.3) THEN
           CALL DONTHAVE(iccc(i),1+5*itime+iwhich)
           GO TO 110
        END IF
        READ (line,'(F20.0)',ERR=100) cth
        IF (itime.EQ.0) THEN
           IF (iwhich.EQ.2 .AND. cth.LT.0.0D0) GO TO 100
           IF (iwhich.NE.2 .AND. cth.LT.-1000.0D0) GO TO 100
        END IF
        Para(iccc(i),1+5*itime+iwhich) = cth
        CALL HAVE(iccc(i),1+5*itime+iwhich)
     END IF
  END IF
110 iwhich = iwhich+1
  IF (iwhich.LT.3) GO TO 70
  IF (iwhich.LT.5 .AND. Phase(iccc(i)).EQ.'CO2-CH4') GO TO 70
  !
  ! SULFUR EDITING
120 IF (ia2.EQ.0) GO TO 150
  CALL POSCUR(Inum+ico2+1+itime)
  CALL CLPART
  ist = ISTATE(iccc(i),2)
  isulf = 0
  IF (itime.EQ.1 .AND. (ist.EQ.-1.OR.ist.EQ.-2) .AND.  &
       Nodata(Well(0),23).EQ.0 .AND. Dbdata(Well(0),2).GT.0.0D0 .AND.  &
       Dbdata(Well(0),29).GT.0.0D0) isulf = 1
  IF (itime.EQ.1 .AND. ist.EQ.6 .AND. Nodata(Well(0),23).EQ.0 .AND.  &
       Dbdata(Well(0),2).GT.0.0D0 .AND. Dbdata(Well(0),30).GT.0.0D0) &
       isulf = 2
  IF (isulf.GT.0) THEN
     IF (Iflag(1).EQ.0 .OR. Disalong.GE.1.0D0) THEN
        CALL SFRACT(fract1,isulf,1,Ierror)
        IF (Ierror.NE.1) THEN
           IF (.NOT.Dowehave(iccc(i),8)) Para(iccc(i),8) = 0.0D0
           CALL HAVE(iccc(i),8)
           WRITE (*,9075) sulf(isulf), fract1
        ELSE
           isulf = 0
        END IF
     ELSE
        fmax = -999.0D0
        fmin = 999.0D0
        DO ij = 1, Iflag(1)+1
           CALL SFRACT(fract,isulf,ij,Ierror)
           IF (Ierror.NE.1 .AND. fract.LT.fmin) fmin = fract
           IF (Ierror.NE.1 .AND. fract.GT.fmax) fmax = fract
        enddo
        IF (fmin.LE.fmax) THEN
           IF (.NOT.Dowehave(iccc(i),8)) Para(iccc(i),8) = 0.0D0
           CALL HAVE(iccc(i),8)
           WRITE (*,9080) sulf(isulf), fmin, fmax
        ELSE
           isulf = 0
        END IF
     END IF
  END IF
140 IF (Dowehave(iccc(i),4+5*itime)) THEN
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          'Sulfur-34', Phase(iccc(i)),  &
          Para(iccc(i),4+5*itime)
  ELSE
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          'Sulfur-34', Phase(iccc(i))
  END IF
  IF (isulf.GT.0) WRITE (*,9085) sulf(isulf)
  READ (*,9120) line
  IF (line.NE.' ') THEN
     READ (line,'(F20.0)',ERR=140) cth
     IF (cth.LT.-1000.0D0) GO TO 140
     Para(iccc(i),4+5*itime) = cth
  ELSE IF (.NOT.Dowehave(iccc(i),4+5*itime)) THEN
     CALL POSCUR(Inum+ico2+1+itime)
     CALL CLPART
     GO TO 140
  END IF
  CALL HAVE(iccc(i),4+5*itime)
150 IF (ia3.EQ.0) GO TO 170
160 CALL POSCUR(Inum+ico2+1+itime)
  CALL CLPART
  IF (Dowehave(iccc(i),5+5*itime)) THEN
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          'Strontium-87', Phase(iccc(i)),  &
          Para(iccc(i),5+5*itime)
  ELSE
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          'Strontium-87', Phase(iccc(i))
  END IF
  READ (*,9120) line
  IF (line.NE.' ') THEN
     READ (line,'(F20.0)',ERR=160) cth
     IF (cth.LT.-1000.0D0) GO TO 160
     Para(iccc(i),5+5*itime) = cth
  ELSE IF (.NOT.Dowehave(iccc(i),5+5*itime)) THEN
     GO TO 160
  END IF
  CALL HAVE(iccc(i),5+5*itime)
170 IF (ia4.EQ.0) GO TO 10
180 CALL POSCUR(Inum+ico2+1+itime)
  CALL CLPART
  IF (Dowehave(iccc(i),6+5*itime)) THEN
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          'Nitrogen-15', Phase(iccc(i)),  &
          Para(iccc(i),6+5*itime)
  ELSE
     WRITE (*,9090) title2(itime)(1:LENS(title2(itime))),  &
          'Nitrogen-15', Phase(iccc(i))
  END IF
  READ (*,9120) line
  IF (line.NE.' ') THEN
     READ (line,'(f20.0)',ERR=180) cth
     IF (cth.LT.-1000.0D0) GO TO 180
     Para(iccc(i),6+5*itime) = cth
  ELSE IF (.NOT.Dowehave(iccc(i),6+5*itime)) THEN
     GO TO 180
  END IF
  CALL HAVE(iccc(i),6+5*itime)
  GO TO 10
190 CALL POSCUR(Inum+ico2+1+itime)
  CALL CLPART
  WRITE (*,9115) Disalong
  READ (*,9120) line
  IF (line.EQ.' ') GO TO 50
  IF (line(1:1).NE.'0' .AND. line(1:1).NE.'.' .AND. line(1:1) &
       .NE.'1') GO TO 190
  READ (line,'(F20.0)',ERR=190) cth
  IF (cth.LT.0.0D0 .OR. cth.GT.1.0D0) GO TO 190
  Disalong = cth
  GO TO 10
9000 FORMAT (I2,': ',A8)
9005 FORMAT (F8.3,A1)
9010 FORMAT (A)
9015 FORMAT (F8.3,2X)
9020 FORMAT (F8.3,A,1X)
9025 FORMAT (14X,A43,//,' Num Phase    Carbon-13  C14 % mod', &
       '  Sulfur-34    Sr-87       N-15',/,1X,67('-'))
9030 FORMAT (14X,A43,/,25X,'Relative to solution',/, &
       ' Num Phase    Carbon-13  C14 % mod', &
       '  Sulfur-34    Sr-87       N-15',/,1X,67('-'))
9035 FORMAT (/,' Enter number of phase to edit. <Enter> when done.')
9040 FORMAT (' * = based on computed value at ',F4.2, &
       ' fraction between init and final waters')
9045 FORMAT (' # = based on computed value at initial water')
9050 FORMAT (' @ = based on computed value at final water')
9055 FORMAT (/' Enter number of phase to edit, ''0'' for fraction', &
       ' along path, <Enter> when done.')
9060 FORMAT (' Type ''CALC'' to use calculated value or <Enter> for', &
       F10.4)
9065 FORMAT (' <Enter> to use calculated value.')
9070 FORMAT (' <Enter> to use',F10.4)
9075 FORMAT (' The calculated fractionation factor between ',A, &
       ' and total sulfur is',/,F11.4)
9080 FORMAT (' The calculated fractionation factor between ',A, &
       ' and total sulfur is',/,' between ',F10.4,' and ',F10.4)
9085 FORMAT (' (Value will be added to ',A,'-sulfur value)')
9090 FORMAT (' Enter ',A,' of ',A,' for ''',A,'''.',:,/' <Enter> for ', &
       F10.4)
9095 FORMAT (' The calculated additive fractionation factor for CO2 is' &
       ,/,F11.4)
9100 FORMAT (' The calculated additive fractionation factor for CO2 is' &
       ,/,' between',F10.4,' and',F10.4)
9105 FORMAT ( &
       ' The calculated additive fractionation factor for carbonates is' &
       ,/,F11.4)
9110 FORMAT ( &
       ' The calculated additive fractionation factor for carbonates is' &
       ,/,' between',F10.4,' and',F10.4)
9115 FORMAT ( &
       ' Enter fractional distance along path (0=initial,1=final)' &
       ,/,' <Enter> for ',F5.3,'.')
9120 FORMAT (A)
9125 FORMAT (1X,A)
end subroutine ISOTDATA
!
!
!
INTEGER FUNCTION ISTATE(I,J)
  USE max_size
  !
  ! The redox state of either carbon or sulfur in a given phase is
  ! calculated.  If the list of constraints is changed, this subroutine
  ! may have to be modified.
  !
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER k, I, J
  DOUBLE PRECISION rs, state, div
  INTRINSIC NINT, DABS
  !
  ISTATE = 0
  rs = 0.0D0
  k = 0
10 k = k+1
  IF (Jele(I,k).NE.0) THEN
     IF (Jele(I,k).NE.20) GO TO 10
     rs = Pcoeff(I,k)
  END IF
  state = 0.0D0
  div = 1.0D0
  k = 0
20 k = k+1
  IF (Jele(I,k).NE.0) THEN
     IF (Jele(I,k).EQ.J) THEN
        div = Pcoeff(I,k)
        GO TO 20
     END IF
     IF (Jele(I,k).EQ.1) state = state+4*Pcoeff(I,k)
     IF (Jele(I,k).EQ.2) state = state+6*Pcoeff(I,k)
     IF (Jele(I,k).EQ.16) state = state+2*Pcoeff(I,k)
     IF (Jele(I,k).EQ.17) state = state+2*Pcoeff(I,k)
     GO TO 20
  END IF
  IF (DABS(div).GE.1.0D-5) ISTATE = NINT((rs-state)/div)
  RETURN
END FUNCTION ISTATE
!
!
!
SUBROUTINE LISTCON(NELE)
  implicit none
  !
  ! The constraints currently under consideration are listed.  This is
  ! different from CONLIST, which lists all constraints that the program
  ! uses.
  !
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER NELE, i, jj, kk
  EXTERNAL CLS
  !
  CALL CLS
  WRITE (*,9000)
  i = 1
10 jj = i+1
  kk = jj+1
  IF (kk.NE.NELE) THEN
     IF (jj.EQ.NELE) THEN
        WRITE (*,9005) i, Elelong(Iele(i)), jj, Elelong(Iele(jj))
        GO TO 20
     ELSE IF (i.EQ.NELE) THEN
        WRITE (*,9010) i, Elelong(Iele(i))
        GO TO 20
     END IF
  END IF
  WRITE (*,9015) i, Elelong(Iele(i)), jj, Elelong(Iele(jj)), kk,  &
       Elelong(Iele(kk))
20 IF (i.EQ.NELE .OR. i+1.EQ.NELE .OR. i+2.EQ.NELE) RETURN
  i = i+3
  GO TO 10
9000 FORMAT (/,' List of constraints',/)
9005 FORMAT (I4,': ',A14,8X,I3,': ',A14)
9010 FORMAT (I4,': ',A14)
9015 FORMAT (I4,': ',A14,8X,I3,': ',A14,8X,I3,': ',A14)
END SUBROUTINE LISTCON
!
!
!
SUBROUTINE LISTPHA(II)
  USE max_size
  use filenames
  implicit none
  !
  ! The phases currently included in the model are listed.  This is differ
  ! from PHALIST, which lists all the phases available from netpath.dat.
  !

  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER II, i, ij, jj, kk
  CHARACTER*3 bans
  EXTERNAL CLS
  !
  II = 0
  CALL CLS
  WRITE (*,9000)
  i = 1
  ij = 1
10 jj = i+1
  kk = jj+1
  IF (i.GE.48*ij) THEN
     WRITE (*,9005)
     ij = ij+1
     READ (*,'(A)') bans
     IF (bans.NE.' ') THEN
        READ (bans,'(I3)',ERR=20) II
        RETURN
     END IF
  END IF
20 IF (kk.EQ.Nopha) THEN
     WRITE (*,9020) i, Phase(i), jj, Phase(jj), kk, Phase(kk)
  ELSE IF (jj.EQ.Nopha) THEN
     WRITE (*,9015) i, Phase(i), jj, Phase(jj)
  ELSE IF (i.NE.Nopha) THEN
     WRITE (*,9020) i, Phase(i), jj, Phase(jj), kk, Phase(kk)
  ELSE
     WRITE (*,9010) i, Phase(i)
  END IF
  IF (i.EQ.Nopha .OR. i+1.EQ.Nopha .OR. i+2.EQ.Nopha) RETURN
  i = i+3
  GO TO 10
9000 FORMAT (//,' List of phases',/)
9005 FORMAT (' Hit RETURN to see next page or enter number of phase.')
9010 FORMAT (I4,': ',A10)
9015 FORMAT (I4,': ',A10,10X,I3,': ',A10)
9020 FORMAT (I4,': ',A10,10X,I3,': ',A10,10X,I3,': ',A10)
END SUBROUTINE LISTPHA
!
!
!
SUBROUTINE MODELS
  USE max_size
  use filenames
  implicit none
  !
  ! The previously stored models for the given well file are displayed,
  ! one may be selected, and if so the data from it are read in.
  !

  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  DOUBLE PRECISION i10, i11
  EQUIVALENCE (C14dat(10),i10)
  EQUIVALENCE (C14dat(11),i11)
  CHARACTER*7 del(0:1)
  CHARACTER*2 short, pelem(36)
  CHARACTER*8 phases(39), middle
  CHARACTER mid*10, rsline*6
  CHARACTER*80 files(100), ofiles(100), line, UPCS80
  CHARACTER*1 UPCS, ans
  INTEGER idel(100), ielem(39), i, nfiles, nofiles, ij, icount,  &
       noelem, iic, j, li, nophas, ict, jo, k, FINDTOT, LENS, jj
  EXTERNAL UPCS80, CLS, INITVALS, DONTHAVE, HAVE, WELLS, UPCS,  &
       FINDTOT, LENS
  INTEGER tmpwell(0:5)
  DATA del/'       ', 'deleted'/
  !
  IF (Iedit.EQ.0) Noele = 0
  i = 0
  DO i = 1, 39
     ielem(i) = 1
     phases(i) = ' '
  enddo
  OPEN (UNIT=Runit,FILE='model.fil',STATUS='OLD',ERR=270)
  REWIND (Runit)
  nfiles = 0
  nofiles = 0
  READ (Runit,9000,ERR=30,END=270) line
  IF (line.EQ.' ') GO TO 30
20 OPEN (Wunit,FILE=line,STATUS='OLD',ERR=30)
  READ (Wunit,9000,ERR=30,END=30) root
  CLOSE (Wunit)
  IF (Icase.EQ.0) THEN
     root = UPCS80(root)
     Wfile = UPCS80(Wfile)
  END IF
  IF (root.NE.Wfile) THEN
     nofiles = nofiles+1
     ofiles(nofiles) = line
  ELSE
     nfiles = nfiles+1
     files(nfiles) = line
     idel(nfiles) = 0
  END IF
30 READ (Runit,9000,ERR=30,END=40) line
  GO TO 20
40 IF (nfiles.EQ.0) GO TO 240
  ij = 1
50 icount = (ij-1)*15
  CALL CLS
  WRITE (*,9005)
60 icount = icount+1
  WRITE (*,9010) icount, files(icount), del(idel(icount))
  IF (icount.LT.nfiles .AND. icount.LT.ij*15) GO TO 60
  DO i = icount, (ij*15-1)
     WRITE (*,*)
  enddo
  IF (ij*15.LT.nfiles) THEN
     ij = ij+1
  ELSE
     ij = 1
  END IF
  IF (Iedit.EQ.0) THEN
     IF (nfiles.LE.15) WRITE (*,9020)
     IF (nfiles.GT.15) WRITE (*,9015)
     Noele = 0
  ELSE
     IF (nfiles.LE.15) WRITE (*,9030)
     IF (nfiles.GT.15) WRITE (*,9025)
  END IF
  READ (*,9000) line
  IF (line.EQ.' ' .AND. Iedit.EQ.0) GO TO 50
  IF (line.EQ.' ') GO TO 240
  READ (line,'(I3)',ERR=50) i
  IF (i.EQ.0) THEN
     Iedit = 0
     CALL INITVALS(1)
     GO TO 240
  END IF
  IF (i.GT.nfiles) GO TO 40
  CALL CLS
  noelem = 0
  WRITE (*,9035) files(i), del(idel(i))
  OPEN (Wunit,FILE=files(i))
  READ (Wunit,'(1X)',END=140)
  ! GET AND PRINT WELLS
  READ (Wunit,9040,ERR=140,END=140) (tmpwell(ij),ij=1,5), tmpwell(0)
  Iflag(1) = 4
  DO ij = 5, 2, -1
     IF (tmpwell(0).EQ.0) THEN
        Iflag(1) = ij-2
        tmpwell(0) = tmpwell(ij)
        tmpwell(ij) = 0
     END IF
  enddo
  DO ij = 0, Iflag(1)+1
     tmpwell(ij) = FINDTOT(tmpwell(ij))
  enddo
  DO ij = 1, Iflag(1)+1
     WRITE (*,9045) Wllnms(tmpwell(ij))(5:36)
  enddo
  WRITE (*,9050) Wllnms(tmpwell(0))(5:36)
  ! GET AND PRINT CONSTRAINTS
  READ (Wunit,9055,END=140) line
  iic = 0
110 iic = iic+1
  READ (line(3*iic-1:3*iic),9060) short
  IF (short.EQ.'  ') THEN
     WRITE (*,9065)
     WRITE (*,9070) (Elelong(ielem(li)),li=1,noelem)
     !
     !    GET AND PRINT PHASES
     !
     nophas = 0
120  READ (Wunit,'(A10,A2)',END=140) mid, short
     IF (mid.EQ.' ' .AND. short.EQ.'  ') THEN
        WRITE (*,9075)
        WRITE (*,9080) (phases(li),li=1,nophas)
     ELSE
        IF (mid.NE.' ') THEN
           nophas = nophas+1
           phases(nophas) = mid(1:8)
        END IF
        GO TO 120
     END IF
  ELSE
     j = 0
130  j = j+1
     IF (j.EQ.29) GO TO 110
     IF (Eleshort(j).NE.short) GO TO 130
     noelem = noelem+1
     ielem(noelem) = j
     GO TO 110
  END IF
  ! DONE READING DATA
140 CLOSE (Wunit)
  IF (idel(i).NE.1) WRITE (*,9085)
  IF (idel(i).EQ.1) WRITE (*,9090)
  READ (*,9055) ans
  ans = UPCS(ans)
  IF (idel(i).EQ.1 .AND. ans.NE.'D') GO TO 40
  IF (ans.EQ.'D') THEN
     idel(i) = 1-idel(i)
     GO TO 40
  END IF
  IF (ans.EQ.'N') GO TO 40
  idel(i) = 0
  ! MAKE WELLS PERMANENT
  DO ij = 0, Iflag(1)+1
     Well(ij)=tmpwell(ij)
  enddo
  ! MAKE ELEMENTS PERMANENT
  Noele = noelem
  DO ict = 1, Noele
     Iele(ict) = ielem(ict)
  enddo
  Efile = files(i)
  ! MAKE PHASES PERMANENT
  Nopha = nophas
  DO ict = 1, Nopha
     Phase(ict) = phases(ict)
  enddo
  OPEN (UNIT=Wunit,FILE=files(i),STATUS='OLD',ERR=230)
  READ (Wunit,9055,END=220) line
  READ (Wunit,9055,END=220) line
  READ (Wunit,9055,END=220) line
  i = 0
170 i = i+1
  READ (Wunit,9055,END=220) line
  IF (line(1:12).EQ.' ') THEN
     ! MAKE PARAMETERS PERMANENT
     READ (Wunit,9095,ERR=220) (Iflag(i),i=2,6), (P(i),i=1,2),  &
          rsline, Disalong, (C14dat(i),i=1,9),  &
          i10, i11, (C14dat(i),i=12,13),  &
          (Dbdata(Well(0),i),i=44,47),  &
          Dbdata(Well(0),49),  &
          ((Dbdata(Well(j),i),i=44,47), &
          Dbdata(Well(j),49),Usera(j),j=1, &
          Iflag(1)+1)
     !   Support old model file format
     IF (Usera(1).LT.0.01) Usera(1) = C14dat(6)
     IF (Usera(2).LT.0.01) Usera(2) = C14dat(7)
     !
     ! This handles old and new style isdocrs numbers. (i.e. There used to be
     ! only 2 initial wells possible, now there are 5)
     IF (rsline(1:1).EQ.' ') THEN
        READ (rsline,9100) Isdocrs(1), Isdocrs(2), Isdocrs(0)
        Isdocrs(3) = 0
        Isdocrs(4) = 0
        Isdocrs(5) = 0
     ELSE
        READ (rsline,9105) (Isdocrs(j),j=0,5)
     END IF
     GO TO 230
  ELSE
     READ (line,9110,ERR=220,END=220) middle, F(i), It(i),  &
          (pelem(j),Pcoeff(i,j),j=1,7)
     IF (middle.EQ.' ') THEN
        i = i-1
        jo = 0
180     jo = jo+1
        IF (Jele(i,jo).NE.0) GO TO 180
        jo = jo-1
        DO j = 1, jo
           pelem(j+jo) = pelem(j)
           Pcoeff(i,j+jo) = Pcoeff(i+1,j)
        enddo
     ELSE
        Para(i,1) = 0.0D0
        CALL DONTHAVE(i,1)
        DO jj = 1, 36
           Jele(i,jj) = 0
        enddo
        jo = 0
     END IF
     j = 0
  END IF
200 j = j+1
  IF (j.EQ.8) THEN
     Pcoeff(i,j+jo) = 0.0D0
     GO TO 170
  END IF
  IF (pelem(j+jo).EQ.'  ') THEN
     Jele(i,j+jo) = 0
     GO TO 170
  END IF
  k = 0
210 k = k+1
  IF (pelem(j+jo).EQ.Eleshort(k)) THEN
     IF (k.LE.20) THEN
        Jele(i,j+jo) = k
        GO TO 200
     ELSE IF (k.LE.25 .OR. k.GE.29) THEN
        IF (k.EQ.34) THEN
           It(i) = '*'
           Para(i,1) = Pcoeff(i,j+jo)
        END IF
        IF (k.GT.34 .AND. k.LE.39) THEN
           Para(i,k-23) = Pcoeff(i,j+jo)
           CALL HAVE(i,k-23)
        END IF
        IF (k.LE.25) THEN
           Para(i,k-19) = Pcoeff(i,j+jo)
           CALL HAVE(i,k-19)
        END IF
        IF (k.GE.29 .AND. k.LE.33) THEN
           Para(i,k-22) = Pcoeff(i,j+jo)
           CALL HAVE(i,k-22)
        END IF
        Jele(i,j+jo) = 0
        GO TO 200
     END IF
  ELSE IF (k.LT.37) THEN
     GO TO 210
  END IF
  WRITE (*,'(//,i3,1x,a2,'' : '',a,1x,i2)') j+jo, pelem(j+jo),  &
       'Bad constraint in phase #', i
  STOP
220 WRITE (*,9115) Efile
  READ (*,*) line
230 CLOSE (Wunit)
  ! save model file list
240 REWIND (UNIT=Runit)
  WRITE (Runit,'(A80)') ' '
  REWIND (UNIT=Runit)
  DO j = 1, nfiles
     IF (idel(j).NE.1) WRITE (Runit,9055) files(j)(1:LENS(files(j)))
  enddo
  DO j = 1, nofiles
     WRITE (Runit,9055) ofiles(j)
  enddo
  CLOSE (Runit)
270 DO i = 0, Iflag(1)+1
     IF (Well(i).EQ.0) CALL WELLS
  enddo
  RETURN
9000 FORMAT (A80)
9005 FORMAT (/,' -----------------',/,' CHOOSE MODEL FILE',/, &
       ' -----------------',/)
9010 FORMAT (I4,':  ',A40,A7)
9015 FORMAT (/,1X, &
       'Enter number of file, 0 for none, or <ENTER> to see more files:')
9020 FORMAT (/,' Enter number of file or 0 for none:')
9025 FORMAT (/,' Enter number of file, ''M'' to see more choices, 0 to' &
       ,' reset the model',/, &
       '  or <ENTER> to keep the current model:')
9030 FORMAT (/,' Enter number of file, 0 to reset model, or <ENTER>', &
       ' to keep current model:')
9035 FORMAT (1X,40('-'),/,1X,A40,A20,/,1X,40('-'))
9040 FORMAT (6I3)
9045 FORMAT (' Initial well: ',A32)
9050 FORMAT (' Final well  : ',A32,/)
9055 FORMAT (A,A)
9060 FORMAT (A2)
9065 FORMAT (' CONSTRAINTS:')
9070 FORMAT (:,5(1X,A12,:,2X))
9075 FORMAT (/,' PHASES:')
9080 FORMAT (:,5(1X,A8,:,6X))
9085 FORMAT (/,' Use this file?  (N)o, (D)elete, or <ENTER> to accept: ', $ &
       )
9090 FORMAT (/,' Undelete this file?  un(D)elete, or <ENTER> for no: ', $)
9095 FORMAT (5(I2),2(F8.4),a6,F6.3,/,7(F8.3),/,2(F8.0),2(F8.0),2(F8.0), &
       /,5(F8.3),/,5(6(F8.3),/))
9100 FORMAT (3(i2))
9105 FORMAT (6(i1))
9110 FORMAT (A8,2(A1),7(A2,F8.4))
9115 FORMAT (//' Error in file: ',A)
END SUBROUTINE MODELS
!
!
!
SUBROUTINE MODELS214
  USE max_size
  use filenames
  implicit none
  !
  ! The previously stored models for the given well file are displayed,
  ! one may be selected, and if so the data from it are read in.
  !

  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  DOUBLE PRECISION i10, i11
  EQUIVALENCE (C14dat(10),i10)
  EQUIVALENCE (C14dat(11),i11)
  CHARACTER*7 del(0:1)
  CHARACTER*2 short, pelem(36)
  CHARACTER*8 phases(39), middle
  CHARACTER mid*10, rsline*6
  CHARACTER*80 files(100), ofiles(100), line, UPCS80
  CHARACTER*200 longline
  CHARACTER*1 UPCS, ans
  INTEGER idel(100), ielem(39), i, nfiles, nofiles, ij, icount,  &
       noelem, iic, j, li, nophas, ict, jo, k, FINDTOT, LENS, jj
  EXTERNAL UPCS80, CLS, INITVALS, DONTHAVE, HAVE, WELLS, UPCS,  &
       FINDTOT, LENS
  INTEGER tmpwell(0:5)
  DATA del/'       ', 'deleted'/
  !
  IF (Iedit.EQ.0) Noele = 0
  i = 0
  DO i = 1, 39
     ielem(i) = 1
     phases(i) = ' '
  enddo
  OPEN (UNIT=Runit,FILE='model.fil',STATUS='OLD',ERR=270)
  REWIND (Runit)
  nfiles = 0
  nofiles = 0
  READ (Runit,9000,ERR=30,END=270) line
  IF (line.EQ.' ') GO TO 30
20 continue 
  OPEN (Wunit,FILE=line,STATUS='OLD',ERR=25)
  READ (Wunit,9000,ERR=30,END=30) root
  CLOSE (Wunit)
  goto 26
25 continue
   write(*,*) "Error opening model file ", line
   root = 'xxxxxxxxxxxxxxxxxx'
26 continue
  IF (Icase.EQ.0) THEN
     root = UPCS80(root)
     Wfile = UPCS80(Wfile)
  END IF
  IF (root.NE.Wfile) THEN
     nofiles = nofiles+1
     ofiles(nofiles) = line
  ELSE
     nfiles = nfiles+1
     files(nfiles) = line
     idel(nfiles) = 0
  END IF

30 continue
  READ (Runit,9000,ERR=30,END=40) line
  GO TO 20
40 IF (nfiles.EQ.0) GO TO 240
  ij = 1
!50 icount = (ij-1)*15
  CALL CLS
  WRITE (*,9005)
!60 icount = icount+1
!  WRITE (*,9010) icount, files(icount), del(idel(icount))
!  IF (icount.LT.nfiles .AND. icount.LT.ij*15) GO TO 60
!  DO i = icount, (ij*15-1)
!     WRITE (*,*)
!  enddo
!  IF (ij*15.LT.nfiles) THEN
!     ij = ij+1
!  ELSE
!     ij = 1
!  END IF
!  IF (Iedit.EQ.0) THEN
!     IF (nfiles.LE.15) WRITE (*,9020)
!     IF (nfiles.GT.15) WRITE (*,9015)
!     Noele = 0
!  ELSE
!     IF (nfiles.LE.15) WRITE (*,9030)
!     IF (nfiles.GT.15) WRITE (*,9025)
!  END IF
!  READ (*,9000) line
!  IF (line.EQ.' ' .AND. Iedit.EQ.0) GO TO 50
!  IF (line.EQ.' ') GO TO 240
!  READ (line,'(I3)',ERR=50) i
!  IF (i.EQ.0) THEN
!     Iedit = 0
!     CALL INITVALS(1)
!     GO TO 240
!  END IF
!  IF (i.GT.nfiles) GO TO 40

  ! select model
  do icount = 1, nfiles
    WRITE (*,9010) icount, files(icount), del(idel(icount))
  enddo
  IF (Iedit.EQ.0) THEN
    WRITE (*,9020)
    Noele = 0
  ELSE
    WRITE (*,9030)
  END IF
  READ (*,9000) line
  IF (line.EQ.' ' .AND. Iedit.EQ.0) GO TO 40
  IF (line.EQ.' ') GO TO 240
  READ (line,'(I3)',ERR=40) i
  IF (i.EQ.0) THEN
     Iedit = 0
     CALL INITVALS(1)
     GO TO 240
  END IF
  IF (i.GT.nfiles .or. i .lt. 0) GO TO 40
  ! done selecting model
  
  CALL CLS
  noelem = 0
  WRITE (*,9035) files(i), del(idel(i))
  OPEN (Wunit,FILE=files(i))
  READ (Wunit,'(1X)',END=140)
  ! GET AND PRINT WELLS
  READ (Wunit,9040,ERR=140,END=140) (tmpwell(ij),ij=1,5), tmpwell(0)
  Iflag(1) = 4
  DO ij = 5, 2, -1
     IF (tmpwell(0).EQ.0) THEN
        Iflag(1) = ij-2
        tmpwell(0) = tmpwell(ij)
        tmpwell(ij) = 0
     END IF
  enddo
  DO ij = 0, Iflag(1)+1
     tmpwell(ij) = FINDTOT(tmpwell(ij))
  enddo
  DO ij = 1, Iflag(1)+1
     WRITE (*,9045) Wllnms(tmpwell(ij))(5:36)
  enddo
  WRITE (*,9050) Wllnms(tmpwell(0))(5:36)
  ! GET AND PRINT CONSTRAINTS
  READ (Wunit,9055,END=140) line
  iic = 0
110 iic = iic+1
  READ (line(3*iic-1:3*iic),9060) short
  IF (short.EQ.'  ') THEN
     WRITE (*,9065)
     WRITE (*,9070) (Elelong(ielem(li)),li=1,noelem)
     !
     !    GET AND PRINT PHASES
     !
     nophas = 0
120  READ (Wunit,'(A10,A2)',END=140) mid, short
     IF (mid.EQ.' ' .AND. short.EQ.'  ') THEN
        WRITE (*,9075)
        WRITE (*,9080) (phases(li),li=1,nophas)
     ELSE
        IF (mid.NE.' ') THEN
           nophas = nophas+1
           phases(nophas) = mid(1:8)
        END IF
        GO TO 120
     END IF
  ELSE
     j = 0
130  j = j+1
     IF (j.EQ.29) GO TO 110
     IF (Eleshort(j).NE.short) GO TO 130
     noelem = noelem+1
     ielem(noelem) = j
     GO TO 110
  END IF
  ! DONE READING DATA
140 CLOSE (Wunit)
  IF (idel(i).NE.1) WRITE (*,9085)
  IF (idel(i).EQ.1) WRITE (*,9090)
  READ (*,9055) ans
  ans = UPCS(ans)
  IF (idel(i).EQ.1 .AND. ans.NE.'D') GO TO 40
  IF (ans.EQ.'D') THEN
     idel(i) = 1-idel(i)
     GO TO 40
  END IF
  IF (ans.EQ.'N') GO TO 40
  idel(i) = 0
  ! MAKE WELLS PERMANENT
  DO ij = 0, Iflag(1)+1
     Well(ij)=tmpwell(ij)
  enddo
  ! MAKE ELEMENTS PERMANENT
  Noele = noelem
  DO ict = 1, Noele
     Iele(ict) = ielem(ict)
  enddo
  Efile = files(i)
  ! MAKE PHASES PERMANENT
  Nopha = nophas
  DO ict = 1, Nopha
     Phase(ict) = phases(ict)
  enddo
  OPEN (UNIT=Wunit,FILE=files(i),STATUS='OLD',ERR=230)
  READ (Wunit,9055,END=220) line
  READ (Wunit,9055,END=220) line
  READ (Wunit,9055,END=220) line
  i = 0
170 i = i+1
  READ (Wunit,9055,END=220) longline
  IF (longline(1:12).EQ.' ') THEN
     ! MAKE PARAMETERS PERMANENT
     READ (Wunit,9095,ERR=220) (Iflag(i),i=2,6), (P(i),i=1,2),  &
          rsline, Disalong, (C14dat(i),i=1,9),  &
          i10, i11, (C14dat(i),i=12,13),  &
          (Dbdata(Well(0),i),i=44,47),  &
          Dbdata(Well(0),49),  &
          ((Dbdata(Well(j),i),i=44,47), &
          Dbdata(Well(j),49),Usera(j),j=1, &
          Iflag(1)+1)
     !   Support old model file format
     IF (Usera(1).LT.0.01) Usera(1) = C14dat(6)
     IF (Usera(2).LT.0.01) Usera(2) = C14dat(7)
     !
     ! This handles old and new style isdocrs numbers. (i.e. There used to be
     ! only 2 initial wells possible, now there are 5)
     IF (rsline(1:1).EQ.' ') THEN
        READ (rsline,9100) Isdocrs(1), Isdocrs(2), Isdocrs(0)
        Isdocrs(3) = 0
        Isdocrs(4) = 0
        Isdocrs(5) = 0
     ELSE
        READ (rsline,9105) (Isdocrs(j),j=0,5)
     END IF
     GO TO 230
  ELSE
	! read phases
    !READ (line,'(A8,2(A1),7(A2,F8.4))',ERR=220,END=220) middle, F(i), It(i),  &
    !     (pelem(j),Pcoeff(i,j),j=1,7)
	call parse_line(longline, middle, f(i), it(i), pelem(1), pcoeff, i)
     IF (middle.EQ.' ') THEN
        i = i-1
        jo = 0
180     jo = jo+1
        IF (Jele(i,jo).NE.0) GO TO 180
        jo = jo-1
        DO j = 1, jo
           pelem(j+jo) = pelem(j)
           Pcoeff(i,j+jo) = Pcoeff(i+1,j)
        enddo
     ELSE
        Para(i,1) = 0.0D0
        CALL DONTHAVE(i,1)
        DO jj = 1, 36
           Jele(i,jj) = 0
        enddo
        jo = 0
     END IF
     j = 0
  END IF
200 j = j+1
  IF (j.EQ.8) THEN
     Pcoeff(i,j+jo) = 0.0D0
     GO TO 170
  END IF
  IF (pelem(j+jo).EQ.'  ') THEN
     Jele(i,j+jo) = 0
     GO TO 170
  END IF
  k = 0
210 k = k+1
  IF (pelem(j+jo).EQ.Eleshort(k)) THEN
     IF (k.LE.20) THEN
        Jele(i,j+jo) = k
        GO TO 200
     ELSE IF (k.LE.25 .OR. k.GE.29) THEN
        IF (k.EQ.34) THEN
           It(i) = '*'
           Para(i,1) = Pcoeff(i,j+jo)
        END IF
        IF (k.GT.34 .AND. k.LE.39) THEN
           Para(i,k-23) = Pcoeff(i,j+jo)
           CALL HAVE(i,k-23)
        END IF
        IF (k.LE.25) THEN
           Para(i,k-19) = Pcoeff(i,j+jo)
           CALL HAVE(i,k-19)
        END IF
        IF (k.GE.29 .AND. k.LE.33) THEN
           Para(i,k-22) = Pcoeff(i,j+jo)
           CALL HAVE(i,k-22)
        END IF
        Jele(i,j+jo) = 0
        GO TO 200
     END IF
  ELSE IF (k.LT.37) THEN
     GO TO 210
  END IF
  WRITE (*,'(//,i3,1x,a2,'' : '',a,1x,i2)') j+jo, pelem(j+jo),  &
       'Bad constraint in phase #', i
  STOP
220 WRITE (*,9115) Efile
  READ (*,*) line
230 CLOSE (Wunit)
  ! save model file list
240 REWIND (UNIT=Runit)
  WRITE (Runit,'(A80)') ' '
  REWIND (UNIT=Runit)
  DO j = 1, nfiles
     IF (idel(j).NE.1) WRITE (Runit,9055) files(j)(1:LENS(files(j)))
  enddo
  DO j = 1, nofiles
     WRITE (Runit,9055) ofiles(j)
  enddo
  CLOSE (Runit)
270 DO i = 0, Iflag(1)+1
     IF (Well(i).EQ.0) CALL WELLS
  enddo
  RETURN
9000 FORMAT (A80)
9005 FORMAT (/,' -----------------',/,' CHOOSE MODEL FILE',/, &
       ' -----------------',/)
9010 FORMAT (I4,':  ',A40,A7)
9015 FORMAT (/,1X, &
       'Enter number of file, 0 for none, or <ENTER> to see more files:')
9020 FORMAT (/,' Enter number of file or 0 for none:')
9025 FORMAT (/,' Enter number of file, ''M'' to see more choices, 0 to' &
       ,' reset the model',/, &
       '  or <ENTER> to keep the current model:')
9030 FORMAT (/,' Enter number of file, 0 to reset model, or <ENTER>', &
       ' to keep current model:')
9035 FORMAT (1X,40('-'),/,1X,A40,A20,/,1X,40('-'))
9040 FORMAT (6I3)
9045 FORMAT (' Initial well: ',A32)
9050 FORMAT (' Final well  : ',A32,/)
9055 FORMAT (A,A)
9060 FORMAT (A2)
9065 FORMAT (' CONSTRAINTS:')
9070 FORMAT (:,5(1X,A12,:,2X))
9075 FORMAT (/,' PHASES:')
9080 FORMAT (:,5(1X,A8,:,6X))
9085 FORMAT (/,' Use this file?  (N)o, (D)elete, or <ENTER> to accept: ', $ &
       )
9090 FORMAT (/,' Undelete this file?  un(D)elete, or <ENTER> for no: ', $)
9095 FORMAT (5(I2),2(F8.4),a6,F6.3,/,7(F8.3),/,2(F8.0),2(F8.0),2(F8.0), &
       /,5(F8.3),/,5(6(F8.3),/))
9100 FORMAT (3(i2))
9105 FORMAT (6(i1))
9110 FORMAT (A8,2(A1),7(A2,F8.4))
9115 FORMAT (//' Error in file: ',A)
END SUBROUTINE MODELS214
!
!
!
SUBROUTINE PHALIST(II)
  USE max_size
  implicit none
  !
  ! The available phases (from netpath.dat) are displayed.  Also, if
  ! there are more than 60, the display is paused after the first 60, and
  ! the next page can be seen or the number of the phase to be used can be
  ! entered.
  !
  CHARACTER*360 Fline(100)
  COMMON /CHAR7 / Fline
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER II, i, ij, jj, kk
  CHARACTER*3 bans
  !
  II = 0
  WRITE (*,9000)
  IF (Flin.EQ.0) THEN
     WRITE (*,*) 'No phases - check netpath.dat'
     RETURN
  END IF
  i = 1
  ij = 1
10 jj = i+1
  kk = jj+1
  IF (i.GE.60*ij) THEN
     WRITE (*,9005)
     ij = ij+1
     READ (*,'(A)') bans
     IF (bans.NE.' ') THEN
        READ (bans,'(I3)',ERR=20) II
        RETURN
     END IF
  END IF
20 IF (kk.NE.Flin) THEN
     IF (jj.EQ.Flin) THEN
        WRITE (*,9010) i, Fline(i), jj, Fline(jj)
        GO TO 30
     ELSE IF (i.EQ.Flin) THEN
        WRITE (*,9015) i, Fline(i)
        GO TO 30
     END IF
  END IF
  WRITE (*,9020) i, Fline(i), jj, Fline(jj), kk, Fline(kk)
30 IF (i.EQ.Flin .OR. i+1.EQ.Flin .OR. i+2.EQ.Flin) RETURN
  i = i+3
  GO TO 10
9000 FORMAT (//,' List of phases and their corresponding numbers.')
9005 FORMAT (' Hit RETURN to see next page or enter number of phase.')
9010 FORMAT (I4,': ',A10,10X,I3,': ',A10)
9015 FORMAT (I4,': ',A10)
9020 FORMAT (I4,': ',A10,10X,I3,': ',A10,10X,I3,': ',A10)
END SUBROUTINE PHALIST
!
!
!
!
!
!
SUBROUTINE RAYLEIGH(IFILE)
  implicit none
  !
  ! Given initial isotopic values and phase values in and out, the final
  ! isotopic value is calculated, using the basic m inputs, n outputs
  ! Rayleigh equation.
  !
  DOUBLE PRECISION Predat, Disdat, Cfinal, Cinit, Dinit, Result
  COMMON /DP2   / Predat(39,2), Disdat(39,2), Cfinal, Cinit, Dinit, &
       Result
  INTEGER Idis, Ipre, Rwunit
  COMMON /INT2  / Idis, Ipre, Rwunit
  INTEGER IFILE, m, n
  DOUBLE PRECISION smgam, epsbar, gamma, phacomp, epsgam, beta,  &
       expon
  INTRINSIC DABS, EXP
  !
  Result = Dinit
  IF (Ipre+Idis.EQ.0) THEN
     IF (IFILE.EQ.1) WRITE (Rwunit,*) 'No phases in or out'
     RETURN
  END IF
  smgam = 0.0D0
  epsbar = 0.0D0
  gamma = 0.0D0
  phacomp = 0.0D0
  IF (Ipre.EQ.0) Predat(1,1) = 1.0D0
  DO m = 1, Ipre
     smgam = smgam+Predat(m,1)/Predat(1,1)
     epsbar = epsbar+Predat(m,1)*Predat(m,2)/Predat(1,1)
  enddo
  ! GAMMA AVERAGE ISO COMP. COMPUTATIONS
  do n = 1, Idis
     phacomp = phacomp+Disdat(n,1)/Predat(1,1)*Disdat(n,2)
     gamma = gamma+Disdat(n,1)/Predat(1,1)
  enddo
  ! CALCULATE EPSBAR/GAMMA
  IF (DABS(gamma).LT.1.0D-20) THEN
     phacomp = -1000.0D0
     epsgam = 0.0D0
  ELSE
     epsgam = epsbar/gamma
     phacomp = phacomp/gamma
  END IF
  beta = 1.0D0+(.001D0*epsgam)
  IF (DABS(smgam-gamma).GT.1.D-20) THEN
     IF (IFILE.EQ.1) WRITE (Rwunit,9000) 'CFINAL', Cfinal, 'CINIT',  &
          Cinit
     expon = (Cfinal/Cinit)**((gamma+.001D0*epsbar)/(smgam-gamma))
     IF (IFILE.EQ.1) WRITE (Rwunit,9000) 'EXPONENT',  &
          ((gamma+.001D0*epsbar) &
          /(smgam-gamma)), 'EPSBAR',  &
          epsbar
  ELSE
     expon = EXP(-beta*gamma*Predat(1,1)/Cinit)
  END IF
  Result = ((beta*Result-phacomp+epsgam)*expon+phacomp-epsgam)/beta
  IF (IFILE.NE.0) THEN
     WRITE (Rwunit,9000) 'BETA', beta, 'PHACOMP', phacomp, 'EPSGAM',  &
          epsgam
     WRITE (Rwunit,9000) 'EXPON', expon, 'SMGAM', smgam, 'GAMMA',  &
          gamma
  END IF
  RETURN
9000 FORMAT (3(A,1X,F11.5,1X))
END SUBROUTINE RAYLEIGH
!
!
!
SUBROUTINE RDPATH(FILEONE)
  USE max_size
  implicit none
  CHARACTER*80 line
  CHARACTER*80 FILEONE
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd


  OPEN (UNIT=Runit,FILE=FILEONE,STATUS='OLD',ERR=60)
  READ (Runit,'(A4)',ERR=60,END=60) line
  CLOSE (Runit)
  if (line .eq. "2.14") then
     call rdpath214(fileone)
  else
     rewind(Runit)
     call rdpath2(fileone)
  endif

60 continue
  RETURN

END SUBROUTINE RDPATH
!
!
!
SUBROUTINE RDPATH2(FILEONE)
  USE max_size
  implicit none
  !
  ! The data from the .PATH file are read in, including the data that is
  ! passed through untouched by WATEQFP.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  DOUBLE PRECISION cc, cfe2, cfe3, cmn2, cmn3, cmn6, cmn7, cch4,  &
       cdoc, rsdoc
  INTEGER lline(8), i, j, istar
  CHARACTER*80 FILEONE
  CHARACTER*32 temp
  CHARACTER*1 dummy(14), star(MAXWELLS)
  ! THESE DATA NOT AFFECTED BY WATEQFP
  DATA lline/21, 22, 24, 26, 27, 28, 31, 32/
  !
  OPEN (UNIT=Runit,FILE=FILEONE,STATUS='OLD',ERR=40)
  REWIND (UNIT=Runit)
  READ (Runit,9000) temp
  Nwlls = 0
10 Nwlls = Nwlls+1
  i = Nwlls
  DO j = 1, MAXWELLS
     star(j) = ' '
  enddo
  READ (Runit,'(A32)',ERR=60,END=50) temp
  Wllnms(i) = ' '
  Wllnms(i)(5:36) = temp
  READ (Runit,9010,ERR=60) (Dbdata(i,j),star(j),j=1,8)
  READ (Runit,9010,ERR=60) (Dbdata(i,j),star(j),j=9,16)
  READ (Runit,9010,ERR=60) (Dbdata(i,j),star(j),j=17,19),  &
       Dbdata(i,50), dummy(1), Dbdata(i,29),  &
       star(29), Dbdata(i,30), star(30),  &
       Dbdata(i,25), star(25), Dbdata(i,33),  &
       star(33)
  READ (Runit,9010,ERR=60) Dbdata(i,20), dummy(4),  &
       (Dbdata(i,j),dummy(5),j=36,39), cc,  &
       dummy(8), cfe2, dummy(7), cfe3, dummy(6)
  READ (Runit,9010,ERR=60) cmn2, dummy(9), cmn3, dummy(10), cmn6,  &
       dummy(11), cmn7, dummy(12), cch4,  &
       dummy(13), cdoc, dummy(14), rsdoc,  &
       star(48)
  Dbdata(i,33) = Dbdata(i,33)*Dbdata(i,18)
  Dbdata(i,41) = cc
  Dbdata(i,42) = cch4
  Dbdata(i,43) = cdoc
  Dbdata(i,44) = -40.0D0
  Dbdata(i,45) = -25.0D0
  Dbdata(i,46) = 0.0D0
  Dbdata(i,47) = 0.0D0
  Dbdata(i,48) = rsdoc
  READ (Runit,9015,ERR=60) (Dbdata(i,lline(j)),star(lline(j)),j=1,8) &
       , Tot(i)
  IF (Tot(i).EQ.0) Tot(i) = i
  ! Equation for 34SH2S based on 34SSO4:  CAN CHANGE!
  !
  IF (star(32).EQ.'*' .AND. star(31).EQ.' ') THEN
     Dbdata(i,32) = Dbdata(i,31)-54.0D0+0.4D0*Dbdata(i,50)
     star(32) = ' '
  END IF
  Dbdata(i,20) = Dbdata(i,20)*4.0D0
  Dbdata(i,24) = Dbdata(i,24)*Dbdata(i,15)
  Dbdata(i,21) = Dbdata(i,21)*Dbdata(i,41)
  Dbdata(i,22) = Dbdata(i,22)*Dbdata(i,41)
  Dbdata(i,31) = Dbdata(i,31)*Dbdata(i,30)
  Dbdata(i,32) = Dbdata(i,32)*Dbdata(i,29)
  Dbdata(i,23) = Dbdata(i,31)+Dbdata(i,32)
  ! DBDATA( ,34) DOES NOT INCLUDE RS OF DOC, BECAUSE THIS VARIABLE. IT IS
  !  INCLUDED WHEN THE MODEL IS RUN.
  Dbdata(i,34) = cc*4-cch4*4
  Dbdata(i,35) = cfe2*2+cfe3*3
  Dbdata(i,40) = cmn2*2+cmn3*3+cmn6*6+cmn7*7
  IF (star(31).EQ.'*' .AND. star(32).EQ.'*') star(23) = '*'
  IF (star(29).EQ.'*' .AND. star(30).EQ.'*') star(2) = '*'
  DO istar = 1, 50
     Nodata(i,istar) = 0
     IF (star(istar).EQ.'*') Nodata(i,istar) = 1
  enddo
  GO TO 10
40 STOP 1
50 CLOSE (Runit)
  Nwlls = Nwlls-1
  RETURN
60 WRITE (*,9005) Nwlls, j, Wllnms(Nwlls)(1:79)
  STOP
9000 FORMAT (///////,A32)
9005 FORMAT (/,' **** Error ****',/,' Unknown error at well#',2I3,/,1X, &
       A79,/,' ***************',/)
9010 FORMAT (8(F8.0,A1))
9015 FORMAT (8(F8.0,A1),2X,I6)
END SUBROUTINE RDPATH2
!
!
!
SUBROUTINE RDPATH214(FILEONE)
  USE max_size
  implicit none
  !
  ! The data from the .PATH file are read in, including the data that is
  ! passed through untouched by WATEQFP.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  DOUBLE PRECISION cc, cfe2, cfe3, cmn2, cmn3, cmn6, cmn7, cch4,  &
       cdoc, rsdoc 
  INTEGER lline(8), i, j, istar
  CHARACTER*80 FILEONE
  CHARACTER*32 temp
  CHARACTER*1 dummy(14), star(MAXWELLS)
  ! THESE DATA NOT AFFECTED BY WATEQFP
  DATA lline/21, 22, 24, 26, 27, 28, 31, 32/
  !
  OPEN (UNIT=Runit,FILE=FILEONE,STATUS='OLD',ERR=40)
  REWIND (UNIT=Runit)
  READ (Runit,'(A)') temp
  Nwlls = 0
10 continue
  Nwlls = Nwlls+1
  i = Nwlls
  DO j = 1, MAXWELLS
     star(j) = ' '
  enddo
  READ (Runit,'(A32)',ERR=60,END=50) temp
  Wllnms(i) = ' '
  Wllnms(i)(5:36) = temp
  !      READ (Runit,9010,ERR=60) (Dbdata(i,j),star(j),j=1,8)
  !      READ (Runit,9010,ERR=60) (Dbdata(i,j),star(j),j=9,16)
  !      READ (Runit,9010,ERR=60) (Dbdata(i,j),star(j),j=17,19),  &
  !                              Dbdata(i,50), dummy(1), Dbdata(i,29),  &
  !                              star(29), Dbdata(i,30), star(30),  &
  !                              Dbdata(i,25), star(25), Dbdata(i,33),  &
  !                              star(33)
  do j = 1,19
     read(runit,"(f14.0,A1)") dbdata(i,j), star(j)
  enddo
  read(runit,"(f14.0,A1)") dbdata(i,50), dummy(1)
  do j = 29,30
     read(runit,"(f14.0,A1)") dbdata(i,j), star(j)
  enddo
  j = 25
  read(runit,"(f14.0,A1)") dbdata(i,j), star(j)
  j = 33
  read(runit,"(f14.0,A1)") dbdata(i,j), star(j)
  !      READ (Runit,9010,ERR=60) Dbdata(i,20), dummy(4),  &
  !                              (Dbdata(i,j),dummy(5),j=36,39), cc,  &
  !                              dummy(8), cfe2, dummy(7), cfe3, dummy(6)
  j = 20
  read(runit,"(f14.0,A1)") dbdata(i,j), dummy(4)
  do j = 36,39
     read(runit,"(f14.0,A1)") dbdata(i,j), dummy(5)
  enddo
  read(runit,"(f14.0,A1)") cc, dummy(8)
  read(runit,"(f14.0,A1)") cfe2, dummy(7)
  read(runit,"(f14.0,A1)") cfe3, dummy(6)
  !      READ (Runit,9010,ERR=60) cmn2, dummy(9), cmn3, dummy(10), cmn6,  &
  !                              dummy(11), cmn7, dummy(12), cch4,  &
  !                              dummy(13), cdoc, dummy(14), rsdoc,  &
  !                              star(48)
  read(runit,"(f14.0,A1)") cmn2, dummy(9)
  read(runit,"(f14.0,A1)") cmn3, dummy(10)
  read(runit,"(f14.0,A1)") cmn6, dummy(11)
  read(runit,"(f14.0,A1)") cmn7, dummy(12)
  read(runit,"(f14.0,A1)") cch4, dummy(13)
  read(runit,"(f14.0,A1)") cdoc, dummy(14)
  read(runit,"(f14.0,A1)") rsdoc, star(48)
  read(runit,"(A1)") temp

  Dbdata(i,33) = Dbdata(i,33)*Dbdata(i,18)
  Dbdata(i,41) = cc
  Dbdata(i,42) = cch4
  Dbdata(i,43) = cdoc
  Dbdata(i,44) = -40.0D0
  Dbdata(i,45) = -25.0D0
  Dbdata(i,46) = 0.0D0
  Dbdata(i,47) = 0.0D0
  Dbdata(i,48) = rsdoc
  !      READ (Runit,9015,ERR=60) (Dbdata(i,lline(j)),star(lline(j)),j=1,8) &
  !                              , Tot(i)
  do j = 1, 8
     read(runit,"(f14.0,A1)") dbdata(i,lline(j)), star(lline(j))
  enddo
  read(runit,'(I15)') Tot(i)

  IF (Tot(i).EQ.0) Tot(i) = i
  ! Equation for 34SH2S based on 34SSO4:  CAN CHANGE!
  !
  IF (star(32).EQ.'*' .AND. star(31).EQ.' ') THEN
     Dbdata(i,32) = Dbdata(i,31)-54.0D0+0.4D0*Dbdata(i,50)
     star(32) = ' '
  END IF
  Dbdata(i,20) = Dbdata(i,20)*4.0D0
  Dbdata(i,24) = Dbdata(i,24)*Dbdata(i,15)
  Dbdata(i,21) = Dbdata(i,21)*Dbdata(i,41)
  Dbdata(i,22) = Dbdata(i,22)*Dbdata(i,41)
  Dbdata(i,31) = Dbdata(i,31)*Dbdata(i,30)
  Dbdata(i,32) = Dbdata(i,32)*Dbdata(i,29)
  Dbdata(i,23) = Dbdata(i,31)+Dbdata(i,32)
  ! DBDATA( ,34) DOES NOT INCLUDE RS OF DOC, BECAUSE THIS VARIABLE. IT IS
  !  INCLUDED WHEN THE MODEL IS RUN.
  Dbdata(i,34) = cc*4-cch4*4
  Dbdata(i,35) = cfe2*2+cfe3*3
  Dbdata(i,40) = cmn2*2+cmn3*3+cmn6*6+cmn7*7
  IF (star(31).EQ.'*' .AND. star(32).EQ.'*') star(23) = '*'
  IF (star(29).EQ.'*' .AND. star(30).EQ.'*') star(2) = '*'
  DO istar = 1, 50
     Nodata(i,istar) = 0
     IF (star(istar).EQ.'*') Nodata(i,istar) = 1
  enddo
  GO TO 10
40 STOP 1
50 CLOSE (Runit)
  Nwlls = Nwlls-1
  RETURN
60 WRITE (*,9005) Nwlls, j, Wllnms(Nwlls)(1:79)
  STOP
9000 FORMAT (///////,A32)
9005 FORMAT (/,' **** Error ****',/,' Unknown error at well#',2I3,/,1X, &
       A79,/,' ***************',/)
9010 FORMAT (8(F8.0,A1))
9015 FORMAT (8(F8.0,A1),2X,I6)
END SUBROUTINE RDPATH214
!
!
!
SUBROUTINE SAVE
  implicit none
  !
  !  The user is asked whether the model or results should be saved, is
  !  prompted for filenames, and the data is saved, if desired.
  !
  CHARACTER ans*1, UPCS*1
  EXTERNAL POSCUR, UPCS, SAVEMOD214, SAVERUN
  !
  CALL POSCUR(-1)
  WRITE (*,9000) 'model'
  READ (*,'(A)') ans
  IF (UPCS(ans).EQ.'Y') CALL SAVEMOD214
10 CALL POSCUR(-1)
  WRITE (*,9000) 'results'
  READ (*,'(A)') ans
  IF (UPCS(ans).EQ.'Y') CALL SAVERUN(*10)
  RETURN
9000 FORMAT (' Do you want to save the ',A,'?',/,' (<Enter> for no)')
END SUBROUTINE SAVE
!
!
!
SUBROUTINE SAVEMOD
  USE max_size
  use filenames
  implicit none
  !
  !  The model file, containing ALL the data affecting the model is
  !  stored.  Name duplications are recognized and the user is allowed to
  !  change the name under which to store the data.
  !

  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  LOGICAL isthere
  INTEGER je(36), LENS, i, j, k, ipatfil, ifli, iwell
  DOUBLE PRECISION pc(36)
  CHARACTER mfile(100)*80, line*80, UPCS80*80
  DOUBLE PRECISION i10, i11
  EQUIVALENCE (C14dat(10),i10)
  EQUIVALENCE (C14dat(11),i11)
  EXTERNAL POSCUR, INCLISO, LENS, UPCS80
  !
10 CALL POSCUR(-1)
  IF (Efile.EQ.' ') WRITE (*,9005)
  IF (Efile.NE.' ') WRITE (*,9010) Efile(1:LENS(Efile))
  READ (*,9000) line
  IF (line.EQ.' ' .AND. Efile.EQ.' ') RETURN
  IF (line.NE.' ') Efile = line
  INQUIRE (FILE=Efile,EXIST=isthere)
  IF (isthere) GO TO 110
  OPEN (Wunit,FILE=Efile,STATUS='NEW',ERR=110)
20 WRITE (Wunit,'(A80)') Wfile
  WRITE (Wunit,9015) (Tot(Well(i)),i=1,Iflag(1)+1), Tot(Well(0))
  WRITE (Wunit,9020) (Eleshort(Iele(i)),i=1,Noele)
  DO i = 1, Nopha
     DO j = 1, 36
        je(j) = Jele(i,j)
        pc(j) = Pcoeff(i,j)
     enddo
     j = 0
40   j = j+1
     IF (je(j).NE.0) GO TO 40
     IF (Para(i,1).GT.0.0D0) THEN
        je(j) = 34
        pc(j) = Para(i,1)
     ELSE
        j = j-1
     END IF
     CALL INCLISO(je,pc,i,j,0)
     WRITE (Wunit,9025) Phase(i), F(i), It(i),  &
          (Eleshort(je(k)),pc(k),k=1,j)
  enddo
  WRITE (Wunit,9000)
  WRITE (Wunit,9030) (Iflag(i),i=2,6), (P(i),i=1,2),  &
       (Isdocrs(i),i=0,5), Disalong, (C14dat(i),i=1,5) &
       , (Usera(i),i=1,2), (C14dat(i),i=8,9), i10,  &
       i11, (C14dat(i),i=12,13),  &
       (Dbdata(Well(0),i),i=44,47), Dbdata(Well(0),49) &
       , ((Dbdata(Well(iwell),i),i=44,47),Dbdata(Well( &
       iwell),49),Usera(iwell),iwell=1,Iflag(1)+1)
  CLOSE (Wunit)
  OPEN (UNIT=Runit,FILE='model.fil')
  REWIND (Runit)
  ipatfil = 0
60 ipatfil = ipatfil+1
70 READ (Runit,9000,END=80) mfile(ipatfil)
  IF (mfile(ipatfil).EQ.' ') GO TO 70
  IF (Icase.EQ.0 .AND. (UPCS80(mfile(ipatfil)).EQ.UPCS80(Efile))) &
       GO TO 100
  IF (mfile(ipatfil).EQ.Efile) GO TO 100
  GO TO 60
80 CLOSE (Runit,STATUS='DELETE')
  OPEN (Runit,FILE='model.fil')
  DO ifli = 1, ipatfil-1
     WRITE (Runit,'(A)') mfile(ifli)
  enddo
  WRITE (Runit,'(A)') Efile
100 CLOSE (Runit)
  RETURN
110 CALL POSCUR(-1)
  OPEN (Wunit,FILE=Efile,ERR=10)
  WRITE (*,9035)
  READ (*,9000) line
  IF (line.NE.' ') THEN
     CLOSE (Wunit)
     OPEN (Wunit,FILE=line,STATUS='NEW',ERR=110)
     Efile = line
  END IF
  GO TO 20
  !
9000 FORMAT (A)
9005 FORMAT (' Enter file name. <Enter> to abort')
9010 FORMAT (' Enter file name.  <Enter> for ',A)
9015 FORMAT (20I3)
9020 FORMAT (26(A3))
9025 FORMAT (A8,2(A1),7(A2,F8.3),4(:,/,10X,7(A2,F8.3)))
9030 FORMAT (5(I2),2(F8.4),6(I1),F6.3,/,7(F8.3),/,2(F8.3),2(F8.0), &
       2(F8.3),/,5(F8.3),/,7(6(F8.3),/))
9035 FORMAT ( &
       ' File exists.  Type new filename or <Enter> to overwrite.')
END SUBROUTINE SAVEMOD
!
!
!
SUBROUTINE SAVEMOD214
  USE max_size
  use filenames
  implicit none
  !
  !  The model file, containing ALL the data affecting the model is
  !  stored.  Name duplications are recognized and the user is allowed to
  !  change the name under which to store the data.
  !

  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  LOGICAL isthere
  INTEGER je(36), LENS, i, j, k, ipatfil, ifli, iwell
  DOUBLE PRECISION pc(36)
  CHARACTER mfile(100)*80, line*80, UPCS80*80
  DOUBLE PRECISION i10, i11
  EQUIVALENCE (C14dat(10),i10)
  EQUIVALENCE (C14dat(11),i11)
  EXTERNAL POSCUR, INCLISO, LENS, UPCS80
  !
10 CALL POSCUR(-1)
  IF (Efile.EQ.' ') WRITE (*,9005)
  IF (Efile.NE.' ') WRITE (*,9010) Efile(1:LENS(Efile))
  READ (*,9000) line
  IF (line.EQ.' ' .AND. Efile.EQ.' ') RETURN
  IF (line.NE.' ') Efile = line
  INQUIRE (FILE=Efile,EXIST=isthere)
  IF (isthere) GO TO 110
  OPEN (Wunit,FILE=Efile,STATUS='NEW',ERR=110)
20 WRITE (Wunit,'(A80)') Wfile
!
! well numbers
  WRITE (Wunit,'(20I3)') (Tot(Well(i)),i=1,Iflag(1)+1), Tot(Well(0))
!
! element constraints
  WRITE (Wunit,'(26(A3))') (Eleshort(Iele(i)),i=1,Noele)
!
! phase data
  DO i = 1, Nopha
	 ! copy to working element and coefficient
     DO j = 1, 36
        je(j) = Jele(i,j)
        pc(j) = Pcoeff(i,j)
     enddo

     j = 0
40   j = j+1
     IF (je(j).NE.0) GO TO 40
     IF (Para(i,1).GT.0.0D0) THEN
        je(j) = 34
        pc(j) = Para(i,1)
     ELSE
        j = j-1
     END IF
     CALL INCLISO(je,pc,i,j,0)
	 ! write name, force, dis/pre
     WRITE (Wunit,'(A8,2(A1), $)') Phase(i), F(i), It(i)
	 ! write stoichiometry
	 WRITE (Wunit,'(7(A3,F12.7),4(:,/,10X,7(A3,F12.7)))') (Eleshort(je(k)),pc(k),k=1,j)
  enddo
  WRITE (Wunit,9000)
  WRITE (Wunit,9030) (Iflag(i),i=2,6), (P(i),i=1,2),  &
       (Isdocrs(i),i=0,5), Disalong, (C14dat(i),i=1,5) &
       , (Usera(i),i=1,2), (C14dat(i),i=8,9), i10,  &
       i11, (C14dat(i),i=12,13),  &
       (Dbdata(Well(0),i),i=44,47), Dbdata(Well(0),49) &
       , ((Dbdata(Well(iwell),i),i=44,47),Dbdata(Well( &
       iwell),49),Usera(iwell),iwell=1,Iflag(1)+1)
  CLOSE (Wunit)
  OPEN (UNIT=Runit,FILE='model.fil')
  REWIND (Runit)
  ipatfil = 0
60 ipatfil = ipatfil+1
70 READ (Runit,9000,END=80) mfile(ipatfil)
  IF (mfile(ipatfil).EQ.' ') GO TO 70
  IF (Icase.EQ.0 .AND. (UPCS80(mfile(ipatfil)).EQ.UPCS80(Efile))) &
       GO TO 100
  IF (mfile(ipatfil).EQ.Efile) GO TO 100
  GO TO 60
80 CLOSE (Runit,STATUS='DELETE')
  OPEN (Runit,FILE='model.fil')
  DO ifli = 1, ipatfil-1
     WRITE (Runit,'(A)') mfile(ifli)
  enddo
  WRITE (Runit,'(A)') Efile
100 CLOSE (Runit)
  RETURN
110 CALL POSCUR(-1)
  OPEN (Wunit,FILE=Efile,ERR=10)
  WRITE (*,9035)
  READ (*,9000) line
  IF (line.NE.' ') THEN
     CLOSE (Wunit)
     OPEN (Wunit,FILE=line,STATUS='NEW',ERR=110)
     Efile = line
  END IF
  GO TO 20
  !
9000 FORMAT (A)
9005 FORMAT (' Enter file name. <Enter> to abort')
9010 FORMAT (' Enter file name.  <Enter> for ',A)
9015 FORMAT (20I3)
9020 FORMAT (26(A3))
9025 FORMAT (A8,2(A1),7(A2,F8.3),4(:,/,10X,7(A2,F8.3)))
9030 FORMAT (5(I2),2(F8.4),6(I1),F6.3,/,7(F8.3),/,2(F8.3),2(F8.0), &
       2(F8.3),/,5(F8.3),/,7(6(F8.3),/))
9035 FORMAT ( &
       ' File exists.  Type new filename or <Enter> to overwrite.')
END SUBROUTINE SAVEMOD214
!
!
!
SUBROUTINE SAVEOTHER(IJ)
  USE max_size
  use filenames
  implicit none
  !
  ! After a phase is made from scratch or a current phase is edited, it
  ! may be included in netpath.dat for use during future runs.
  !

  CHARACTER*360 Fline(100)
  COMMON /CHAR7 / Fline
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER je(36), i, j, k, IJ
  DOUBLE PRECISION pc(36)
  CHARACTER middle*8, UPCS*1, ans*1
  EXTERNAL INCLISO, UPCS
  INTRINSIC LGT
  !
  IF (Flin.EQ.100) GO TO 60
  WRITE (*,9000) Phase(IJ)
  READ (*,9005) ans
  ans = UPCS(ans)
  IF (ans.NE.'Y') GO TO 60
  middle = Phase(IJ)
  i = 0
10 i = i+1
  IF (LGT(middle,Fline(i)(1:8)) .AND. i.LT.Flin) GO TO 10
  IF (middle.NE.Fline(i)(1:8)) THEN
     DO j = Flin, i, -1
        Fline(j+1) = Fline(j)
     enddo
     Flin = Flin+1
     k = 0
     GO TO 40
  END IF
30 WRITE (*,9010)
  READ (*,9005) ans
  IF (ans.EQ.' ') GO TO 60
  READ (ans,'(I1)',ERR=30) j
  IF (j.LT.1 .OR. j.GT.2) GO TO 30
  IF (j.NE.2) THEN
     WRITE (*,9015)
     READ (*,9005,ERR=30) middle
     IF (middle.EQ.' ') GO TO 30
     i = 0
     GO TO 10
  END IF
  k = 0
40 k = k+1
  IF (Jele(IJ,k).NE.0) GO TO 40
  k = k-1
  DO j = 1, 36
     je(j) = Jele(IJ,j)
     pc(j) = Pcoeff(IJ,j)
  enddo
  CALL INCLISO(je,pc,IJ,k,0)
  WRITE (Fline(i),9020) middle, It(IJ),  &
       (Eleshort(je(j)),pc(j),j=1,k)
60 RETURN
9000 FORMAT (/,' Do you want to store ''',A, &
       ''' in netpath.dat? <Enter> for no')
9005 FORMAT (A)
9010 FORMAT (/,' This phase already exists in netpath.dat.',/, &
       ' (1) Save phase with different name or (2) Replace phase in file' &
       ,/,' <Enter> to quit.')
9015 FORMAT (/' Enter new phase name.')
9020 FORMAT (A8,1X,A1,35(A2,F8.4))
END SUBROUTINE SAVEOTHER
!
!
!
SUBROUTINE SAVERUN(*)
  USE max_size
  implicit none
  !
  ! The results of the run, including isotope data and all C-14 models, if
  ! applicable, are stored in a file.
  !
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  LOGICAL isthere
  CHARACTER*80 line2, line
  EXTERNAL POSCUR, RUN
  !
  CALL POSCUR(-1)
  WRITE (*,9000)
  READ (*,'(A)') line
  IF (line.NE.' ') THEN
10   INQUIRE (FILE=line,EXIST=isthere)
     IF (.NOT.isthere) THEN
        OPEN (Wunit,FILE=line,STATUS='NEW',ERR=20)
        GO TO 30
     END IF
20   WRITE (*,9005)
     READ (*,'(A)') line2
     IF (line2.NE.' ') THEN
        line = line2
        GO TO 10
     END IF
     OPEN (Wunit,FILE=line,ERR=40)
30   CALL RUN(0)
  END IF
  RETURN
40 RETURN 1
9000 FORMAT (' Enter file name. <Enter> to abort')
9005 FORMAT ( &
       ' File exists - hit <Enter> to overwrite or enter new name')
END SUBROUTINE SAVERUN
!
!
!
SUBROUTINE SCREEN
  USE max_size
  use filenames
  use version
  implicit none
  !
  ! The important aspects of the current model are displayed on the
  ! screen.  The wells, constraints, phases, and key parameters are
  ! displayed.  Also, warnings about insufficient data or incorrect sets
  ! of constraints and phases are displayed, when necessary.
  !

  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  CHARACTER first3rd*26, middle*28, last3rd*23
  LOGICAL inoc
  DOUBLE PRECISION C14
  INTEGER LENS, iforce, iex, ico2, i, j, n, m, l, iskip, ijk
  EXTERNAL CLS, WARN, C14, LENS
  INTRINSIC INT
   
  !
  iforce = 0
  iex = 0
  ico2 = 0
  DO i = 1, Nopha
     IF (Phase(i).EQ.'EXCHANGE') iex = 1
     IF (Phase(i).EQ.'CO2-CH4 ') ico2 = 1
     IF (F(i).EQ.'F') iforce = iforce+1
  enddo
  IF (iex.EQ.0) Iflag(2) = 3
  CALL CLS
  DO i = 1, Iflag(1)
     WRITE (*,9000) Wllnms(Well(i))(5:36)
  enddo
  WRITE (*,9000) Wllnms(Well(Iflag(1)+1))(5:36), TRIM(ProgramName)//" "//VersionNumber
  IF (iforce.EQ.0) THEN
     WRITE (*,9005) Wllnms(Well(0))(5:36), datestr, Noele, Nopha
  ELSE
     WRITE (*,9010) Wllnms(Well(0))(5:36), datestr, Noele, Nopha
  END IF
  i = 0
30 i = i+1
  ! WRITE CONSTRAINTS
  first3rd = ' '
  IF (2*i.LE.Noele) THEN
     WRITE (first3rd,9015) (Elelong(Iele(j)),j=2*i-1,2*i)
  ELSE IF (2*i-1.EQ.Noele) THEN
     WRITE (first3rd,9020) Elelong(Iele(2*i-1))
  END IF
  ! WRITE PHASES
  middle = ' '
  IF (3*i-2.LE.iforce) THEN
     n = 0
     DO m = 1, 3*i-3
40      n = n+1
        IF (F(n).NE.'F') GO TO 40
     enddo
     l = 0
60   l = l+1
70   n = n+1
     IF (F(n).NE.'F') GO TO 70
     middle(-8+9*l:9*l) = It(n)//Phase(n)
     IF (3*i-2+l.LE.iforce .AND. l.LT.3) GO TO 60
  END IF
  IF (i.GT.1 .AND. iforce.GT.3*i-6 .AND. iforce.LT.3*i-2 .AND.  &
       iforce.LT.Nopha) middle = '----------Unforced----------'
  IF (iforce.NE.0) THEN
     l = i-INT((iforce+2)/3)-1
     IF (Nopha-iforce.GE.3*l-2 .AND. l.GT.0) THEN
        n = 0
        DO m = 1, 3*l-3
80         n = n+1
           IF (F(n).EQ.'F') GO TO 80
        enddo
        j = 0
100     j = j+1
110     n = n+1
        IF (F(n).EQ.'F') GO TO 110
        middle(-8+9*j:9*j) = It(n)//Phase(n)
        IF (3*l-2+j.LE.Nopha-iforce .AND. j.LT.3) GO TO 100
     END IF
  ELSE IF (3*i-2.LE.Nopha) THEN
     n = 3*i-3
     l = 0
120  l = l+1
     n = n+1
     middle(-8+9*l:9*l) = It(n)//Phase(n)
     IF (3*i-2+l.LE.Nopha .AND. l.LT.3) GO TO 120
  END IF
  ! WRITE PARAMETERS
  last3rd = ' '
  IF (i.EQ.1) THEN
     IF (Iflag(1).EQ.0) THEN
        WRITE (last3rd,9025) 'Mixing: ', 'No'
     ELSE
        WRITE (last3rd,9025) 'Mixing: ', 'Yes'
     END IF
  END IF
  IF (i.EQ.2) WRITE (last3rd,9025) 'Evaporation: ', Yes(Iflag(6))
  IF (i.EQ.3) WRITE (last3rd,9025) 'Rayleigh Calcs: ', Yes(Iflag(3))
  IF (iex.EQ.1 .AND. i.EQ.4) WRITE (last3rd,9025) 'Exchange: ',  &
       Ion(Iflag(2))
  iskip = 0
  IF (Iflag(2).EQ.4) iskip = 1
  IF (iex.EQ.1 .AND. Iflag(2).EQ.4 .AND. i.EQ.5) &
       WRITE (last3rd,9030) 'X Calcium:     ', P(2)
  IF (ico2.EQ.1 .AND. i.EQ.4+iex+iskip) WRITE (last3rd,9040) &
       'X CO2 in CO2-CH4:', P(1)
  inoc = .FALSE.
  DO ijk = 1, Iflag(1)+1
     IF (Dbdata(Well(ijk),1).LE.0.0D0) inoc = .TRUE.
  enddo
  IF (.NOT.inoc) THEN
     IF (Iflag(3).EQ.1) THEN
        IF (i.GE.(4+iex+iskip+ico2) .AND. i.LE.(4+iex+iskip+ico2) &
             +Iflag(1)) THEN
           IF (Iflag(1).EQ.0) THEN
              WRITE (last3rd,9030) 'Init C-14 ', C14(Iflag(4),1),  &
                   ' (TDC)'
           ELSE
              WRITE (last3rd,9035) 'Init ', i+1-(4+iex+iskip+ico2),  &
                   ' C-14  ',  &
                   C14(Iflag(4),i+1-(4+iex+iskip+ico2))
           END IF
        END IF
        IF (i.EQ.(4+iex+iskip+ico2)+1+Iflag(1)) WRITE (last3rd,9025) &
             '(', Model(Iflag(4))(1:LENS(Model(Iflag(4)))), ')'
     END IF
  END IF
  IF (first3rd.NE.' ' .OR. middle.NE.' ' .OR. last3rd.NE.' ') THEN
     WRITE (*,'(1X,A79)') first3rd//'|'//middle//'|'//last3rd
     GO TO 30
  END IF
  Ilength = i-1
  WRITE (*,9050)
  CALL WARN
  RETURN
9000 FORMAT (' Initial Well: ',A32,A32)
9005 FORMAT (' Final Well  : ',A32,A32,/,1X,79('='),/,6X, &
       'Constraints:',I3,6X,'|',8X,'Phases:',I3,10X,'|',6X, &
       'Parameters'/,1X,26('-'),'|',28('-'),'|',23('-'))
9010 FORMAT (' Final Well  : ',A32,A32,/,1X,79('='),/,6X, &
       'Constraints:',I3,6X,'|',8X,'Phases:',I3,10X,'|',6X, &
       'Parameters'/,1X,26('-'),'|',11('-'),'Forced',11('-'),'|', &
       23('-'))
9015 FORMAT (A12,1X,A12)
9020 FORMAT (A12)
9025 FORMAT (A,A,A,A)
9030 FORMAT (A,F6.2,A)
9035 FORMAT (A,I1,A,F6.2)
9040 FORMAT (A,F5.2)
9050 FORMAT (1X,79('='))
END SUBROUTINE SCREEN
!
!
!
SUBROUTINE SFRACT(FRACTION,ITIME,IWELL,IERROR)
  USE max_size
  implicit none
  DOUBLE PRECISION FRACTION
  INTEGER ITIME, IWELL, IERROR
  !
  !  A specific frationation factor is calculated, based on some fraction
  !  of the initial and final waters.
  !  Meaning of ITIME 1:  Sulfide-solution at some point along the flowpath
  !                   2:  Sulfate-solution at some point along the flowpath
  !  Meaning of IWELL -1: Use each inital well equally
  !                    0: Use computed mixing ratio
  !                   >0: Use number for just that initial well
  !  Meaning of IERROR 0:  No error
  !                    1:  Can't compute at final or initial well
  !                    2:  Can't compute at final well
  !                    3:  Can't compute at initial well
  !
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  DOUBLE PRECISION dion, aion, ds, as, ratio, frac
  INTEGER i, iwhich, loopnum
  !
  IERROR = 0
  iwhich = IWELL
  IF (Iflag(1).EQ.0) iwhich = 1
  DO i = 1, Iflag(1)+1
     IF (Nodata(Well(i),23).EQ.1 .OR. Nodata(Well(i),2).EQ.1) THEN
        IF (iwhich.LE.0 .OR. iwhich.EQ.i) IERROR = 3
     END IF
  enddo
  IF (Nodata(Well(0),23).EQ.1 .OR. Nodata(Well(0),2).EQ.1) THEN
     IF (IERROR.EQ.3) THEN
        IERROR = 1
        RETURN
     END IF
     IERROR = 2
  END IF
  loopnum = 1
20 IF (loopnum.NE.1) THEN
     dion = Dbdata(Well(0),32)
     aion = Dbdata(Well(0),29)
     IF (ITIME.EQ.2) THEN
        dion = Dbdata(Well(0),31)
        aion = Dbdata(Well(0),30)
     END IF
     ds = Dbdata(Well(0),23)
     as = Dbdata(Well(0),2)
  ELSE IF (iwhich.GT.0) THEN
     dion = Dbdata(Well(iwhich),32)
     aion = Dbdata(Well(iwhich),29)
     IF (ITIME.EQ.2) THEN
        dion = Dbdata(Well(iwhich),31)
        aion = Dbdata(Well(iwhich),30)
     END IF
     ds = Dbdata(Well(iwhich),23)
     as = Dbdata(Well(iwhich),2)
  ELSE
     dion = 0.0D0
     aion = 0.0D0
     ds = 0.0D0
     as = 0.0D0
     DO i = 1, Iflag(1)+1
        IF (iwhich.EQ.0) THEN
           frac = Delta(i)
        ELSE
           frac = 1.0D0/(Iflag(1)+1)
        END IF
        IF (ITIME.EQ.3) THEN
           dion = dion+frac*Dbdata(Well(i),32)
           aion = aion+frac*Dbdata(Well(i),29)
        ELSE
           aion = aion+frac*Dbdata(Well(i),30)
           dion = dion+frac*Dbdata(Well(i),31)
        END IF
        ds = ds+frac*Dbdata(Well(i),23)
        as = as+frac*Dbdata(Well(i),2)
     enddo
  END IF
  IF (aion.LE.1.0D-10) THEN
     IF (loopnum.EQ.1 .AND. IERROR.EQ.2) IERROR = 1
     IF (loopnum.EQ.1 .AND. IERROR.EQ.0) IERROR = 3
     IF (loopnum.EQ.2 .AND. IERROR.EQ.3) IERROR = 1
     IF (loopnum.EQ.2 .AND. IERROR.EQ.0) IERROR = 2
  ELSE
     ratio = dion/aion
  END IF
  IF (IERROR.NE.1) THEN
     IF (loopnum.EQ.1) THEN
        IF (IERROR.NE.3) FRACTION = ratio-ds/as
        loopnum = 2
        GO TO 20
     END IF
     IF (IERROR.EQ.0) THEN
        FRACTION = FRACTION*(1.0D0-Disalong)+Disalong*(ratio-ds/as)
     ELSE IF (IERROR.EQ.3) THEN
        FRACTION = ratio-ds/as
     END IF
  END IF
  RETURN
END SUBROUTINE SFRACT
!
!
!
SUBROUTINE TRANS(IJ)
  USE max_size
  use filenames
  implicit none
  !
  ! The user is asked whether the direction of transfer of a phase should
  ! be limited.  Also, a phase can be forced to be included in all models
  ! considered.
  !

  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  INTEGER itr, i, IJ, j, k
  DOUBLE PRECISION pnum
  CHARACTER*10 line*10, ans*1, UPCS80*80, UPCS*1
  EXTERNAL HAVE, DONTHAVE, UPCS, UPCS80
  !
10 i = 0
  DO j = 1, Nopha
     IF (j.NE.IJ) THEN
        IF (UPCS80(Phase(j)).EQ.UPCS80(Phase(IJ))) i = j
     END IF
  enddo
  IF (i.GT.0) THEN
     WRITE (*,9000) Phase(IJ)
     READ (*,9010) line
     IF (UPCS(line(1:1)).EQ.'R') THEN
        WRITE (*,9005) Phase(IJ)
        READ (*,9010) line
        Phase(IJ) = line(1:8)
        GO TO 10
     ELSE IF (UPCS(line(1:1)).EQ.'D') THEN
        DO j = IJ+1, Nopha
           Phase(j-1) = Phase(j)
           It(j-1) = It(j)
           F(j-1) = F(j)
           DO k = 1, 36
              Jele(j-1,k) = Jele(j,k)
              Pcoeff(j-1,k) = Pcoeff(j,k)
           enddo
           DO k = 1, 16
              IF (k.GT.1) Dowehave(j-1,k) = Dowehave(j,k)
              Para(i-1,k) = Para(i,k)
           enddo
        enddo
        Nopha = Nopha-1
        RETURN
     ELSE
        GO TO 10
     END IF
  END IF
60 IF (It(IJ).EQ.'+') WRITE (*,9015) Phase(IJ), 'dissolution'
  IF (It(IJ).EQ.'-') WRITE (*,9015) Phase(IJ), 'precipitation'
  IF (It(IJ).EQ.'*') WRITE (*,9015) Phase(IJ), 'isotopic exchange'
  IF (It(IJ).EQ.' ') WRITE (*,9015) Phase(IJ), 'both'
  READ (*,9010) line
  IF (line.EQ.' ') THEN
     IF (It(IJ).NE.'*') GO TO 190
     It(IJ) = '*'
     GO TO 70
  END IF
  READ (line,'(I1)',ERR=60) itr
  IF (itr.LT.1 .OR. itr.GT.4) GO TO 60
  IF (itr.EQ.1) It(IJ) = '+'
  IF (itr.EQ.2) It(IJ) = '-'
  IF (itr.EQ.3) It(IJ) = ' '
  IF (itr.NE.4) THEN
     Para(IJ,1) = 0.0D0
     GO TO 190
  END IF
  It(IJ) = '*'
70 WRITE (*,9020)
  IF (Para(IJ,1).GT.0.0D0) WRITE (*,9025) Para(IJ,1)
  READ (*,9010) line
  IF (line.NE.' ' .OR. Para(IJ,1).LE.0.0D0) THEN
     READ (line,9045,ERR=70) pnum
     IF (pnum.LE.0.0D0) GO TO 60
     Para(IJ,1) = pnum
  END IF
  i = 0
80 i = i+1
  IF (Jele(IJ,i).EQ.1) THEN
     IF (Dowehave(IJ,12)) THEN
        WRITE (*,9030) 'Carbon-13', Para(IJ,12)
     ELSE
        WRITE (*,9035) 'Carbon-13'
     END IF
     READ (*,9010) line
     READ (line,9045,ERR=90) pnum
     IF (line.NE.' ') THEN
        CALL HAVE(IJ,12)
        Para(IJ,12) = pnum
     END IF
     GO TO 100
90   CALL DONTHAVE(IJ,12)
100  IF (Dowehave(IJ,13)) THEN
        WRITE (*,9030) 'Carbon-14 (% mod)', Para(IJ,13)
     ELSE
        WRITE (*,9035) 'Carbon-14 (% mod)'
     END IF
     READ (*,9010) line
     READ (line,9045,ERR=110) pnum
     IF (line.NE.' ') THEN
        CALL HAVE(IJ,13)
        Para(IJ,13) = pnum
     END IF
  END IF
  GO TO 120
110 CALL DONTHAVE(IJ,13)
120 IF (Jele(IJ,i).EQ.2) THEN
     IF (Dowehave(IJ,14)) THEN
        WRITE (*,9030) 'Sulfur-34', Para(IJ,14)
     ELSE
        WRITE (*,9035) 'Sulfur-34'
     END IF
     READ (*,9010) line
     READ (line,9045,ERR=130) pnum
     IF (line.NE.' ') THEN
        CALL HAVE(IJ,14)
        Para(IJ,14) = pnum
     END IF
  END IF
  GO TO 140
130 CALL DONTHAVE(IJ,14)
140 IF (Jele(IJ,i).EQ.15) THEN
     IF (Dowehave(IJ,15)) THEN
        WRITE (*,9030) 'Strontium 87/86', Para(IJ,15)
     ELSE
        WRITE (*,9035) 'Strontium 87/86'
     END IF
     READ (*,9010) line
     READ (line,9045,ERR=150) pnum
     IF (line.NE.' ') THEN
        CALL HAVE(IJ,15)
        Para(IJ,15) = pnum
     END IF
  END IF
  GO TO 160
150 CALL DONTHAVE(IJ,15)
160 IF (Jele(IJ,i).EQ.18) THEN
     IF (Dowehave(IJ,16)) THEN
        WRITE (*,9030) 'Nitrogen-15', Para(IJ,16)
     ELSE
        WRITE (*,9035) 'Nitrogen-15'
     END IF
     READ (*,9010) line
     READ (line,9045,ERR=170) pnum
     IF (line.NE.' ') THEN
        CALL HAVE(IJ,16)
        Para(IJ,16) = pnum
     END IF
  END IF
  GO TO 180
170 CALL DONTHAVE(IJ,16)
180 IF (Jele(IJ,i).NE.0) GO TO 80
190 IF (F(IJ).EQ.' ') WRITE (*,9040) Phase(IJ), 'no.'
  IF (F(IJ).EQ.'F') WRITE (*,9040) Phase(IJ), 'yes.'
  READ (*,9010) ans
  ans = UPCS(ans)
  IF (ans.EQ.'Y') F(IJ) = 'F'
  IF (ans.EQ.'N') F(IJ) = ' '
  RETURN
9000 FORMAT (//,' The phase ''',A,''' already exists.',/, &
       ' <R>ename this phase, or <D>elete it?')
9005 FORMAT (/,' Enter the new name for the phase ''',A,'''')
9010 FORMAT (A)
9015 FORMAT (/,' Enter transfer allowed for ''',A,'''. (1)', &
       ' dissolution, (2) precipitation,',/, &
       ' (3) both, or (4) isotopic exchange.',/, &
       ' Hit <Enter> for ',A,'.')
9020 FORMAT (/,' Enter amount to exchange.  (0 to cancel).')
9025 FORMAT (' <Enter> for ',F9.3)
9030 FORMAT (' Enter istopic composition of ',A, &
       ' in exchanging mineral:',/,' (Hit <Enter> for ',F10.3, &
       ', type ''N'' to use the phase value,',/, &
       ' or enter a new value)')
9035 FORMAT (' Enter isotopic composition of ',A, &
       ' in exchanging mineral:',/, &
       ' (Hit <Enter> to use the phase value, or enter a new value)')
9040 FORMAT (/,' Should ''',A,''' be included in every model?',/, &
       ' <Enter> for ',A)
9045 FORMAT (F10.0)
END SUBROUTINE TRANS
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
  ! All the letters in a line are changed to upper case.
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
  UPCS80 = ' '
  IF (n.GT.0) UPCS80 = LINE(:n)
  DO i = 1, n
     ich = ICHAR(LINE(i:i))
     IF (ich.GE.lowera .AND. ich.LE.lowerz) UPCS80(i:i) &
          = CHAR(ich-ishft)
  enddo
  RETURN
END FUNCTION UPCS80
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
SUBROUTINE VIEW
  USE max_size
  use filenames

  implicit none

  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  CHARACTER ans*1, line*80
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER LENS, iwell, i
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  EXTERNAL LENS, CLS

10 continue
  CALL CLS
  WRITE (*,9000)
  DO i = 1, Iflag(1)+1
     WRITE (*,9005) i, Wllnms(Well(i))(5:36)
  enddo
  WRITE (*,9005) Iflag(1)+2, Wllnms(Well(0))(5:36)
30 WRITE (*,9010)
  READ (*,9015) ans
  IF (ans.EQ.' ') THEN
     CALL SCREEN
     RETURN
  END IF
  READ (ans,'(I1)',ERR=30) iwell
  IF (iwell.LT.1 .OR. iwell.GT.(Iflag(1)+2)) GO TO 30
  IF (iwell.EQ.(Iflag(1)+2)) iwell = 0
  IF (Well(iwell).EQ.0) GO TO 30
  OPEN (UNIT=Wunit,FILE=Wfile(1:LENS(Wfile))//'.lon',STATUS='OLD', &
       ERR=10)

  READ (Wunit,'(A4)') line
  close(Wunit)
  if (line .eq. "2.14") then
     call view214(iwell)
  else
     call view2(iwell)
  endif
  goto 10

9000 FORMAT (//,' Select well to view:')
9005 FORMAT (' (',i1,')',20('.'),' ',a)
9010 FORMAT (' Enter your choice (<Enter> to quit)')
9015 FORMAT (A)
END SUBROUTINE VIEW
!
!
!
SUBROUTINE VIEW2(iwell)
  USE max_size
  use filenames
  IMPLICIT NONE
  !

  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  CHARACTER*10 inpt, labels(45), peval(0:4)
  CHARACTER*11 units(0:4)
  CHARACTER*83 frmt1
  CHARACTER ans*1, line*80
  DOUBLE PRECISION ddata(45), prnt(5)
  INTEGER count, dsfg(45), err, numwel, LENS, iwell, jk, itot, i,  &
       index, k, l, j, iu(4), show(45)
  EXTERNAL CLS, SCREEN, GETNO, LENS
  INTRINSIC CHAR
  DATA labels/'Temp.     ', 'pH        ', 'Diss. O2  ',  &
       'Field Alk.', 'Tritium   ', 'H2S       ', 'Ca2+      ',  &
       'Eh        ', 'Mg2+      ', 'Na+       ', 'K+        ',  &
       'Cl-       ', 'SO42-     ', 'F-        ', 'SiO2      ',  &
       'Br-       ', 'B         ', 'Ba2+      ', 'Li+       ',  &
       'Sr2+      ', 'Fe        ', 'Mn        ', 'NO3-N     ',  &
       'NH4-N     ', 'PO4-P     ', 'DOC       ', 'Sp. Cond. ',  &
       'Density   ', '13C TDIC  ', '14C TDIC  ', '34SSO4    ',  &
       '34SH2S    ', 'D         ', '18O       ', 'Diss CH4  ',  &
       'Sr 87/86  ', 'Al3+      ', 'N2-N      ', '15N N2    ',  &
       '15N NO3   ', '15N NH4   ', 'Depth     ', 'Casing    ',  &
       'Elevation ', '          '/
  DATA peval/'Redox Ign.', 'NOT USED  ', 'Diss. O2  ', 'Sato O2   ',  &
       'SO4/H2S   '/
  DATA units/'mmoles/l   ', 'meq/l      ', 'mg/l       ',  &
       'ppm        ', 'mmol/kg H2O'/
  DATA show/41*1, 4*0/
  DATA frmt1/ &
       '(/ 4X,************,4X,************,4X,************,4X,************,4X,************)'/
  !
  !   10 CALL CLS
  !      WRITE (*,9000)
  !      DO 20 i = 1, Iflag(1)+1
  !        WRITE (*,9005) i, Wllnms(Well(i))(5:36)
  !   20 CONTINUE
  !      WRITE (*,9005) Iflag(1)+2, Wllnms(Well(0))(5:36)
  !  30 WRITE (*,9010)
  !      READ (*,9015) ans
  !      IF (ans.EQ.' ') THEN
  !        CALL SCREEN
  !        RETURN
  !      END IF
  !      READ (ans,'(I1)',ERR=30) iwell
  !      IF (iwell.LT.1 .OR. iwell.GT.(Iflag(1)+2)) GO TO 30
  !      IF (iwell.EQ.(Iflag(1)+2)) iwell = 0
  !      IF (Well(iwell).EQ.0) GO TO 30
  !
  ! TIME TO READ THE WELL
  !
  iwell = Well(iwell)
  numwel = 0
  OPEN (UNIT=Wunit,FILE=Wfile(1:LENS(Wfile))//'.lon',STATUS='OLD', &
       ERR=130)
  REWIND (Wunit)
40 READ (Wunit,9015,ERR=90,END=90) line
  numwel = numwel+1
  IF (numwel.EQ.51) THEN
     WRITE (*,9020)
     GO TO 140
  ELSE
     READ (line,'(4(I1))',ERR=130) (iu(jk),jk=1,4)
     READ (Wunit,9015,END=120) line
     READ (line(66:70),'(I5)') itot
     IF (itot.NE.Tot(iwell)) THEN
        DO i = 1, 15
           READ (Wunit,9015,END=120) line
        enddo
        GO TO 40
     END IF
     DO Iadd = 1, 5
        READ (Wunit,9015,END=120) line
     enddo
     index = 0
     k = 0
70   k = k+1
     READ (Wunit,9015,END=120) line
     DO l = 5, 61, 14
        index = index+1
        inpt = line(l:l+9)
        CALL GETNO(inpt,ddata(index),dsfg(index),err)
        IF (err.GT.0) THEN
           IF (err.EQ.1) GO TO 130
           IF (err.EQ.2) THEN
              WRITE (*,9025) iwell, index, Wllnms(iwell)(1:79)
           ELSE IF (err.EQ.3) THEN
              WRITE (*,9030) iwell, index, Wllnms(iwell)(1:79)
           ELSE
              cycle
           END IF
           GO TO 140
        END IF
     enddo
     IF (k.LT.9) GO TO 70
  END IF
90 CLOSE (Wunit)
  CALL CLS

  WRITE (*,9035) Wllnms(iwell)(5:36), units(iu(1))
  IF (iu(3).EQ.0) labels(4) = 'Field Alk.'
  IF (iu(3).EQ.1) labels(4) = 'Carb. Alk.'
  IF (iu(3).EQ.2) labels(4) = 'TDIC      '
  IF (iu(3).EQ.3) labels(4) = 'CaCO3 Alk.'
  !      WRITE (*,9040) CHAR(27)//'[H', (labels(i),i=1,41)
  !      WRITE (*,'(1X,A4//)') CHAR(27)//'[H'
  call home
  WRITE (*,'(/,9(//3X,5(4X,A10)))') (labels(i),i=1,41)
  call home

  WRITE (*,'(//)') 
  DO i = 1, 41, 5
     count = 0
     DO j = i, i+4
        IF (j.EQ.8) THEN
           ! SPECIAL CASE: Eh
           IF (iu(2).NE.1) THEN
              frmt1((j-i)*16+7:(j-i)*16+18) = ''''//peval(iu(2))//''''
              cycle
           END IF
        END IF
        IF (show(j).EQ.0) THEN
           frmt1((j-i)*16+7:(j-i)*16+18) = '''          '''
        ELSE
           IF (dsfg(j).GE.0) THEN
              frmt1((j-i)*16+7:(j-i)*16+18) = '       f10.'//CHAR(dsfg(j)+48)
           ELSE IF (dsfg(j).EQ.-2) THEN
              frmt1((j-i)*16+7:(j-i)*16+18) = '''<'',f9.3  '
           ELSE
              frmt1((j-i)*16+7:(j-i)*16+18) = '''     --   '''
              cycle
           END IF
           count = count+1
           prnt(count) = ddata(j)
        END IF
     enddo
     WRITE (*,frmt1) (prnt(j),j=1,count)
  enddo
  WRITE (*,9045)
  READ (*,9015) line
  !      GO TO 10
  RETURN
120 WRITE (*,9050)
  GO TO 140
130 WRITE (*,9055) iwell, index, Wllnms(iwell)(1:79)
140 WRITE (*,9060)
  READ (*,9015) line
  RETURN
9000 FORMAT (//,' Select well to view:')
9005 FORMAT (' (',i1,')',20('.'),' ',a)
9010 FORMAT (' Enter your choice (<Enter> to quit)')
9015 FORMAT (A)
9020 FORMAT (/' **** Error ****'/ &
       ' Arrays not big enough to hold all of data base.'/ &
       ' ***************'/)
9025 FORMAT (/' **** Error ****'/' No decimal point in number.'/ &
       ' well#',I3,' index#',I3/1X,A79/'***************'/)
9030 FORMAT (/' **** Error ****'/' Unknown error at well#',I3, &
       ' index#',I3/1X,A79/'***************'/)
9035 FORMAT (1X,A32,10X,'Units: ',A11/)
9040 FORMAT (1X,A4,/,9(//3X,5(4X,A10)))
9045 FORMAT (/,' Hit <Enter> to continue')
9050 FORMAT (/' **** Error ****'/ &
       ' End of file before all data in last well was read.'/ &
       ' ***************'/)
9055 FORMAT (/' **** Error ****'/' Reading error at well#',I3, &
       ' index#',I3/1X,A79/'***************'/)
9060 FORMAT (/,' Hit <Enter> to continue>')
END SUBROUTINE VIEW2
!
!
!
SUBROUTINE VIEW214(iwell)
  USE max_size
  use filenames

  !
  IMPLICIT NONE
  INTEGER jcounter

  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  CHARACTER*10 labels(45), peval(0:4)
  CHARACTER*15 inpt
  CHARACTER*11 units(0:4)
  CHARACTER*83 frmt1
  CHARACTER ans*1, line*80
  DOUBLE PRECISION ddata(45), prnt(5)
  INTEGER count, dsfg(45), err, numwel, LENS, iwell, jk, itot, i,  &
       index, k, l, j, iu(4), show(45)
  EXTERNAL CLS, SCREEN, GETNO, LENS
  INTRINSIC CHAR
  DATA labels/'Temp.     ', 'pH        ', 'Diss. O2  ',  &
       'Field Alk.', 'Tritium   ', 'H2S       ', 'Ca2+      ',  &
       'Eh        ', 'Mg2+      ', 'Na+       ', 'K+        ',  &
       'Cl-       ', 'SO42-     ', 'F-        ', 'SiO2      ',  &
       'Br-       ', 'B         ', 'Ba2+      ', 'Li+       ',  &
       'Sr2+      ', 'Fe        ', 'Mn        ', 'NO3-N     ',  &
       'NH4-N     ', 'PO4-P     ', 'DOC       ', 'Sp. Cond. ',  &
       'Density   ', '13C TDIC  ', '14C TDIC  ', '34SSO4    ',  &
       '34SH2S    ', 'D         ', '18O       ', 'Diss CH4  ',  &
       'Sr 87/86  ', 'Al3+      ', 'N2-N      ', '15N N2    ',  &
       '15N NO3   ', '15N NH4   ', 'Depth     ', 'Casing    ',  &
       'Elevation ', '          '/
  DATA peval/'Redox Ign.', 'NOT USED  ', 'Diss. O2  ', 'Sato O2   ',  &
       'SO4/H2S   '/
  DATA units/'mmoles/l   ', 'meq/l      ', 'mg/l       ',  &
       'ppm        ', 'mmol/kg H2O'/
  DATA show/41*1, 4*0/
  DATA frmt1/ &
       '(/ 4X,************,4X,************,4X,************,4X,************,4X,************)'/
  !
  !   10 CALL CLS
  !      WRITE (*,9000)
  !      DO 20 i = 1, Iflag(1)+1
  !        WRITE (*,9005) i, Wllnms(Well(i))(5:36)
  !   20 CONTINUE
  !      WRITE (*,9005) Iflag(1)+2, Wllnms(Well(0))(5:36)
  !   30 WRITE (*,9010)
  !      READ (*,9015) ans
  !      IF (ans.EQ.' ') THEN
  !        CALL SCREEN
  !        RETURN
  !      END IF
  !      READ (ans,'(I1)',ERR=30) iwell
  !      IF (iwell.LT.1 .OR. iwell.GT.(Iflag(1)+2)) GO TO 30
  !      IF (iwell.EQ.(Iflag(1)+2)) iwell = 0
  !      IF (Well(iwell).EQ.0) GO TO 30
  !
  ! TIME TO READ THE WELL
  !
  iwell = Well(iwell)
  numwel = 0
  OPEN (UNIT=Wunit,FILE=Wfile(1:LENS(Wfile))//'.lon',STATUS='OLD', &
       ERR=130)
  REWIND (Wunit)
  !     File format
  READ (Wunit,9015,ERR=90,END=90) line

40 continue
  !     Well name
  READ (Wunit,9015,ERR=90,END=90) line

  numwel = numwel+1
  IF (numwel.EQ.51) THEN
     WRITE (*,9020)
     GO TO 140
  endif
  READ (line,'(4(I1))',ERR=130) (iu(jk),jk=1,4)
  !     Lat/lon
  READ (Wunit,'(A)',END=120) line
  !     Well number
  !        READ (line(66:70),'(I5)') itot
  READ (Wunit,'(I15)',END=120,ERR=130) itot
  IF (itot.NE.Tot(iwell)) THEN
     DO i = 1, 48
        READ (Wunit,'(A)',END=120) line
     enddo
     GO TO 40
  END IF
  !
  !  Found right well
  !
  !     Total number of wells
  READ (Wunit,'(A)',END=120) line
  !     Addresses
  DO Iadd = 1, 5
     READ (Wunit,9015,END=120) line
  enddo
  !      index = 0
  !      k = 0
  !   70 k = k+1
  !      READ (Wunit,9015,END=120) line
  !      DO 80 l = 5, 61, 14
  !         index = index+1
  !         inpt = line(l:l+9)
  !         CALL GETNO(inpt,ddata(index),dsfg(index),err)
  !         IF (err.GT.0) THEN
  !            IF (err.EQ.1) GO TO 130
  !            IF (err.EQ.2) THEN
  !               WRITE (*,9025) iwell, index, Wllnms(iwell)(1:79)
  !            ELSE IF (err.EQ.3) THEN
  !               WRITE (*,9030) iwell, index, Wllnms(iwell)(1:79)
  !            ELSE
  !               GO TO 80
  !            END IF
  !            GO TO 140
  !         END IF
  !   80 CONTINUE
  !      IF (k.LT.9) GO TO 70
  !
  !     Read data
  !
  jcounter = 41
  do index = 1, jcounter 
     READ (Wunit,'(A15)',END=120,ERR=130) line
     inpt = line(1:15)
     CALL GETNO214(inpt,ddata(index),dsfg(index),err)
     IF (err.GT.0) THEN
        IF (err.EQ.1) GO TO 130
        IF (err.EQ.2) THEN
           WRITE (*,9025) iwell, index, Wllnms(iwell)(1:79)
        ELSE IF (err.EQ.3) THEN
           WRITE (*,9030) iwell, index, Wllnms(iwell)(1:79)
        END IF
        goto 140
     END IF
  enddo
90 CLOSE (Wunit)
  !
  !  Paint view screen
  !
  CALL CLS
  WRITE (*,9035) Wllnms(iwell)(5:36), units(iu(1))
  IF (iu(3).EQ.0) labels(4) = 'Field Alk.'
  IF (iu(3).EQ.1) labels(4) = 'Carb. Alk.'
  IF (iu(3).EQ.2) labels(4) = 'TDIC      '
  IF (iu(3).EQ.3) labels(4) = 'CaCO3 Alk.'
  !      WRITE (*,9040) CHAR(27)//'[H', (labels(i),i=1,41)
  !      WRITE (*,'(1X,A4//)') CHAR(27)//'[H'
  call home
  WRITE (*,'(/,9(//3X,5(4X,A10)))') (labels(i),i=1,41)
  call home
  WRITE (*,'(//)') 
  !     Write more significant figures for Sr87
  dsfg(36) = 6
  DO i = 1, 41, 5
     count = 0
     DO j = i, i+4
        IF (j.EQ.8) THEN
           ! SPECIAL CASE: Eh
           IF (iu(2).NE.1) THEN
              frmt1((j-i)*16+7:(j-i)*16+18) = ''''//peval(iu(2))//''''
              cycle
           END IF
        END IF
        IF (show(j).EQ.0) THEN
           frmt1((j-i)*16+7:(j-i)*16+18) = '''          '''
        ELSE
           IF (dsfg(j).GE.0) THEN
              frmt1((j-i)*16+7:(j-i)*16+18) = '       f10.'//CHAR(dsfg(j)+48)
           ELSE IF (dsfg(j).EQ.-2) THEN
              frmt1((j-i)*16+7:(j-i)*16+18) = '''<'',f9.3  '
           ELSE
              frmt1((j-i)*16+7:(j-i)*16+18) = '''     --   '''
              cycle
           END IF
           count = count+1
           prnt(count) = ddata(j)
        END IF
     enddo
     WRITE (*,frmt1) (prnt(j),j=1,count)
  enddo
  WRITE (*,9045)
  READ (*,9015) line
  RETURN
120 WRITE (*,9050)
  GO TO 140
130 WRITE (*,9055) iwell, index, Wllnms(iwell)(1:79)
140 WRITE (*,9060)
  READ (*,9015) line
  close(Wunit)
  RETURN
9000 FORMAT (//,' Select well to view:')
9005 FORMAT (' (',i1,')',20('.'),' ',a)
9010 FORMAT (' Enter your choice (<Enter> to quit)')
9015 FORMAT (A)
9020 FORMAT (/' **** Error ****'/ &
       ' Arrays not big enough to hold all of data base.'/ &
       ' ***************'/)
9025 FORMAT (/' **** Error ****'/' No decimal point in number.'/ &
       ' well#',I3,' index#',I3/1X,A79/'***************'/)
9030 FORMAT (/' **** Error ****'/' Unknown error at well#',I3, &
       ' index#',I3/1X,A79/'***************'/)
9035 FORMAT (1X,A32,10X,'Units: ',A11/)
9040 FORMAT (1X,A4,/,9(//3X,5(4X,A10)))
9045 FORMAT (/,' Hit <Enter> to continue')
9050 FORMAT (/' **** Error ****'/ &
       ' End of file before all data in last well was read.'/ &
       ' ***************'/)
9055 FORMAT (/' **** Error ****'/' Reading error at well#',I3, &
       ' index#',I3/1X,A79/'***************'/)
9060 FORMAT (/,' Hit <Enter> to continue>')
END SUBROUTINE VIEW214
!
!
!
SUBROUTINE WARN
  USE max_size
  use filenames
  IMPLICIT NONE
  !
  ! This subroutine prints warnings if data is missing or if the model
  ! will not run.
  ! Called by SCREEN every time the screen is drawn.
  !

  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER LENS, numwarn, nbadcon, ii, icount, icheck, j, k, nbadpha,  &
       itotal, ibadones(10)
  EXTERNAL LENS
  !
  numwarn = 0
  !
  !  Check to see if there are enough constraints (EVAP and each mixing
  !  well take up one constraint)
  !
  IF (Noele-Iflag(1)-Iflag(6).GT.Nopha) THEN
     WRITE (*,9000) Noele-Iflag(1)-Iflag(6)-Nopha
     numwarn = numwarn+1
  END IF
  !
  ! Check to see if there are any constraints that aren't in any of the
  ! phases.
  !
  nbadcon = 0
  DO icount = 1, Noele
     icheck = Iele(icount)
     ! For isotopes, check for element, not isotope
     IF (icheck.EQ.21 .OR. icheck.EQ.22) icheck = 1
     IF (icheck.EQ.23) icheck = 2
     IF (icheck.EQ.24) icheck = 15
     IF (icheck.EQ.25) icheck = 18
     j = 0
10   j = j+1
     ! If there are no more phases, this constraint is bad.
     IF (j.GT.Nopha) THEN
        ! Oh no! The element isn't in any of the phases
        nbadcon = nbadcon+1
        ibadones(nbadcon) = icount
        ! Handle special cases
     ELSE IF (.NOT.(Phase(j).EQ.'EXCHANGE'.AND.(icheck.EQ.3.OR.icheck &
          .EQ.5.OR.icheck.EQ.6))) THEN
        IF (Phase(j).NE.'CO2-CH4 ' .OR. icheck.NE.20) THEN
           k = 0
20         k = k+1
           ! If there aren't more constraints in this phase, check the next one
           IF (Jele(j,k).EQ.0) GO TO 10
           ! If the constraint found, go to the next constraint to check
           IF (Jele(j,k).NE.icheck) GO TO 20
        END IF
     END IF
  enddo
  IF (nbadcon.GT.Iflag(1)+Iflag(6)) THEN
     DO icount = 1, nbadcon
        WRITE (*,9005) Elelong(Iele(ibadones(icount))) &
             (1:LENS(Elelong(Iele(ibadones(icount)))))
     enddo
     numwarn = numwarn+nbadcon
  ELSE IF (nbadcon.EQ.Iflag(1)+Iflag(6) .AND. nbadcon.EQ.1) THEN
     IF (Iflag(6).EQ.1) THEN
        WRITE (*,9015) Elelong(Iele(ibadones(1)))
     ELSE
        WRITE (*,9010) Elelong(Iele(ibadones(1)))
     END IF
     numwarn = numwarn+1
  END IF
  !
  ! Now we check for phases that won't do anything (they have none of the
  !  chosen constraints.
  !
  nbadpha = 0
  DO icount = 1, Nopha
     j = 0
50   j = j+1
     IF (Jele(icount,j).NE.0 .OR. Phase(icount).EQ.'EXCHANGE') THEN
        k = 0
60      k = k+1
        IF (k.GT.Noele) THEN
           IF (Phase(icount).EQ.'EXCHANGE') GO TO 70
           GO TO 50
        END IF
        IF (Jele(icount,j).EQ.Iele(k)) cycle
        IF (Phase(icount).EQ.'EXCHANGE') THEN
           IF (Iele(k).EQ.3 .OR. Iele(k).EQ.5 .OR. Iele(k).EQ.6) &
                cycle
        END IF
        IF (Phase(icount).EQ.'CO2-CH4') THEN
           IF (Iele(k).EQ.20) cycle
        END IF
        GO TO 60
     END IF
70   nbadpha = nbadpha+1
     WRITE (*,9020) Phase(icount)(1:LENS(Phase(icount)))
  enddo
  numwarn = numwarn+nbadpha
  !
  !  Phase check is done, now it's time for check whether data for
  !  constraint was present in .PAT file
  !
  DO icount = 1, Noele
     icheck = Iele(icount)
     itotal = 0
     DO ii = 0, Iflag(1)+1
        IF (Nodata(Well(ii),icheck).EQ.1) itotal = itotal+1
     enddo
     IF (itotal.NE.0) THEN
        numwarn = numwarn+1
        WRITE (*,9025) Elelong(icheck)(1:LENS(Elelong(icheck))),  &
             itotal
     END IF
  enddo
  IF (numwarn.GT.0) THEN
     Ilength = Ilength+numwarn+1
     WRITE (*,9030)
  END IF
  RETURN
9000 FORMAT (' There are ',I2,' too few phases for the given const', &
       'raints. No models will run.')
9005 FORMAT (1X,A,' is not contained in any phase. No models will run.' &
       )
9010 FORMAT (' The mixing ratio will be determined by ',A)
9015 FORMAT (' The evaporation/dilution factor will be determined by ', &
       A)
9020 FORMAT (1X,A,' does not contain any of the chosen constraints.')
9025 FORMAT (' Warning: There is no data for ',A,' in ',I1,' of the', &
       ' wells: zero will be used')
9030 FORMAT (1X,79('='))
END SUBROUTINE WARN
!
!
!

!
!
!
SUBROUTINE WELLS
  USE max_size
  IMPLICIT NONE
  !
  ! The specific wells to be used are selected here, as well as whether
  ! mixing will be considered.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER i, ino, j
  CHARACTER*1 ans, UPCS
  LOGICAL YN
  EXTERNAL CLS, EDITMIX, WLLIST, YN, UPCS, POSCUR
  !
  CALL CLS
  IF (Iedit.EQ.3) Iedit = 0
  IF (Iedit.EQ.0) THEN
     Iflag(1) = 0
10   CALL EDITMIX(1)
     WRITE (*,9000) Wllnms(Well(1))(5:36)
     DO i = 1, Iflag(1)
        WRITE (*,9005) Wllnms(Well(i+1))(5:36)
     enddo
     WRITE (*,9010) Wllnms(Well(0))(5:36)
     IF (.NOT.YN('Y')) THEN
        DO ino = 0, 5
           Well(ino) = 0
        enddo
        CALL CLS
        GO TO 10
     END IF
  ELSE
40   CALL CLS
     WRITE (*,9015)
     DO i = 1, Iflag(1)+1
        WRITE (*,9020) i, Wllnms(Well(i))(5:36)
     enddo
     WRITE (*,9020) Iflag(1)+2, Wllnms(Well(0))(5:36)
     CALL POSCUR(Iflag(1)+1)
     WRITE (*,9025)
     READ (*,9030) ans
     ans = UPCS(ans)
     IF (ans.EQ.' ') RETURN
     IF (ans.EQ.'D') THEN
        IF (Iflag(1).EQ.0) THEN
           WRITE (*,9040)
           READ (*,9030) ans
        ELSE
           WRITE (*,9035)
           READ (*,9030,ERR=40) ans
           READ (ans,'(I1)',ERR=40) i
           IF (i.GE.1 .AND. i.LE.Iflag(1)+1) THEN
              DO j = i, Iflag(1)
                 Well(j) = Well(j+1)
              enddo
              Well(Iflag(1)+1) = 0
              Iflag(1) = Iflag(1)-1
           END IF
        END IF
        GO TO 40
     END IF
     IF (ans.EQ.'A') THEN
        IF (Iflag(1).EQ.4) THEN
           WRITE (*,9045)
           READ (*,9030) ans
        ELSE
           Iflag(1) = Iflag(1)+1
           CALL WLLIST(Iflag(1)+1)
        END IF
        GO TO 40
     END IF
     READ (ans,'(I1)',ERR=40) i
     IF (i.GE.1 .AND. i.LE.Iflag(1)+2) THEN
        IF (i.EQ.Iflag(1)+2) i = 0
        CALL WLLIST(i)
     END IF
     GO TO 40
  END IF
  RETURN
9000 FORMAT (///,' Initial well: ',A32)
9005 FORMAT (' Initial well: ',A32)
9010 FORMAT (' Final well  : ',A32,//, &
       ' Are these correct? (<Enter> for yes)')
9015 FORMAT (' Current wells',/,' -------------')
9020 FORMAT (3X,I1,': ',A32)
9025 FORMAT (/, &
       ' Enter D to delete a well, A to add a well, or the number of' &
       ,/,' the well you wish to change. <Enter> when done.')
9030 FORMAT (A1)
9035 FORMAT (' Enter number of well to delete or <Enter> to abort.')
9040 FORMAT ( &
       ' You can not have fewer than 2 wells. Hit <Enter> to continue')
9045 FORMAT (' You can not have more than 5 mixing wells. Hit <Enter>', &
       ' to continue.')
END SUBROUTINE WELLS
!
!
!
SUBROUTINE WLLIST(II)
  USE max_size
  IMPLICIT NONE
  !
  ! The wells in a given well file are listed.  After 40 wells, the number
  ! of the well to be used may be entered, instead of having to see the
  ! rest of the wells.
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  CHARACTER*3 bans
  INTEGER II, i, ij, iij
  !
10 IF (II.EQ.0) THEN
     WRITE (*,9000)
  ELSE
     WRITE (*,9005) II
  END IF
  i = 1
  ij = 1
20 IF (i.GT.MAXWELLS*ij) THEN
     WRITE (*,9010)
     ij = ij+1
     READ (*,'(A)') bans
     IF (bans.NE.'  ') THEN
        READ (bans,'(I3)',ERR=30) Well(II)
        Isdocrs(II) = 0
        GO TO 50
     END IF
  END IF
30 IF (i.LE.Nwlls) THEN
     !
     WRITE (*,9015) i, Wllnms(i)(5:36), i+1, Wllnms(i+1)(5:36)
     !
     IF (i.NE.Nwlls .AND. i+1.NE.Nwlls) THEN
        i = i+2
        GO TO 20
     END IF
  ELSE
     WRITE (*,9020) i, Wllnms(i)(5:36)
  END IF
40 WRITE (*,9025)
  IF (Well(II).GT.0) WRITE (*,9030) Wllnms(Well(II))(5:36)
  READ (*,'(A)') bans
  IF (bans.NE.' ' .OR. Well(II).LE.0) THEN
     READ (bans,'(I3)',ERR=10) iij
     IF (iij.LT.1 .OR. iij.GT.Nwlls) GO TO 40
     Well(II) = iij
     Isdocrs(II) = 0
  END IF
50 RETURN
9000 FORMAT (/,10x,' Final Well',/)
9005 FORMAT (/,10x,' Initial Well',i2,/)
9010 FORMAT ( &
       ' Hit RETURN to see next page or enter number of well to use.')
9015 FORMAT (I4,': ',A32,5X,I3,': ',A32)
9020 FORMAT (I4,': ',A32)
9025 FORMAT (/,' Input number of well.')
9030 FORMAT (' <Enter> for ',A32)
END SUBROUTINE WLLIST
!
!
!
LOGICAL FUNCTION YN(DEF)
  !
  ! A logical true/false is returned as an answer to a yes/no question.
  !
  CHARACTER*1 yesno, DEF
  !
10 READ (*,'(A)') yesno
  IF (yesno.EQ.' ') yesno = DEF
  IF (yesno.EQ.'Y' .OR. yesno.EQ.'y') THEN
     YN = .TRUE.
  ELSE IF (yesno.EQ.'N' .OR. yesno.EQ.'n') THEN
     YN = .FALSE.
  ELSE
     WRITE (*,9000)
     GO TO 10
  END IF
  RETURN
9000 FORMAT (/,' Please enter ''Y'' or ''N''.')
END FUNCTION YN
!
!
!
SUBROUTINE RUN(NUMRUN)
  USE max_size
  use filenames
  IMPLICIT NONE
  !
  ! The model is run, with output going to the screen or a file.  The
  ! phases are prepared to be run and all the data is put into the NEWBAL
  ! format.  NEWBAL is then called.
  !

  CHARACTER Pname*8, Ename*2, Force*1
  COMMON /CHAR3 / Pname(39), Ename(39), Force(39)
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1), &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION Clmain, Cleach, Pdat, Res, Sfinal, Sinit, Maxdel, &
       Mindel, Dfinal
  COMMON /DP3   / Clmain(202,40), Cleach(202,40), Pdat(40,40), &
       Res(100), Sfinal(39), Sinit(5,59), Maxdel(39), &
       Mindel(39), Dfinal
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40), &
       Disalong, Usera(5)
  DOUBLE PRECISION Pcoef
  COMMON /DP6   / Pcoef(39,39)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50), &
       Isdocrs(0:5)
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone, &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39), &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  DOUBLE PRECISION pc(36), ca
  INTEGER je(36), i, j, k, loop, NUMRUN
  CHARACTER*1 UPCS, ittemp(39), ans
  CHARACTER*80 UPCS80
  EXTERNAL CLS, UPCS80, INCLISO, BALN, SCREEN, UPCS
  !
  Nrun = NUMRUN
  CALL CLS
  Iunit = Tunit
  IF (Nrun.EQ.0) Iunit = Wunit
  IF (Iflag(1).EQ.0) THEN
     WRITE (Iunit,9000) Wllnms(Well(1))(5:36), Wllnms(Well(0))(5:36)
  ELSE
     WRITE (Iunit,9005) (i,Wllnms(Well(i))(5:36),i=1,Iflag(1)+1)
     WRITE (Iunit,9010) Wllnms(Well(0))(5:36), (i,i=1,Iflag(1)+1)
  END IF
  DO i = 1, Noele
     Ename(i) = Eleshort(Iele(i))
     Sfinal(i) = Dbdata(Well(0),Iele(i))
     DO k = 1, Iflag(1)+1
        Sinit(k,i) = Dbdata(Well(k),Iele(i))
        IF (Iele(i).EQ.21) Sinit(k,i) = Sinit(k,i)+Dbdata(Well(k),42) &
             *Dbdata(Well(k),44) &
             +Dbdata(Well(k),43) &
             *Dbdata(Well(k),45)
        IF (Iele(i).EQ.22) Sinit(k,i) = Sinit(k,i)+Dbdata(Well(k),42) &
             *Dbdata(Well(k),46) &
             +Dbdata(Well(k),43) &
             *Dbdata(Well(k),47)
        IF (Iele(i).EQ.20) THEN
           DO j = 1, Noele
              IF (Iele(j).EQ.1) THEN
                 IF (Isdocrs(k).EQ.0) THEN
                    Sinit(k,i) = Sinit(k,i)+Dbdata(Well(k),34) &
                         +Dbdata(Well(k),43)*Dbdata(Well(k),48)
                 ELSE
                    Sinit(k,i) = Sinit(k,i)+Dbdata(Well(k),34) &
                         +Dbdata(Well(k),43)*Dbdata(Well(k),49)
                 END IF
              END IF
              IF (Iele(j).EQ.2) Sinit(k,i) = Sinit(k,i) &
                   +Dbdata(Well(k),30) &
                   *6-Dbdata(Well(k),29)*2
              IF (Iele(j).EQ.16) Sinit(k,i) = Sinit(k,i) &
                   +Dbdata(Well(k),35)
              IF (Iele(j).EQ.17) Sinit(k,i) = Sinit(k,i) &
                   +Dbdata(Well(k),40)
              IF (Iele(j).EQ.18) Sinit(k,i) = Sinit(k,i) &
                   +Dbdata(Well(k),33)
           enddo
        END IF
     enddo
     IF (Iele(i).EQ.21) Sfinal(i) = Sfinal(i)+Dbdata(Well(0),42) &
          *Dbdata(Well(0),44) &
          +Dbdata(Well(0),43) &
          *Dbdata(Well(0),45)
     IF (Iele(i).EQ.22) Sfinal(i) = Sfinal(i)+Dbdata(Well(0),42) &
          *Dbdata(Well(0),46) &
          +Dbdata(Well(0),43) &
          *Dbdata(Well(0),47)
     IF (Iele(i).EQ.20) THEN
        DO j = 1, Noele
           IF (Iele(j).EQ.1) THEN
              IF (Isdocrs(0).EQ.0) THEN
                 Sfinal(i) = Sfinal(i)+Dbdata(Well(0),34) &
                      +Dbdata(Well(0),43)*Dbdata(Well(0),48)
              ELSE
                 Sfinal(i) = Sfinal(i)+Dbdata(Well(0),34) &
                      +Dbdata(Well(0),43)*Dbdata(Well(0),49)
              END IF
           END IF
           IF (Iele(j).EQ.2) Sfinal(i) = Sfinal(i)+Dbdata(Well(0),30) &
                *6-Dbdata(Well(0),29)*2
           IF (Iele(j).EQ.16) Sfinal(i) = Sfinal(i)+Dbdata(Well(0),35)
           IF (Iele(j).EQ.17) Sfinal(i) = Sfinal(i)+Dbdata(Well(0),40)
           IF (Iele(j).EQ.18) Sfinal(i) = Sfinal(i)+Dbdata(Well(0),33)
        enddo
     END IF
     !        WRITE (Iunit,9015) Ename(i), Sfinal(i),  &
     !            (Sinit(k,i),k=1,Iflag(1)+1)
     if (iele(i) .eq. 24) then
        WRITE (Iunit,9016) Ename(i), Sfinal(i),  &
             (Sinit(k,i),k=1,Iflag(1)+1)
     else
        WRITE (Iunit,9015) Ename(i), Sfinal(i),  &
             (Sinit(k,i),k=1,Iflag(1)+1)
     endif
  enddo
  IF (Nrun.NE.0) THEN
     WRITE (*,9020)
     READ (*,9025) ans
     IF (UPCS(ans).EQ.'Q') GO TO 160
  END IF
  WRITE (Iunit,*)
  DO i = 1, Nopha
     DO j = 1, 36
        je(j) = Jele(i,j)
        pc(j) = Pcoeff(i,j)
     enddo
     ittemp(i) = It(i)
     IF (It(i).EQ.'*') ittemp(i) = ' '
     IF (UPCS80(Phase(i)).EQ.'EXCHANGE') THEN
        je(1) = 3
        je(2) = 6
        je(3) = 5
        je(4) = 0
        pc(2) = 2.0D0
        IF (Iflag(2).EQ.1) THEN
           IF (Dbdata(Well(0),3)+Dbdata(Well(0),5).GT.0.0D0) THEN
              ca = -Dbdata(Well(0),3) &
                   /(Dbdata(Well(0),3)+Dbdata(Well(0),5))
           ELSE
              ca = -1.0D0
           END IF
        END IF
        IF (Iflag(2).EQ.2) ca = -0.5D0
        IF (Iflag(2).EQ.3) ca = -1.0D0
        IF (Iflag(2).EQ.4) ca = -P(2)
        pc(1) = ca
        pc(3) = -1.0D0-ca
     END IF
     IF (UPCS80(Phase(i)).EQ.'CO2-CH4 ') THEN
        je(2) = 20
        je(3) = 0
        pc(2) = (P(1)*8.0D0)-4.0D0
     END IF
     j = 0
60   j = j+1
     IF (je(j).NE.0) GO TO 60
     j = j-1
     CALL INCLISO(je,pc,i,j,1)
     Pname(i) = Phase(i)
     Transfer(i) = ittemp(i)
     Force(i) = F(i)
     DO k = 1, 36
        IF (k.LE.j) THEN
           Pelt(i,k) = Eleshort(je(k))
           Pcoef(i,k) = pc(k)
        ELSE
           Pelt(i,k) = '  '
           Pcoef(i,k) = 0.0D0
        END IF
     enddo
  enddo
  Nmins = Nopha
  j = 0
  DO i = 1, Nmins
     IF (Force(i).EQ.'F') THEN
        j = j+1
        Pname(Nmins+1) = Pname(i)
        Force(Nmins+1) = Force(i)
        Transfer(Nmins+1) = Transfer(i)
        DO k = 1, 39
           Pelt(Nmins+1,k) = Pelt(i,k)
           Pcoef(Nmins+1,k) = Pcoef(i,k)
        enddo
        DO loop = i, j+1, -1
           Pname(loop) = Pname(loop-1)
           Force(loop) = Force(loop-1)
           Transfer(loop) = Transfer(loop-1)
           DO k = 1, 39
              Pelt(loop,k) = Pelt(loop-1,k)
              Pcoef(loop,k) = Pcoef(loop-1,k)
           enddo
        enddo
        Pname(j) = Pname(Nmins+1)
        Force(j) = Force(Nmins+1)
        Transfer(j) = Transfer(Nmins+1)
        DO k = 1, 39
           Pelt(j,k) = Pelt(Nmins+1,k)
           Pcoef(j,k) = Pcoef(Nmins+1,k)
        enddo
     END IF
  enddo
  DO i = 1, Nopha
     k = 0
140  k = k+1
     IF (Pelt(i,k).NE.'  ') GO TO 140
     k = k-1
     WRITE (Iunit,9030) Pname(i), (Pelt(i,j),Pcoef(i,j),j=1,k)
  enddo
  IF (Nrun.NE.0) THEN
     WRITE (*,9020)
     READ (*,9025) ans
     IF (UPCS(ans).EQ.'Q') GO TO 160
  END IF
  Imix = Iflag(1)
  CALL BALN
  CLOSE (Wunit)
160 CALL SCREEN
  RETURN
9000 FORMAT (' Initial Well : ',a,/,' Final Well   : ',a,//, &
       '          Final     Initial')
9005 FORMAT (' Initial Well',I2,' : ',A)
9010 FORMAT (' Final well     : ',A//,'          Final    ', &
       5(:,'Initial',I2,'   '))
9015 FORMAT (1X,A2,6(F12.4))
9016 FORMAT (1X,A2,6(F12.8))
9020 FORMAT (/,' Hit <Q> to quit, or <Enter> to continue')
9025 FORMAT (A)
9030 FORMAT (1X,A8,5(1X,A2,1X,F8.4),7(:,/,9X,5(1X,A2,1X,F8.4)))
END SUBROUTINE RUN
!
!
!
SUBROUTINE BALN
  USE max_size
  IMPLICIT NONE
  !
  CHARACTER Pname*8, Ename*2, Force*1
  COMMON /CHAR3 / Pname(39), Ename(39), Force(39)
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION Pcoef
  COMMON /DP6   / Pcoef(39,39)
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  LOGICAL First, Quit
  COMMON /LOG3  / First, Quit
  INTEGER i, inextprint, istop, j, k, lastfound, maxignore,  &
       noldignore, numfound
  CHARACTER*1 UPCS, ans
  EXTERNAL INIT, NEXT, RUNONE, CLS, PRINT, CISO, UPCS
  !
  numfound = 0
  inextprint = 100
  IF (Imix.GT.0 .OR. Iflag(6).EQ.1) THEN
     k = 1+Imix+Iflag(6)
     Nmins = Nmins+k
     DO i = Nmins, k+1, -1
        Pname(i) = Pname(i-k)
        Transfer(i) = Transfer(i-k)
        Force(i) = Force(i-k)
        DO j = 1, 39
           Pelt(i,j) = Pelt(i-k,j)
           Pcoef(i,j) = Pcoef(i-k,j)
        enddo
     enddo
     DO i = 1, Imix+1+Iflag(6)
        WRITE (Pname(i),9000) i
        Transfer(i) = '+'
        Force(i) = 'F'
     enddo
     IF (Iflag(6).EQ.1) THEN
        Pname(Imix+1+Iflag(6)) = 'WATER'
        Transfer(Imix+1+Iflag(6)) = ' '
     END IF
  END IF
  Numdrop = Nmins-Noele-Imix-Iflag(6)
  IF (Numdrop.LT.0) Numdrop = 0
  CALL INIT
  IF (Ierror.EQ.1) THEN
     !  ??? I don't know about this case....needs to be checked out.
     WRITE (*,*) 'Stopped - Hit <Enter>'
     READ (*,9005) ans
     RETURN
  END IF
  lastfound = 99
  First = .TRUE.
  CALL NEXT
  IF (.NOT.Quit) THEN
     maxignore = 99
     Igotone = 0
40   noldignore = maxignore
     CALL RUNONE(maxignore)
     IF (Igotone.EQ.1) THEN
        IF (lastfound.EQ.Numdrop .AND. maxignore.EQ.noldignore) THEN
           numfound = numfound+1
        ELSE
           lastfound = Numdrop
           numfound = 1
        END IF
     END IF
     IF (Numtest.GT.inextprint) THEN
        inextprint = inextprint+100
        CALL CLS
        IF (numfound.EQ.1) THEN
           WRITE (*,9015) Numtest, numfound
        ELSE
           WRITE (*,9010) Numtest, numfound
        END IF
        IF (maxignore.GT.0) WRITE (*,9030) maxignore
     END IF
     Igotone = 0
     CALL NEXT
     IF (.NOT.Quit) GO TO 40
  END IF
  ! VERY IMPORTANT!!!! If this part is included, models with fewer
  !     phases than constraints are possible. If no inequalities are used,
  !     this just slows down the code. Put it back if uncertainties are used
  !
  !      if (lastfound.eq.numdrop) then
  !        numdrop=numdrop+1
  !        maxignore=99
  !        go to 40
  !      end if
  IF (lastfound.EQ.99) THEN
     IF (Iunit.EQ.Tunit) THEN
        WRITE (Iunit,9035)
        READ (*,9005) ans
     ELSE
        WRITE (Iunit,9040)
     END IF
     RETURN
  END IF
  istop = -1
  IF (Iunit.EQ.Tunit .AND. numfound.GT.1) THEN
     CALL CLS
     WRITE (Iunit,9010) Numtest, numfound
     IF (maxignore.GT.0) WRITE (Iunit,9030) maxignore
50   WRITE (Iunit,9045)
     READ (*,9005,ERR=50) ans
     ans = UPCS(ans)
     IF (ans.EQ.'A') istop = 0
     IF (ans.EQ.'N') RETURN
     IF (ans.EQ.' ') istop = 1
     IF (istop.EQ.-1) GO TO 50
  ELSE IF (numfound.EQ.1) THEN
     CALL CLS
     IF (Numtest.EQ.1) THEN
        WRITE (Iunit,9020) Numtest, numfound
     ELSE
        WRITE (Iunit,9015) Numtest, numfound
     END IF
     IF (maxignore.GT.0) WRITE (Iunit,9030) maxignore
  ELSE
     IF (Numtest.EQ.1) THEN
        WRITE (Iunit,9025) Numtest, numfound
     ELSE
        WRITE (Iunit,9010) Numtest, numfound
     END IF
     IF (maxignore.GT.0) WRITE (Iunit,9030) maxignore
     WRITE (Iunit,9050)
     istop = 0
     !
  END IF
  Numdrop = lastfound
  First = .TRUE.
  Inum = 0
  CALL NEXT
  !  We shouldn't need to check for errors, we know there will be at least
  !  one good model.
60 CALL RUNONE(maxignore)
  IF (Igotone.NE.0) THEN
     Inum = Inum+1
     CALL PRINT
     IF (Nrun.EQ.0 .AND. Iflag(3).EQ.1) THEN
        CALL CISO(2)
        CALL CISO(1)
        GO TO 90
     END IF
     IF (Inum.GE.numfound) THEN
        IF (Iflag(3).EQ.0 .OR. istop.EQ.0) THEN
           WRITE (*,9055)
           READ (*,9005) ans
           RETURN
        END IF
70      WRITE (*,9070)
        READ (*,9005) ans
        ans = UPCS(ans)
        IF (ans.EQ.'S') THEN
           CALL CISO(1)
           GO TO 70
        ELSE IF (ans.EQ.'C') THEN
           CALL CISO(2)
           GO TO 70
        ELSE IF (ans.EQ.'E') THEN
           CALL CISO(3)
           GO TO 70
        END IF
        RETURN
     ELSE IF (istop.EQ.1) THEN
        IF (Iflag(3).EQ.0) THEN
           WRITE (*,9060)
           READ (*,9005) ans
           IF (ans.NE.' ') RETURN
        ELSE
80         WRITE (*,9075)
           READ (*,9005) ans
           ans = UPCS(ans)
           IF (ans.EQ.'S') THEN
              CALL CISO(1)
              GO TO 80
           ELSE IF (ans.EQ.'C') THEN
              CALL CISO(2)
              GO TO 80
           ELSE IF (ans.EQ.'E') THEN
              CALL CISO(3)
              GO TO 80
           END IF
           IF (ans.NE.' ') RETURN
        END IF
     END IF
  END IF
90 CALL NEXT
  IF (.NOT.Quit) GO TO 60
  IF (Nrun.EQ.0) RETURN
  STOP 'Inconsistent results.'
9000 FORMAT ('Init',I2)
9005 FORMAT (A)
9010 FORMAT (/,14X,I9,' models checked',/,14X,I9,' models found')
9015 FORMAT (/,14X,I9,' models checked',/,14X,I9,' model found')
9020 FORMAT (/,14X,I9,' model checked',/,14X,I9,' model found')
9025 FORMAT (/,14X,I9,' model checked',/,14X,I9,' models found')
9030 FORMAT (9X,'(Ignoring ',I2, &
       ' dissolution/precipitation constraints)')
9035 FORMAT (' No models found. Hit <Enter> to continue.')
9040 FORMAT (' No models found.')
9045 FORMAT ( &
       ' Display them <A>ll at once, <Return> for each, or <N>one.')
9050 FORMAT (/,/)
9055 FORMAT (' No more models, hit <Enter> to continue')
9060 FORMAT (' Hit <Enter> to continue, any other key to quit.')
9070 FORMAT (' No more models, hit <S> to show Rayleigh data,',/, &
       ' <C> to run all C-14 models, <E> to explain insufficient data.')
9075 FORMAT ( &
       ' Hit <S> to show Rayleigh data, <C> to run all C-14 models,' &
       ,/,' <E> to explain insufficient data')
END SUBROUTINE BALN
!
!
!
SUBROUTINE INIT
  USE max_size
  IMPLICIT NONE
  CHARACTER Pname*8, Ename*2, Force*1
  COMMON /CHAR3 / Pname(39), Ename(39), Force(39)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION Clmain, Cleach, Pdat, Res, Sfinal, Sinit, Maxdel,  &
       Mindel, Dfinal
  COMMON /DP3   / Clmain(202,40), Cleach(202,40), Pdat(40,40),  &
       Res(100), Sfinal(39), Sinit(5,59), Maxdel(39),  &
       Mindel(39), Dfinal
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Pcoef
  COMMON /DP6   / Pcoef(39,39)
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER i, k, iline, imult, itime, j
  INTRINSIC DABS
  !
  Numtest = 0
  Ierror = 0
  iline = 0
  IF (Noele.EQ.0 .OR. Nmins.EQ.0) THEN
     Ierror = 1
     RETURN
  END IF
  DO i = 1, Noele
     IF (Imix.EQ.0 .AND. Iflag(6).EQ.0) THEN
        Maxdel(i) = (Sfinal(i)-Sinit(1,i))+P(3) &
             *(DABS(Sfinal(i))+DABS(Sinit(1,i)))
        Mindel(i) = (Sfinal(i)-Sinit(1,i))-P(3) &
             *(DABS(Sfinal(i))+DABS(Sinit(1,i)))
     ELSE
        Maxdel(i) = Sfinal(i)+P(3)*DABS(Sfinal(i))
        Mindel(i) = Sfinal(i)-P(3)*DABS(Sfinal(i))
     END IF
  enddo
  !
  DO i = 1, Nmins
     IF ((Imix.EQ.0.AND.Iflag(6).EQ.0) .OR. (i.GT.1+Imix+Iflag(6))) &
          THEN
        DO j = 1, Noele
           Pdat(i,j) = 0.0D0
           k = 0
20         k = k+1
           IF (Pelt(i,k).EQ.Ename(j)) Pdat(i,j) = Pcoef(i,k)
           IF (Pelt(i,k).NE.' ') GO TO 20
        enddo
     END IF
  enddo
50 iline = iline+1
  j = 0
60 j = j+1
  IF (j.LE.Imix+1 .AND. Imix.GT.0) THEN
     Clmain(iline,j) = Sinit(j,iline)
  ELSE IF (j.EQ.1 .AND. Iflag(6).EQ.1) THEN
     Clmain(iline,j) = Sinit(1,iline)
  ELSE IF (Iflag(6).EQ.1 .AND. j.EQ.Imix+2) THEN
     Clmain(iline,j) = 0.0D0
  ELSE
     Clmain(iline,j) = Pdat(j,iline)
  END IF
  IF (j.LT.Nmins) GO TO 60
  IF (Imix.GT.0 .OR. Iflag(6).EQ.1) THEN
     Clmain(iline,j+1) = Sfinal(iline)
  ELSE
     Clmain(iline,j+1) = Sfinal(iline)-Sinit(1,iline)
  END IF
  IF (Nmins.EQ.0) THEN
     Ierror = 1
     RETURN
  END IF
  IF (iline.LT.Noele) GO TO 50
  !
  !  Now write the mixing equation, if there is one
  !
  IF (Imix.GT.0 .OR. Iflag(6).EQ.1) THEN
     iline = iline+1
     DO j = 1, Nmins
        IF (j.LE.1+Imix+Iflag(6)) THEN
           Clmain(iline,j) = 1.0D0
        ELSE
           Clmain(iline,j) = 0.0D0
        END IF
     enddo
     Clmain(iline,Nmins+1) = 1.0D0
  END IF
  !
  !  Now write the inequalities, 2 (or 4 if mixing) per constraint
  !
  IF (Imix.EQ.0 .AND. Iflag(6).EQ.0) THEN
     DO i = 1, Noele
        DO imult = 1, -1, -2
           iline = iline+1
           DO j = 1, Nmins
              Clmain(iline,j) = Clmain(i,j)*imult
           enddo
        enddo
        Clmain(iline-1,Nmins+1) = Maxdel(i)
        Clmain(iline,Nmins+1) = -Mindel(i)
     enddo
  ELSE
     DO i = 1, Noele
        DO itime = 1, Imix+1
           iline = iline+1
           DO j = 1, Nmins
              IF (j.GT.1+Imix) THEN
                 Clmain(iline,j) = Clmain(i,j)
              ELSE IF (j.EQ.itime) THEN
                 Clmain(iline,j) = Clmain(i,j)*(1D0-P(3))
              ELSE
                 Clmain(iline,j) = Clmain(i,j)*(1D0+P(3))
              END IF
           enddo
           Clmain(iline,Nmins+1) = Maxdel(i)
           iline = iline+1
           DO j = 1, Nmins
              IF (j.GT.1+Imix) THEN
                 Clmain(iline,j) = -Clmain(i,j)
              ELSE IF (j.EQ.itime) THEN
                 Clmain(iline,j) = -Clmain(i,j)*(1.0D0+P(3))
              ELSE
                 Clmain(iline,j) = -Clmain(i,j)*(1.0D0-P(3))
              END IF
           enddo
           Clmain(iline,Nmins+1) = -Mindel(i)
        enddo
     enddo
  END IF
  Cllines = iline
  RETURN
END SUBROUTINE INIT
!
!
!
SUBROUTINE NEXT
  IMPLICIT NONE
  !
  CHARACTER Pname*8, Ename*2, Force*1
  COMMON /CHAR3 / Pname(39), Ename(39), Force(39)
  DOUBLE PRECISION Clmain, Cleach, Pdat, Res, Sfinal, Sinit, Maxdel,  &
       Mindel, Dfinal
  COMMON /DP3   / Clmain(202,40), Cleach(202,40), Pdat(40,40),  &
       Res(100), Sfinal(39), Sinit(5,59), Maxdel(39),  &
       Mindel(39), Dfinal
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  LOGICAL First, Quit
  COMMON /LOG3  / First, Quit
  LOGICAL skip
  INTEGER i, j, k
  INTRINSIC DABS
  !
10 CONTINUE
  Quit = .TRUE.
  IF (First) THEN
     First = .FALSE.
     DO i = 1, Nmins-Numdrop
        Quit = .FALSE.
        Minpos(i) = i
        Maxpos(i) = Numdrop+i
        IF (Force(i).EQ.'F') Maxpos(i) = Minpos(i)
        Now(i) = i
     enddo
     IF (Numdrop.GT.0) THEN
        IF (Force(Nmins-Numdrop+1).EQ.'F') Quit = .TRUE.
     END IF
  ELSE
     i = Nmins-Numdrop
30   IF (i.NE.0) THEN
        IF (Now(i).LT.Maxpos(i)) THEN
           Quit = .FALSE.
           Now(i) = Now(i)+1
           IF (i.LT.Nmins-Numdrop) THEN
              k = Now(i)
              DO j = i+1, Nmins-Numdrop
                 k = k+1
                 Now(j) = k
              enddo
           END IF
        ELSE
           i = i-1
           GO TO 30
        END IF
     END IF
     IF (Quit) RETURN
  END IF
  Numtest = Numtest+1
  DO i = 1, Cllines
     skip = .TRUE.
     DO j = 1, Nmins-Numdrop
        Cleach(i,j) = Clmain(i,Now(j))
        IF (DABS(Cleach(i,j)).GT.1.0D-5) skip = .FALSE.
     enddo
     if (i .le. Noele) THEN
        IF (skip .AND. Maxdel(i)*Mindel(i).GT.1.0D-16) GO TO 10
     END IF
     Cleach(i,Nmins-Numdrop+1) = Clmain(i,Nmins+1)
  enddo
  RETURN
END SUBROUTINE NEXT
!
!
!
SUBROUTINE RUNONE(MAXIGNORE)
  USE max_size
  IMPLICIT NONE
  !
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION Clmain, Cleach, Pdat, Res, Sfinal, Sinit, Maxdel,  &
       Mindel, Dfinal
  COMMON /DP3   / Clmain(202,40), Cleach(202,40), Pdat(40,40),  &
       Res(100), Sfinal(39), Sinit(5,59), Maxdel(39),  &
       Mindel(39), Dfinal
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  DOUBLE PRECISION cu(2,121), error, toler
  INTEGER i, is(100), isofar, iter, iu(2,121), k, klm2d, klmd, kode,  &
       l, m, MAXIGNORE, n, n2d, nklmd
  EXTERNAL CL1
  !
  k = Noele
  l = 0
  IF (Imix.GT.0 .OR. Iflag(6).EQ.1) l = 1
  m = 2*(Imix+1)*Noele
  n = Nmins-Numdrop
  klmd = 100
  klm2d = 202
  nklmd = 121
  n2d = 40
  kode = 1
  toler = 1.0D-5
  iter = 500
  DO i = 1, 40
     Delta(i) = 0.0D0
     Res(i) = 0.0D0
  enddo
  CALL CL1(k,l,m,n,klmd,klm2d,nklmd,n2d,kode,toler,iter,Delta,error, &
       cu,iu,is)
  Igotone = 0
  IF (kode.NE.0) GOTO 30
  !
  isofar = 0
  DO i = 1, n
     IF (i.LE.Imix+1 .AND. Imix.GT.0) THEN
        IF (Delta(i).LT.0.0D0) GOTO 30
     ELSE IF (i.EQ.1 .AND. Iflag(6).EQ.1) THEN
        IF (Delta(i).LT.0.0D0) GOTO 30
     ELSE IF (i.NE.Imix+2 .OR. Iflag(6).NE.1) THEN
        !        no check
        IF (Transfer(Now(i)).EQ.'+' .AND. Delta(i).LT.0.0D0) &
             isofar = isofar+1
        IF (Transfer(Now(i)).EQ.'-' .AND. Delta(i).GT.0.0D0) &
             isofar = isofar+1
     END IF
  enddo
  IF (isofar.LE.MAXIGNORE) THEN
     Igotone = 1
     MAXIGNORE = isofar
  END IF
30 RETURN
END SUBROUTINE RUNONE
!
!
!
SUBROUTINE PRINT
  USE max_size
  use filenames
  IMPLICIT NONE
  !
  ! Results of the mass transfer are displayed.
  !

  CHARACTER Pname*8, Ename*2, Force*1
  COMMON /CHAR3 / Pname(39), Ename(39), Force(39)
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  INTEGER i, k, ll, l
  CHARACTER*1 ans
  EXTERNAL CISO
  !
  WRITE (Iunit,9000) Inum
  Evap = 0.0D0
  IF (Iflag(6).EQ.1) Evap = Delta(Imix+2)
  DO i = 1, Nmins-Numdrop
     IF (Iflag(6).NE.1 .OR. Imix.LE.0 .OR. i.NE.Imix+2) THEN
        IF (Iflag(6).NE.1 .OR. Imix.NE.0 .OR. i.GT.2) THEN
           Delta(i) = Delta(i)/(1.0D0-Evap)
           k = Now(i)
           ll = 0
           DO l = 1, Nopha
              IF (Phase(l).EQ.Pname(k)) ll = l
           enddo
           IF (ll.eq.0) THEN
              WRITE (Iunit,9005) Pname(k), Transfer(k), Force(k),  &
                   Delta(i)
           ELSE IF (It(ll) .EQ. ' ' .OR.  &
                (It(ll) .EQ. '+' .AND. Delta(i) .GE. -1d-10) .OR.  &
                (It(ll) .EQ. '-' .AND. Delta(i) .LE. 1d-10)) THEN
              WRITE (Iunit,9005) Pname(k), Transfer(k), Force(k),  &
                   Delta(i)
           ELSE IF (It(ll) .EQ. '*') THEN
              WRITE (Iunit,9010) Pname(k), Transfer(k), Force(k),  &
                   Delta(i), Para(ll,1)
           ELSE
              WRITE (Iunit,9015) Pname(k), Transfer(k), Force(k), &
                   Delta(i)
           END IF
        END IF
     END IF
  enddo
  IF (Iflag(6).EQ.1) THEN
     IF (Evap.GT.0.0D0) THEN
        WRITE (Iunit,9020) 1.0D0/(1.0D0-Evap), 1.0D0/(1.0D0-Evap)
     ELSE
        WRITE (Iunit,9025) (1.0D0-Evap), 1000.0D0/(1.0D0-Evap)
     END IF
  END IF
  WRITE (*,*)
  IF (Iflag(3).EQ.1) THEN
     IF (Nrun.NE.0 .AND. (Nmins-Numdrop)-Iflag(6).GT.8) THEN
        WRITE (*,9030) 'Hit <Enter> to continue'
        READ (*,9035) ans
     END IF
     CALL CISO(0)
  END IF
  RETURN
9000 FORMAT (//,12X,'MODEL ',I3)
9005 FORMAT (2X,A8,2X,A1,1X,A1,2X,F13.5)
9010 FORMAT (2X,A8,2X,A1,1X,A1,2X,F13.5,3X,F10.3,' exchanged')
9015 FORMAT (2X,A8,2X,A1,1X,A1,2X,F13.5,6x,'(Constraint ignored)')
9020 FORMAT (2X,'Dilution factor: ',F10.3,1X,F10.3,'kg H2O remain')
9025 FORMAT (2X,'Evaporation factor: ',F10.3,1X,F10.3,'g H2O remain')
9030 FORMAT (1X,A)
9035 FORMAT (A)
END SUBROUTINE PRINT
!
!
!
SUBROUTINE CISO(ISCR)
  USE max_size
  use filenames
  IMPLICIT NONE
  !
  !  The isotopic calculations, according to the Rayleigh equations, are
  !  carried out.  If there is any reason why they cannot be carried out,
  !  a warning message is printed.  Depending on ISCR, different values
  !  are ccomputed and different data are displayed.  To help locate
  !  problems, values of variables in the Rayleigh equation are stored in
  !  NETPATH.OUT
  !

  CHARACTER Pname*8, Ename*2, Force*1
  COMMON /CHAR3 / Pname(39), Ename(39), Force(39)
  CHARACTER Wllnms*80, Transfer*1, Model*20, Yes*3, Ion*10, Ffact*14
  COMMON /CHAR4 / Wllnms(0:MAXWELLS), Transfer(39), Model(9), Yes(0:1),  &
       Ion(4), Ffact(0:1)
  CHARACTER Elelong*12, Pelt*2
  COMMON /CHAR6 / Elelong(0:28), Pelt(39,39)
  DOUBLE PRECISION Evap, Pcoeff
  COMMON /DP1   / Evap, Pcoeff(39,36)
  DOUBLE PRECISION Predat, Disdat, Cfinal, Cinit, Dinit, Result
  COMMON /DP2   / Predat(39,2), Disdat(39,2), Cfinal, Cinit, Dinit, &
       Result
  DOUBLE PRECISION Clmain, Cleach, Pdat, Res, Sfinal, Sinit, Maxdel,  &
       Mindel, Dfinal
  COMMON /DP3   / Clmain(202,40), Cleach(202,40), Pdat(40,40),  &
       Res(100), Sfinal(39), Sinit(5,59), Maxdel(39),  &
       Mindel(39), Dfinal
  DOUBLE PRECISION C14dat, Dbdata, P, Delta, Disalong, Usera
  COMMON /DP4   / C14dat(13), Dbdata(0:MAXWELLS,0:50), P(3), Delta(40),  &
       Disalong, Usera(5)
  DOUBLE PRECISION Para
  COMMON /DP5   / Para(39,16)
  INTEGER Wunit, Nwlls, Icase, Jele, Nodata, Isdocrs
  COMMON /INT1  / Wunit, Nwlls, Icase, Jele(39,36), Nodata(MAXWELLS,50),  &
       Isdocrs(0:5)
  INTEGER Idis, Ipre, Rwunit
  COMMON /INT2  / Idis, Ipre, Rwunit
  INTEGER Numdrop, Cllines, Maxpos, Minpos, Now, Imix, Igotone,  &
       Iunit, Nmins, Ierror, Numtest
  COMMON /INT3  / Numdrop, Cllines, Maxpos(39), Minpos(39), Now(39),  &
       Imix, Igotone, Iunit, Nmins, Ierror, Numtest
  INTEGER Well, Tunit, Iflag, Inum, Nrun
  COMMON /INT4  / Well(0:5), Tunit, Iflag(6), Inum, Nrun
  INTEGER Iele, Noele, Ilength
  COMMON /INT6  / Iele(36), Noele, Ilength
  INTEGER Flin, Runit, Tot, Nopha, Iedit, Iadd
  COMMON /INT7  / Flin, Runit, Tot(MAXWELLS), Nopha, Iedit, Iadd
  LOGICAL Dowehave
  COMMON /LOG5  / Dowehave(39,2:16)
  DOUBLE PRECISION cic, delpre, fraction, age, cdiff, dmin, dmax,  &
       a0, sumiso(20), dout(20,100), C14
  INTEGER iage, i, ierr, idelt, ipha, j, jj, jhave, idiff, k,  &
       iglines, kk, iput, iput2, imod, ISTATE, ISCR, iconst,  &
       ierrn, iw, elmt(5), isot(5), itoterr, LENS
  CHARACTER*8 dispha(39), prepha(39)
  CHARACTER*80 saveout(20), glines(0:10), line
  double precision fgk
  COMMON /FontGarnier/ fgk
  EXTERNAL CLS, CFRACT, SFRACT, RAYLEIGH, C14, ISTATE, LENS
  INTRINSIC DLOG, DABS
  DATA elmt/1, 1, 2, 15, 18/, isot/21, 22, 23, 24, 25/
  ! Only print headers and make netpath.out file on first time through.

  IF (ISCR.LT.1) THEN
     OPEN (Rwunit,FILE='netpath.out')
     WRITE (Iunit,9010)
     iage = 0
  END IF
  DO i = 1, 5
     itoterr = 0
     ! We only want to do this for C-14 if iscr=2
     IF (i.NE.2 .AND. ISCR.EQ.2) cycle
     ! Keep track of which constraint we are on.
     DO j = 1, Noele
        IF (Iele(j).EQ.elmt(i)) iconst = j
     enddo
     IF (ISCR.EQ.1) THEN
        IF (Iunit.EQ.Tunit) CALL CLS
        WRITE (Iunit,9020) Elelong(isot(i))
     ELSE IF (ISCR.EQ.3) THEN
        IF (Iunit.EQ.Tunit) CALL CLS
        WRITE (Iunit,9015) Elelong(isot(i))
        DO iw = 1, Imix+1
           IF (Nodata(Well(iw),isot(i)).EQ.1) THEN
              WRITE (Iunit,9025) Wllnms(Well(iw)) &
                   (5:LENS(Wllnms(Well(iw))))
              ierr = 1
           END IF
        enddo
        IF (Nodata(Well(0),elmt(i)).EQ.1) THEN
           WRITE (Iunit,9030) Wllnms(Well(0))(5:LENS(Wllnms(Well(0))))
           ierr = 1
        END IF
     END IF
     ierr = 0
     IF (ierr.GT.0) THEN
        IF (ISCR.NE.3) GO TO 190
        GO TO 40
     END IF
     IF (Imix.EQ.0) THEN
        !
        !   No mixing case
        !
        Cinit = Dbdata(Well(1),elmt(i))
        IF (Cinit.LE.0.0D0) THEN
           ierr = ierr+1
           IF (ISCR.NE.3) GO TO 190
           WRITE (Iunit,9035) Elelong(elmt(i))
           GO TO 40
        END IF
        IF (i.EQ.2) THEN
           Dinit = C14(Iflag(4),1)*Dbdata(Well(1),elmt(i))
           IF (Dinit.LE.0.0D0) THEN
              ierr = ierr+1
              IF (ISCR.NE.3) GO TO 190
              WRITE (Iunit,9040)
              GO TO 40
           END IF
        ELSE IF (i.EQ.1) THEN
           Dinit = Dbdata(Well(1),isot(i))
           !             Add on CH4 and DOC data
           Dinit = Dinit+Dbdata(Well(1),isot(i)*2+2)*Dbdata(Well(1),42)
           Dinit = Dinit+Dbdata(Well(1),isot(i)*2+3)*Dbdata(Well(1),43)
        ELSE
           Dinit = Dbdata(Well(1),isot(i))
        END IF
     ELSE
        !
        !   Mixing case
        !
        Cinit = 0.0D0
        Dinit = 0.0D0
        DO iw = 1, Imix+1
           Cinit = Cinit+Delta(iw)*Dbdata(Well(iw),elmt(i))
           IF (i.EQ.2) THEN
              Dinit = Dinit+Delta(iw)*C14(Iflag(4),iw) &
                   *Dbdata(Well(iw),elmt(i))
           ELSE
              Dinit = Dinit+Delta(iw)*Dbdata(Well(iw),isot(i))
              IF (i.EQ.1) THEN
                 Dinit = Dinit+Dbdata(Well(iw),isot(i)*2+2) &
                      *Dbdata(Well(iw),42)*Delta(iw)
                 Dinit = Dinit+Dbdata(Well(iw),isot(i)*2+3) &
                      *Dbdata(Well(iw),43)*Delta(iw)
              END IF
           END IF
        enddo
        IF (i.EQ.2 .AND. Dinit.LE.0.0D0) THEN
           ierr = ierr+1
           IF (ISCR.NE.3) GO TO 190
           WRITE (Iunit,9040)
           GO TO 40
        END IF
        IF (Cinit.LE.1.0D-10) THEN
           ierr = ierr+1
           IF (ISCR.NE.3) GO TO 190
           WRITE (Iunit,9035) Elelong(elmt(i))
           GO TO 40
        END IF
     END IF
     IF (i.NE.2) THEN
        Dinit = Dinit/Cinit
     ELSE
        Dinit = 10.0D0*Dinit/Cinit-1000.0D0
     END IF
     Cinit = Cinit*(1.0D0-Evap)
     IF (Nodata(Well(0),isot(i)).EQ.0 .AND. Dbdata(Well(0),elmt(i)) &
          .GT.0.0D0) THEN
        Dfinal = Dbdata(Well(0),isot(i))/Dbdata(Well(0),elmt(i))
        IF (elmt(i).EQ.1) Dfinal = (Dbdata(Well(0),isot(i))+Dbdata( &
             Well(0),42) &
             *Dbdata(Well(0),isot(i)*2+2) &
             +Dbdata(Well(0),43) &
             *Dbdata(Well(0),isot(i)*2+3)) &
             /Dbdata(Well(0),1)
     ELSE
        Dfinal = 0.0D0
     END IF
     Cfinal = Dbdata(Well(0),elmt(i))
     IF (P(3).GT.0) THEN
        Cinit = Cinit*(Clmain(iconst,Nmins+1)-Res(iconst)) &
             /(Cinit+Cfinal)
        Cfinal = Cfinal*(Clmain(iconst,Nmins+1)+Res(iconst) &
             /(Cinit+Cfinal))
     END IF
40   itoterr = itoterr+ierr
     Ipre = 0
     Idis = 0
     idelt = 0
50   idelt = idelt+1
     IF (idelt.GT.Nmins) THEN
        IF (ISCR.EQ.3) THEN
           IF (itoterr.GT.0) THEN
              WRITE (*,9000)
              READ (*,9005) line
           END IF
           cycle
        END IF
        IF (ISCR.EQ.2) THEN
           iage = 0
           IF (Nodata(Well(0),isot(i)).NE.1) THEN
              WRITE (Iunit,9050)
              IF (Imix .GT. 0) THEN
                 Cinit=0
                 DO iw = 1, Imix + 1
                    Cinit = Cinit+Delta(iw)*Dbdata(Well(iw),elmt(i))
                 enddo
              END IF
              !
              !   Write A0 models
              !
              DO imod = 1, 9
                 IF (Imix .EQ. 0) THEN
                    Dinit = C14(imod,1)
                 ELSE
                    Dinit = 0.0D0
                    DO iw = 1, Imix+1
                       IF (C14(imod,iw).LT.0.0D0) cycle
                       Dinit = Dinit+Delta(iw)*C14(imod,iw) &
                            *Dbdata(Well(iw),elmt(i)) 
                    enddo
                    Dinit=Dinit/Cinit
                 END IF
                 IF (Dinit.GT.0.0D0) THEN
                    a0 = Dinit
                    Dinit = Dinit*10.0D0-1000.0D0
                    CALL RAYLEIGH(0)
                    Result = (1000.0D0+Result)/10.0D0
                    IF (Dfinal*Result.GT.0.00001D0) THEN
                       age = 5730.0D0/DLOG(2.0D0)*DLOG(Result/Dfinal)
                       WRITE (Iunit,9055) Model(imod), a0, Result, Dfinal,  &
                            age
                       if (imod == 7) then
                          WRITE (Iunit,'(T7, "F-G K", F10.2)') fgk
                       endif
                    END IF
                 END IF
              enddo
           END IF
        ELSE
           IF (ISCR.EQ.1) THEN
              CALL RAYLEIGH(0)
              IF (i.EQ.2) THEN
                 WRITE (Iunit,9045) Dinit/10.0D0+100.0D0,  &
                      Result/10.0D0+100.0D0
              ELSE
                 WRITE (Iunit,9045) Dinit, Result
              END IF
              IF (Ipre+Idis.EQ.0) THEN
                 WRITE (Iunit,'(A,/)') ' No incoming or outgoing phases'
                 GO TO 130
              END IF
              IF (Idis.GT.0) THEN
                 IF (i.EQ.2) THEN
                    WRITE (Iunit,9060) Idis, Eleshort(elmt(i)),  &
                         '(% modern)'
                 ELSE
                    WRITE (Iunit,9060) Idis, Eleshort(elmt(i)), '(o/oo)'
                 END IF
              END IF
              idiff = 50
              DO j = 1, Idis
                 IF (i.EQ.2) THEN
                    WRITE (Iunit,9065) dispha(j), Disdat(j,1),  &
                         (Disdat(j,2)+1000.0D0)/10.0D0
                 ELSE
                    WRITE (Iunit,9065) dispha(j), Disdat(j,1), Disdat(j,2)
                 END IF
                 Disdat(j,1) = Disdat(j,1)/idiff
              enddo
              IF (Ipre.GT.0) THEN
                 IF (i.EQ.2) THEN
                    WRITE (Iunit,9070) Ipre, Eleshort(elmt(i)),  &
                         '(% modern)'
                 ELSE
                    WRITE (Iunit,9070) Ipre, Eleshort(elmt(i)), '(o/oo)'
                 END IF
              END IF
              DO j = 1, Ipre
                 WRITE (saveout(j),9170) prepha(j), 0.0D0-Predat(j,1),  &
                      Predat(j,2)
                 Predat(j,1) = Predat(j,1)/idiff
                 sumiso(j) = 0.0D0
              enddo
              IF (Cfinal.NE.Cinit .AND. Ipre.NE.0) THEN
                 cdiff = (Cfinal-Cinit)/idiff
                 Cfinal = Cinit+cdiff
                 DO k = 1, idiff
                    CALL RAYLEIGH(0)
                    DO j = 1, Ipre
                       dout(j,k) = (1000.0D0+((Dinit+Result)/2.0D0)) &
                            *(Predat(j,2)/1000.0D0+1.0D0)-1000.0D0
                       sumiso(j) = sumiso(j)+dout(j,k)/idiff
                    enddo
                    Dinit = Result
                    Cinit = Cfinal
                    Cfinal = Cinit+cdiff
                 enddo
                 DO j = 1, Ipre
                    IF (i.EQ.2) sumiso(j) = (sumiso(j)+1000.0D0)/10.0D0
                    WRITE (Iunit,9075) saveout(j)(1:51), sumiso(j)
                 enddo
              END IF
130           IF (Iunit.NE.Tunit) GO TO 200
              IF (Ipre.EQ.0) THEN
                 WRITE (*,9000)
              ELSE
                 WRITE (*,9080)
              END IF
              READ (*,9005) line
              IF (Ipre.EQ.0 .OR. line.EQ.' ') cycle
              GO TO 200
           END IF
           WRITE (Rwunit,9085) Elelong(isot(i))
           WRITE (Rwunit,9090) Idis
           DO jj = 1, Idis
              WRITE (Rwunit,9100) Disdat(jj,1), Disdat(jj,2)
           enddo
           WRITE (Rwunit,9095) Ipre
           DO jj = 1, Ipre
              WRITE (Rwunit,9100) Predat(jj,1), Predat(jj,2)
           enddo
           CALL RAYLEIGH(1)
           IF (i.EQ.2) THEN
              Result = (1000.0D0+Result)/10.0D0
              IF (Dfinal.GT.1.0D-5 .AND. Result.GT.1.0D-5) THEN
                 age = 5730.0D0/DLOG(2.D0)*DLOG(Result/Dfinal)
                 iage = 1
                 WRITE (Iunit,9115) Elelong(isot(i)), Result, Dfinal
              ELSE IF (Nodata(Well(0),isot(i)).EQ.0) THEN
                 WRITE (Iunit,9105) Elelong(isot(i)), Result, Dfinal
              ELSE
                 WRITE (Iunit,9110) Elelong(isot(i)), Result
              END IF
           ELSE IF (i.EQ.4) THEN
              IF (Nodata(Well(0),isot(i)).EQ.0) THEN
                 WRITE (Iunit,9120) Elelong(isot(i)), Result, Dfinal
              ELSE
                 WRITE (Iunit,9125) Elelong(isot(i)), Result
              END IF
           ELSE
              IF (Nodata(Well(0),isot(i)).EQ.0) THEN
                 WRITE (Iunit,9105) Elelong(isot(i)), Result, Dfinal
              ELSE
                 WRITE (Iunit,9110) Elelong(isot(i)), Result
              END IF
              jhave = 0
              DO iw = 0, Imix+1
                 IF (DABS(Dbdata(Well(iw),42)).GT.1.D-20 .OR.  &
                      DABS(Dbdata(Well(iw),43)).GT.1.D-20) jhave = 1
              enddo
              IF (i.EQ.1 .AND. DABS(Dbdata(Well(0),41)).GT.1.D-20 .AND.  &
                   jhave.EQ.1) WRITE (Iunit,9105) 'DIC C-13    ',  &
                   (Result*Cfinal-Dbdata(Well(0),42) &
                   *Dbdata(Well(0),44) &
                   -Dbdata(Well(0),43) &
                   *Dbdata(Well(0),45)) &
                   /Dbdata(Well(0),41),  &
                   Dbdata(Well(0),21) &
                   /Dbdata(Well(0),41)
           END IF
        END IF
        cycle
     ELSE
        IF (Now(idelt).EQ.0) GO TO 50
        ipha = 0
170     ipha = ipha+1
        IF (ipha.GT.Nopha) GO TO 50
        IF (Pname(Now(idelt)).NE.Phase(ipha)) GO TO 170
        cic = 0
        j = 0
180     j = j+1
        IF (Jele(ipha,j).EQ.elmt(i)) cic = Pcoeff(ipha,j)
        IF (Jele(ipha,j).NE.0 .AND. j.LT.14) GO TO 180
        IF (cic.EQ.0) GO TO 50
        IF (Delta(idelt).GT.0.0D0 .OR. It(ipha).EQ.'*') THEN
           Idis = Idis+1
           dispha(Idis) = Phase(ipha)
           Disdat(Idis,1) = Para(ipha,1)*cic
           Disdat(Idis,2) = Para(ipha,i+1)
           IF (Delta(idelt).GT.0.0D0) THEN
              Disdat(Idis,1) = Delta(idelt)*cic
              IF (It(ipha).EQ.'*' .AND. .NOT.Dowehave(ipha,i+11)) &
                   Disdat(Idis,1) = (Para(ipha,1)+Delta(idelt))*cic
           ELSE IF (It(ipha).EQ.'*' .AND. Dowehave(ipha,i+11)) THEN
              Disdat(Idis,2) = Para(ipha,i+11)
           END IF
           IF (.NOT.Dowehave(ipha,i+1)) THEN
              ierr = 1
              IF (ISCR.EQ.3) WRITE (Iunit,9130) Elelong(isot(i)),  &
                   Phase(ipha)
           END IF
           IF (Phase(ipha).EQ.'CO2-CH4 ') THEN
              Disdat(Idis,2) = Para(ipha,i+1)*P(1)+(1.0D0-P(1)) &
                   *Para(ipha,i+3)
              IF (Dowehave(ipha,i+3)) THEN
                 IF (P(1).EQ.0.0D0) ierr = 0
              ELSE IF (P(1).LT.1.0D0) THEN
                 ierr = 1
                 IF (ISCR.EQ.3) WRITE (Iunit,9135) Elelong(isot(i))
              END IF
           END IF
           IF (i.EQ.2) Disdat(Idis,2) = Disdat(Idis,2)*10.0D0-1000.0D0
           IF (Delta(idelt).GT.0.0D0 .AND. It(ipha).EQ.'*' .AND.  &
                Dowehave(ipha,i+11)) THEN
              Idis = Idis+1
              dispha(Idis) = Phase(ipha)
              Disdat(Idis,1) = Para(ipha,1)*cic
              Disdat(Idis,2) = Para(ipha,i+11)
              IF (i.EQ.2) Disdat(Idis,2) = Disdat(Idis,2)*10.0D0-1000.D0
           END IF
        END IF
        IF (Delta(idelt).LT.0.0D0 .OR. It(ipha).EQ.'*') THEN
           Ipre = Ipre+1
           prepha(Ipre) = Pname(Now(idelt))
           Predat(Ipre,1) = 0.0D0
           IF (Delta(idelt).LT.0.D0) Predat(Ipre,1) = -Delta(idelt)*cic
           IF (It(ipha).EQ.'*') Predat(Ipre,1) = Predat(Ipre,1) &
                +Para(ipha,1)*cic
           Predat(Ipre,2) = Para(ipha,i+6)
           IF (.NOT.Dowehave(ipha,i+6)) THEN
              ierr = 1
              IF ((ISCR.EQ.3.AND.i.GT.2) .OR.  &
                   (ISCR.EQ.3.AND.Phase(ipha)(1:3).NE.'CO2'.AND. &
                   ISTATE(ipha,1).NE.4)) WRITE (Iunit,9130) &
                   Elelong(isot(i)), Phase(ipha)
           END IF
           IF (Phase(ipha).EQ.'CO2-CH4 ') THEN
              IF (.NOT.Dowehave(ipha,8+i) .AND. P(1).LT.1.0D0) THEN
                 ierr = 1
                 IF (ISCR.EQ.3) WRITE (Iunit,9135) Elelong(isot(i))
              ELSE
                 ierr = 0
                 delpre = Predat(Ipre,1)
                 ! Outgassing CO2 in CO2-CH4
                 prepha(Ipre) = 'CO2 GAS'
                 Predat(Ipre,1) = delpre*P(1)
                 IF (Dowehave(ipha,6+i)) THEN
                    Predat(Ipre,2) = Para(ipha,6+i)
                 ELSE
                    CALL CFRACT(fraction,2,0,ierrn)
                    Predat(Ipre,2) = i*fraction
                 END IF
                 !  Outgassing CH4 of CO2-CH4
                 Ipre = Ipre+1
                 prepha(Ipre) = 'CH4 GAS'
                 Predat(Ipre,1) = delpre*(1.0D0-P(1))
                 Predat(Ipre,2) = Para(ipha,8+i)
              END IF
           ELSE IF (Phase(ipha)(1:3).EQ.'CO2' .AND. i.LT.3) THEN
              ierr = 0
              CALL CFRACT(fraction,2,0,ierrn)
              IF (.NOT.Dowehave(ipha,i+6)) Predat(Ipre,2) = i*fraction
           ELSE IF (i.LT.3 .AND. ISTATE(ipha,1).EQ.4 .AND.  &
                .NOT.Dowehave(ipha,i+6)) THEN
              ierr = 0
              CALL CFRACT(fraction,1,0,ierrn)
              Predat(Ipre,2) = i*fraction
           ELSE IF (i.EQ.3 .AND.  &
                (ISTATE(ipha,2).EQ.-1.OR.ISTATE(ipha,2).EQ.-2) &
                .AND. Nodata(Well(0),23).EQ.0 .AND.  &
                Dbdata(Well(0),2).GT.0.0D0 .AND. Dbdata(Well(0),29) &
                .GT.0.0D0) THEN
              CALL SFRACT(fraction,1,0,ierrn)
              IF (ierrn.EQ.1) fraction = 0.0D0
              Predat(Ipre,2) = Predat(Ipre,2)+fraction
           ELSE IF (i.EQ.3 .AND. ISTATE(ipha,2).EQ.6 .AND.  &
                Nodata(Well(0),23).EQ.0 .AND. Dbdata(Well(0),2) &
                .GT.0.0D0 .AND. Dbdata(Well(3),29).GT.0.0D0) THEN
              CALL SFRACT(fraction,2,0,ierrn)
              IF (ierrn.EQ.1) fraction = 0.0D0
              Predat(Ipre,2) = Predat(Ipre,2)+fraction
           END IF
        END IF
        itoterr = itoterr+ierr
        IF (ierr.NE.1 .OR. ISCR.EQ.3) GO TO 50
     END IF
190  IF (ISCR.EQ.0) WRITE (Iunit,9140) Elelong(isot(i))
     IF (ISCR.EQ.1 .AND. Iunit.NE.Tunit) WRITE (Iunit,9145)
     IF (ISCR.EQ.1 .AND. Iunit.EQ.Tunit) CALL CLS
     Ipre = 0
     Idis = 0
     cycle
200  iglines = 10
     DO j = 1, Ipre
        IF (Iunit.EQ.Tunit) CALL CLS
        WRITE (Iunit,9150) prepha(j)
        dmin = dout(j,1)
        dmax = dout(j,idiff)
        IF (dout(j,1).GT.dmax) THEN
           dmax = dout(j,1)
           dmin = dout(j,idiff)
        END IF
        IF (i.EQ.2) THEN
           dmin = (dmin+1000.0D0)/10.0D0
           dmax = (dmax+1000.0D0)/10.0D0
        END IF
        if (dmin .gt. dmax - 1e-6) then
           dmax = dmin + 1
        endif
        DO kk = 0, iglines
           glines(kk) = ' '
        enddo
        DO kk = 1, idiff
           IF (i.EQ.2) dout(j,kk) = (dout(j,kk)+1000.0D0)/10.0D0
           iput = iglines*(dout(j,kk)-dmin)/(dmax-dmin)
           IF (iput.EQ.iglines) iput = iput-1
           glines(iput)(kk:kk) = '*'
        enddo
        WRITE (Iunit,9155)
        iput2 = iglines*(sumiso(j)-dmin)/(dmax-dmin)
        IF (iput2.GE.iglines-1) THEN
           WRITE (Iunit,'(4x,f9.4,A,A,A,F9.4)') dmax, '|',  &
                glines(iglines-1)(1:50), '|Avg =', sumiso(j)
        ELSE
           WRITE (Iunit,'(4x,f9.4,A,A,A)') dmax, '|', glines(iglines-1) &
                (1:50), '|'
        END IF
        DO kk = iglines-2, 1, -1
           line = ' '
           IF (kk.EQ.iglines/2) line(1:12) = Elelong(isot(i))
           IF (kk.EQ.iglines/2-1) line(5:8) = 'o/oo'
           IF (kk.EQ.iglines/2-1 .AND. i.EQ.2) line(1:12) = ' '
           IF (iput2.EQ.kk) THEN
              WRITE (line(13:),9160) '|', glines(kk)(1:50), '|Avg =',  &
                   sumiso(j)
           ELSE
              WRITE (line(13:),9160) '|', glines(kk)(1:50), '|'
           END IF
           WRITE (Iunit,9165) line(1:78)
        enddo
        IF (iput2.EQ.0) THEN
           WRITE (Iunit,'(4x,f9.4,A,A,A,F9.4)') dmin, '|', glines(0) &
                (1:50), '|Avg =', sumiso(j)
        ELSE
           WRITE (Iunit,'(4x,f9.4,A,A,A)') dmin, '|', glines(0)(1:50),  &
                '|'
        END IF
        WRITE (Iunit,9155)
        WRITE (Iunit,'(10x,A,45x,A)') 'Initial', 'Final'
        IF (Iunit.EQ.Tunit) THEN
           WRITE (*,9000)
           READ (*,9005) line
        END IF
        IF (Iunit.NE.Tunit) THEN
        END IF
     enddo
  enddo
  IF (ISCR.EQ.0 .AND. iage.EQ.1) WRITE (Iunit,9175) age,  &
       Model(Iflag(4))
  WRITE (Iunit,*)
  CLOSE (Rwunit)
  RETURN
9000 FORMAT (/,' Hit <Enter> to continue')
9005 FORMAT (A)
9010 FORMAT (15X,' Computed    Observed')
9015 FORMAT (/,' Problems with ',A,/)
9020 FORMAT (/,' Data used for ',A)
9025 FORMAT ('      Missing element and/or isotopic data in well ',A)
9030 FORMAT ('      Missing element data in well ',A)
9035 FORMAT ('      Initial value of ',A,' zero or negative')
9040 FORMAT ('      Initial C-14 value must be positive')
9045 FORMAT (/,' Initial Value:',F12.7,8X,'Modeled Final Value:',F12.7, &
       /)
9050 FORMAT ('     Model                 A0     Computed  Observed', &
       '    age',/,1X,'(for initial A0)',6X,'(initial)',1X, &
       '(no decay)',11X,'(final)',/,1X,60('-'))
9055 FORMAT (1X,A,3(F10.2),F10.0)
9060 FORMAT (I3,' dissolving phases:',/,1x,'Phase',7x,'Delta ',A,4x, &
       'Isotopic composition ',A)
9065 FORMAT (1X,A8,1X,F12.7,9X,F12.7)
9070 FORMAT (/,I3,' precipitating phases:',29X,'Average',/,1X,'Phase', &
       7X,'Delta ',A,4X,'Fractionation factor',1X, &
       'Isotopic composition ',A)
9075 FORMAT (A51,F12.7)
9080 FORMAT (/, &
       ' Hit <Enter> to continue, any other key to see graphs of' &
       ,/,3X,'the isotopic composition of precipitating phases.')
9085 FORMAT (1X,A)
9090 FORMAT (I3,' dissolving:')
9095 FORMAT (I3,' precipitating:')
9100 FORMAT (1X,2(F15.6))
9105 FORMAT (1X,A,1X,F10.4,2X,F10.4)
9110 FORMAT (1X,A,1X,F10.4,3X,'Undefined')
9115 FORMAT (1X,A,1X,F10.4,'*',1X,F10.4)
9120 FORMAT (1X,A,1X,F10.6,2X,F10.6)
9125 FORMAT (1X,A,1X,F10.6,3X,'Undefined')
9130 FORMAT (6X,'No data for ',A,' in ',A)
9135 FORMAT (6X,'No data for ',A,' of CH4 gas in CO2-CH4')
9140 FORMAT (1X,A,6X,'Insufficient data')
9145 FORMAT (7X,'Insufficient data')
9150 FORMAT (/,10X,'Isotopic composition of precipitating ',A,/)
9155 FORMAT (13X,'+',50('-'),'+')
9160 FORMAT (A,A,A,F9.4)
9165 FORMAT (1X,A)
9170 FORMAT (1X,A8,1X,F10.5,9X,F10.4)
9175 FORMAT (1X,73('-'),/,' Adjusted C-14 age in years: ',F7.0,'*',5X, &
       '* = based on ',A)
END SUBROUTINE CISO
!
!
!
SUBROUTINE CL1(K,L,M,N,KLMD,KLM2D,NKLMD,N2D,KODE,TOLER,ITER,X, &
     ERROR,CU,IU,S)
  IMPLICIT NONE
  INTEGER K, L, M, N, KLMD, KLM2D, NKLMD, N2D, KODE, ITER,  &
       IU(2,NKLMD), S(KLMD)
  DOUBLE PRECISION TOLER, X(N2D), ERROR, CU(2,NKLMD)
  ! THIS SUBROUTINE USES A MODIFICATION OF THE SIMPLEX
  ! METHOD OF LINEAR PROGRAMMING TO CALCULATE AN L1 SOLUTION
  ! TO A K BY N SYSTEM OF LINEAR EQUATIONS
  !             AX=B
  ! SUBJECT TO L LINEAR EQUALITY CONSTRAINTS
  !             CX=D
  ! AND M LINEAR INEQUALITY CONSTRAINTS
  !             EX.LE.F.
  ! DESCRIPTION OF PARAMETERS
  ! K      NUMBER OF ROWS OF THE MATRIX A (K.GE.1).
  ! L      NUMBER OF ROWS OF THE MATRIX C (L.GE.0).
  ! M      NUMBER OF ROWS OF THE MATRIX E (M.GE.0).
  ! N      NUMBER OF COLUMNS OF THE MATRICES A,C,E (N.GE.1).
  ! KLMD   SET TO AT LEAST K+L+M FOR ADJUSTABLE DIMENSIONS.
  ! KLM2D  SET TO AT LEAST K+L+M+2 FOR ADJUSTABLE DIMENSIONS.
  ! NKLMD  SET TO AT LEAST N+K+L+M FOR ADJUSTABLE DIMENSIONS.
  ! N2D    SET TO AT LEAST N+2 FOR ADJUSTABLE DIMENSIONS
  ! Q      TWO DIMENSIONAL REAL ARRAY WITH KLM2D ROWS AND
  !        AT LEAST N2D COLUMNS.
  !        ON ENTRY THE MATRICES A,C AND E, AND THE VECTORS
  !        B,D AND F MUST BE STORED IN THE FIRST K+L+M ROWS
  !        AND N+1 COLUMNS OF Q AS FOLLOWS
  !             A B
  !         Q = C D
  !             E F
  !        THESE VALUES ARE DESTROYED BY THE SUBROUTINE.
  ! KODE   A CODE USED ON ENTRY TO, AND EXIT
  !        FROM, THE SUBROUTINE.
  !        ON ENTRY, THIS SHOULD NORMALLY BE SET TO 0.
  !        HOWEVER, IF CERTAIN NONNEGATIVITY CONSTRAINTS
  !        ARE TO BE INCLUDED IMPLICITLY, RATHER THAN
  !        EXPLICITLY IN THE CONSTRAINTS EX.LE.F, THEN KODE
  !        SHOULD BE SET TO 1, AND THE NONNEGATIVITY
  !        CONSTRAINTS INCLUDED IN THE ARRAYS X AND
  !        RES (SEE BELOW).
  !        ON EXIT, KODE HAS ONE OF THE
  !        FOLLOWING VALUES
  !             0- OPTIMAL SOLUTION FOUND,
  !             1- NO FEASIBLE SOLUTION TO THE
  !                CONSTRAINTS,
  !             2- CALCULATIONS TERMINATED
  !                PREMATURELY DUE TO ROUNDING ERRORS,
  !             3- MAXIMUM NUMBER OF ITERATIONS REACHED.
  ! TOLER  A SMALL POSITIVE TOLERANCE. EMPIRICAL
  !        EVIDENCE SUGGESTS TOLER = 10**(-D*2/3),
  !        WHERE D REPRESENTS THE NUMBER OF DECIMAL
  !        DIGITS OF ACCURACY AVAILABLE. ESSENTIALLY,
  !        THE SUBROUTINE CANNOT DISTINGUISH BETWEEN ZERO
  !        AND ANY QUANTITY WHOSE MAGNITUDE DOES NOT EXCEED
  !        TOLER. IN PARTICULAR, IT WILL NOT PIVOT ON ANY
  !        NUMBER WHOSE MAGNITUDE DOES NOT EXCEED TOLER.
  ! ITER   ON ENTRY ITER MUST CONTAIN AN UPPER BOUND ON
  !        THE MAXIMUM NUMBER OF ITERATIONS ALLOWED.
  !        A SUGGESTED VALUE IS 10*(K+L+M). ON EXIT ITER
  !        GIVES THE NUMBER OF SIMPLEX ITERATIONS.
  ! X      ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST N2D.
  !        ON EXIT THIS ARRAY CONTAINS A
  !        SOLUTION TO THE L1 PROBLEM. IF KODE=1
  !        ON ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE
  !        SIMPLE NONNEGATIVITY CONSTRAINTS ON THE
  !        VARIABLES. THE VALUES -1, 0, OR 1
  !        FOR X(J) INDICATE THAT THE J-TH VARIABLE
  !        IS RESTRICTED TO BE .LE.0, UNRESTRICTED,
  !        OR .GE.0 RESPECTIVELY.
  ! RES    ONE DIMENSIONAL REAL ARRAY OF SIZE AT LEAST KLMD.
  !        ON EXIT THIS CONTAINS THE RESIDUALS B-AX
  !        IN THE FIRST K COMPONENTS, D-CX IN THE
  !        NEXT L COMPONENTS (THESE WILL BE =0),AND
  !        F-EX IN THE NEXT M COMPONENTS. IF KODE=1 ON
  !        ENTRY, THIS ARRAY IS ALSO USED TO INCLUDE SIMPLE
  !        NONNEGATIVITY CONSTRAINTS ON THE RESIDUALS
  !        B-AX. THE VALUES -1, 0, OR 1 FOR RES(I)
  !        INDICATE THAT THE I-TH RESIDUAL (1.LE.I.LE.K) IS
  !        RESTRICTED TO BE .LE.0, UNRESTRICTED, OR .GE.0
  !        RESPECTIVELY.
  ! ERROR  ON EXIT, THIS GIVES THE MINIMUM SUM OF
  !        ABSOLUTE VALUES OF THE RESIDUALS.
  ! CU     A TWO DIMENSIONAL REAL ARRAY WITH TWO ROWS AND
  !        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE.
  ! IU     A TWO DIMENSIONAL INTEGER ARRAY WITH TWO ROWS AND
  !        AT LEAST NKLMD COLUMNS USED FOR WORKSPACE.
  ! S      INTEGER ARRAY OF SIZE AT LEAST KLMD, USED FOR
  !        WORKSPACE.
  !
  DOUBLE PRECISION Clmain, Cleach, Pdat, Res, Sfinal, Sinit, Maxdel,  &
       Mindel, Dfinal
  COMMON /DP3   / Clmain(202,40), Cleach(202,40), Pdat(40,40),  &
       Res(100), Sfinal(39), Sinit(5,59), Maxdel(39),  &
       Mindel(39), Dfinal
  DOUBLE PRECISION cuv, pivot, sn, sum, tpivot, xmax, xmin, z, zu,  &
       zv
  INTEGER i, j, ia, ii, in, js, kk, nk, n1, n2, jmn, jpn, klm, nkl,  &
       nk1, iimn, iout, klm1, klm2, nklm, nkl1, maxit, iphase,  &
       kforce, iineg
  INTRINSIC DABS, IABS
  !
  ! INITIALIZATION.
  !
  maxit = ITER
  n1 = N+1
  n2 = N+2
  nk = N+K
  nk1 = nk+1
  nkl = nk+L
  nkl1 = nkl+1
  klm = K+L+M
  klm1 = klm+1
  klm2 = klm+2
  nklm = N+klm
  kforce = 1
  ITER = 0
  js = 1
  ia = 0
  ! SET UP LABELS IN CLEACH
  DO j = 1, N
     Cleach(klm2,j) = j
  enddo
  DO i = 1, klm
     Cleach(i,n2) = N+i
     IF (Cleach(i,n1).LT.0.0D0) THEN
        DO j = 1, n2
           Cleach(i,j) = -Cleach(i,j)
        enddo
     END IF
  enddo
  ! SET UP PHASE 1 COSTS.
  iphase = 2
  DO j = 1, nklm
     CU(1,j) = 0.0D0
     CU(2,j) = 0.0D0
     IU(1,j) = 0
     IU(2,j) = 0
  enddo
  IF (L.NE.0) THEN
     DO j = nk1, nkl
        CU(1,j) = 1.0D0
        CU(2,j) = 1.0D0
        IU(1,j) = 1
        IU(2,j) = 1
     enddo
     iphase = 1
  END IF
  IF (M.NE.0) THEN
     DO j = nkl1, nklm
        CU(2,j) = 1.0D0
        IU(2,j) = 1
        jmn = j-N
        IF (Cleach(jmn,n2).LT.0.0D0) iphase = 1
     enddo
  END IF
  IF (KODE.NE.0) THEN
     DO j = 1, N
        IF (X(j).LT.0.0D0) THEN
           CU(1,j) = 1.0D0
           IU(1,j) = 1
        ELSE IF (X(j).NE.0.0D0) THEN
           CU(2,j) = 1.0D0
           IU(2,j) = 1
        END IF
     enddo
     DO j = 1, K
        jpn = j+N
        IF (Res(j).LT.0.0D0) THEN
           CU(1,jpn) = 1.0D0
           IU(1,jpn) = 1
           IF (Cleach(j,n2).GT.0.0D0) iphase = 1
        ELSE IF (Res(j).NE.0.0D0) THEN
           CU(2,jpn) = 1.0D0
           IU(2,jpn) = 1
           IF (Cleach(j,n2).LT.0.0D0) iphase = 1
        END IF
     enddo
  END IF
  IF (iphase.EQ.2) GO TO 290
  ! COMPUTE THE MARGINAL COSTS.
90 DO j = js, n1
     sum = 0.0D0
     DO i = 1, klm
        ii = Cleach(i,n2)
        IF (ii.LT.0) THEN
           iineg = -ii
           z = CU(2,iineg)
        ELSE
           z = CU(1,ii)
        END IF
        sum = sum+Cleach(i,j)*z
     enddo
     Cleach(klm1,j) = sum
  enddo
  DO j = js, N
     ii = Cleach(klm2,j)
     IF (ii.LT.0) THEN
        iineg = -ii
        z = CU(2,iineg)
     ELSE
        z = CU(1,ii)
     END IF
     Cleach(klm1,j) = Cleach(klm1,j)-z
  enddo
  ! DETERMINE THE VECTOR TO ENTER THE BASIS.
130 xmax = 0.0D0
  IF (js.LE.N) THEN
     DO j = js, N
        zu = Cleach(klm1,j)
        ii = Cleach(klm2,j)
        IF (ii.GT.0) THEN
           zv = -zu-CU(1,ii)-CU(2,ii)
        ELSE
           ii = -ii
           zv = zu
           zu = -zu-CU(1,ii)-CU(2,ii)
        END IF
        IF (kforce.NE.1 .OR. ii.LE.N) THEN
           IF (IU(1,ii).NE.1) THEN
              IF (zu.GT.xmax) THEN
                 xmax = zu
                 in = j
              END IF
           END IF
           IF (IU(2,ii).NE.1) THEN
              IF (zv.GT.xmax) THEN
                 xmax = zv
                 in = j
              END IF
           END IF
        END IF
     enddo
     IF (xmax.LE.TOLER) GO TO 280
     IF (Cleach(klm1,in).NE.xmax) THEN
        DO i = 1, klm2
           Cleach(i,in) = -Cleach(i,in)
        enddo
        Cleach(klm1,in) = xmax
     END IF
     ! DETERMINE THE VECTOR TO LEAVE THE BASIS.
     IF (iphase.NE.1 .AND. ia.NE.0) THEN
        xmax = 0.0D0
        DO i = 1, ia
           z = DABS(Cleach(i,in))
           IF (z.GT.xmax) THEN
              xmax = z
              iout = i
           END IF
        enddo
        IF (xmax.GT.TOLER) THEN
           DO j = 1, n2
              z = Cleach(ia,j)
              Cleach(ia,j) = Cleach(iout,j)
              Cleach(iout,j) = z
           enddo
           iout = ia
           ia = ia-1
           pivot = Cleach(iout,in)
           GO TO 220
        END IF
     END IF
     kk = 0
     DO i = 1, klm
        z = Cleach(i,in)
        IF (z.GT.TOLER) THEN
           kk = kk+1
           Res(kk) = Cleach(i,n1)/z
           S(kk) = i
        END IF
     enddo
190  IF (kk.GT.0) THEN
        xmin = Res(1)
        iout = S(1)
        j = 1
        IF (kk.NE.1) THEN
           DO i = 2, kk
              IF (Res(i).LT.xmin) THEN
                 j = i
                 xmin = Res(i)
                 iout = S(i)
              END IF
           enddo
           Res(j) = Res(kk)
           S(j) = S(kk)
        END IF
        kk = kk-1
        pivot = Cleach(iout,in)
        ii = Cleach(iout,n2)
        IF (iphase.NE.1) THEN
           IF (ii.LT.0) THEN
              iineg = -ii
              IF (IU(1,iineg).EQ.1) GO TO 220
           ELSE IF (IU(2,ii).EQ.1) THEN
              GO TO 220
           END IF
        END IF
        ii = IABS(ii)
        cuv = CU(1,ii)+CU(2,ii)
        IF (Cleach(klm1,in)-pivot*cuv.GT.TOLER) THEN
           ! BYPASS INTERMEDIATE VERTICES.
           DO j = js, n1
              z = Cleach(iout,j)
              Cleach(klm1,j) = Cleach(klm1,j)-z*cuv
              Cleach(iout,j) = -z
           enddo
           Cleach(iout,n2) = -Cleach(iout,n2)
           GO TO 190
        END IF
     ELSE
        KODE = 2
        GO TO 340
     END IF
     ! GAUSS-JORDAN ELIMINATION.
220  IF (ITER.LT.maxit) THEN
        ITER = ITER+1
        DO j = js, n1
           IF (j.NE.in) Cleach(iout,j) = Cleach(iout,j)/pivot
        enddo
        DO j = js, n1
           IF (j.NE.in) THEN
              z = -Cleach(iout,j)
              DO i = 1, klm1
                 IF (i.NE.iout) Cleach(i,j) = Cleach(i,j)+z*Cleach(i,in)
              enddo
           END IF
        enddo
        tpivot = -pivot
        DO i = 1, klm1
           IF (i.NE.iout) Cleach(i,in) = Cleach(i,in)/tpivot
        enddo
        Cleach(iout,in) = 1.0D0/pivot
        z = Cleach(iout,n2)
        Cleach(iout,n2) = Cleach(klm2,in)
        Cleach(klm2,in) = z
        ii = DABS(z)
        IF (IU(1,ii).NE.0 .AND. IU(2,ii).NE.0) THEN
           DO i = 1, klm2
              z = Cleach(i,in)
              Cleach(i,in) = Cleach(i,js)
              Cleach(i,js) = z
           enddo
           js = js+1
        END IF
        GO TO 130
     ELSE
        KODE = 3
        GO TO 340
     END IF
  END IF
  ! TEST FOR OPTIMALITY.
280 IF (kforce.EQ.0) THEN
     IF (iphase.NE.1) THEN
        ! PREPARE OUTPUT.
        KODE = 0
        GO TO 340
     ELSE IF (Cleach(klm1,n1).GT.TOLER) THEN
        KODE = 1
        GO TO 340
     END IF
  ELSE IF (iphase.NE.1 .OR. Cleach(klm1,n1).GT.TOLER) THEN
     kforce = 0
     GO TO 130
  END IF
  ! SET UP PHASE 2 COSTS.
290 iphase = 2
  DO j = 1, nklm
     CU(1,j) = 0.0D0
     CU(2,j) = 0.0D0
  enddo
  DO j = n1, nk
     CU(1,j) = 1.0D0
     CU(2,j) = 1.0D0
  enddo
  DO i = 1, klm
     ii = Cleach(i,n2)
     IF (ii.GT.0) THEN
        IF (IU(1,ii).EQ.0) cycle
        CU(1,ii) = 0.0D0
     ELSE
        ii = -ii
        IF (IU(2,ii).EQ.0) cycle
        CU(2,ii) = 0.0D0
     END IF
     ia = ia+1
     DO j = 1, n2
        z = Cleach(ia,j)
        Cleach(ia,j) = Cleach(i,j)
        Cleach(i,j) = z
     enddo
  enddo
  GO TO 90
340 sum = 0.0D0
  DO j = 1, N
     X(j) = 0.0D0
  enddo
  DO i = 1, klm
     Res(i) = 0.0D0
  enddo
  DO i = 1, klm
     ii = Cleach(i,n2)
     sn = 1.0D0
     IF (ii.LE.0) THEN
        ii = -ii
        sn = -1.0D0
     END IF
     IF (ii.GT.N) THEN
        iimn = ii-N
        Res(iimn) = sn*Cleach(i,n1)
        IF (ii.GE.n1 .AND. ii.LE.nk) sum = sum+Cleach(i,n1)
     ELSE
        X(ii) = sn*Cleach(i,n1)
     END IF
  enddo
  ERROR = sum
  RETURN
END SUBROUTINE CL1
!
!
!
Subroutine Parse_Line(line, name, f, it, xelem, xcoef, n) 
	implicit none
	character*(*) line, name, f, it, xelem(*)
	double precision xcoef(39,36)
	logical nextpair
	integer i, n
	character*2 e
	double precision c
	
    !READ (line,'(A8,2(A1),7(A2,F8.4))',ERR=220,END=220) middle, F(i), It(i),  &
    !     (pelem(j),Pcoeff(i,j),j=1,7)
	!call parse_line(line, middle, f(i), it(i), pelem(1), pcoef(i,1))
	read (line,'(A8,2(A1))') name, f, it
	do i = 1, 7
		xelem(i) = ' '
		xcoef(n, i) = 0
	enddo
	i = 1
	line = line(11:)
	do while (nextpair(line, e, c)) 
		 xelem(i) = e
		 xcoef(n, i) = c
		 i = i + 1
	enddo
	return
end subroutine Parse_Line
!
!
!
logical function nextpair(line, e, c)
	implicit none
	double precision c
	character*(*) e, line
	integer i
	integer lens
	external lens
	if (lens(line) < 3) then
		e = ' '
		c = 0.0
		nextpair = .false.
		return
	endif
	do i = 1, lens(line)
		if (line(i:i) .ne. ' ') then
			exit
		endif
	enddo
	line = line(i:)
	read(line,'(A2)') e
	line = line(3:)
	! find next character 
	if (lens(line) == 0) then 
		e = ' '
		c = 0.0
		nextpair = .false.
		return
	endif
	do i = 1, lens(line)
		if (((ichar(line(i:i)) .ge. ICHAR('a')) .and. (ichar(line(i:i)) .le. ICHAR('z'))) .or. &
		    ((ichar(line(i:i)) .ge. ICHAR('A')) .and. (ichar(line(i:i)) .le. ICHAR('Z')))) then
			  exit
		endif
	enddo
	read(line(1:i-1),'(f)') c
	if (i > lens(line)) then
		line = ' '
	else
		line = line(i:)
	endif
	nextpair = .true.
	return
end function nextpair

