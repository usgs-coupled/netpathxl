!
!
!
SUBROUTINE RUNWATEQ(file, silent)
    use filenames
    IMPLICIT NONE
    LOGICAL error
    INTEGER icort, i, inumf, LENS
    CHARACTER*1 yn
    CHARACTER mfile(200)*40, UPCS80*80
    !
    INTEGER Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
    COMMON /INT4DB/ Dbsfg(50,45), Idefault(5), Iu(50,4), Nwlls,  &
       Totwell, Tot(50)
    INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
    COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
    character*(*) file
    !CHARACTER*256 Dfile, path
    !COMMON /FILEC / Dfile, path
    !
    EXTERNAL INITIALIZE, READFILE, CONVDATA, WRAPUP, ITERATE, &
       OUTWRITE, PATWRITE214, LENS, UPCS80
    character*256 string
    logical silent
    !
	Ir = 7
	Iw1 = 8
	Iw2 = 9
	Iex1 = 10
	Io1 = 11
	Iscr2 = 13
	if (.not. silent) then
        WRITE (*,9000)
        READ (*,9005) yn
        call moverelative(-2)
        call clpart 
        call moverelative(-2)
        call clpart 
        icort = 0
        IF (yn.EQ.'Y' .OR. yn.EQ.'y') icort = 1
        excel_cb = icort
    else
        icort = excel_cb
        if (icort == 0) then
            WRITE(*,*) "Charge balance for speciation calculations: No"
        else
            WRITE(*,*) "Charge balance for speciation calculations: Yes"
        endif
    endif
    CALL INITIALIZE(root)
    CALL READFILE(silent)
    !
    WRITE (*,9015)
    !
    DO i = 1, Nwlls
        IF ((i/5)*5.EQ.i) then
            call moverelative(-1)
            call clpart 
            WRITE (*,9010) i
         endif

        error = .FALSE.
        CALL CONVDATA(i,error)
        IF (error) THEN
	        WRITE (*,*) "Error converting data, well ", i
	        WRITE (*,"(A,$)") "Enter to continue "
	        READ (*,9005) yn
	        cycle
 !       CALL WRAPUP
 !       GOTO 70
        END IF
        CALL ITERATE(icort,error)
        IF (error) THEN
	        WRITE (*,*) "Wateq did not converge, well ", i
		    WRITE (*,"(A,$)") "Enter to continue "
	        READ (*,9005) yn
	        cycle
    !       WRITE (*,9020) i
    !       CALL WRAPUP

    !       GOTO 70
        END IF
        !
        CALL OUTWRITE
        CALL PATWRITE214(i,error)
        if (error) then 
            WRITE (*,*) "Error converting data, well ", i
            WRITE (*,"(A,$)") "Enter to continue "
            READ (*,9005) yn
            cycle
        endif
    !      IF (error) GOTO 70
    enddo
  !
    OPEN (unit=Io1,file='netpath.fil')
    inumf = 0
20  READ (Io1,9030,err=30,end=30) mfile(inumf+1)
    inumf = inumf+1
    IF (UPCS80(mfile(inumf)).EQ.UPCS80(file) .AND. Icase.EQ.0) &
       GO TO 50
    IF (mfile(inumf).NE.file) GO TO 20
    GO TO 50
30  CLOSE (Io1,status='DELETE')
    OPEN (Io1,file='netpath.fil')
    DO i = 1, inumf
        WRITE (Io1,'(A)') mfile(i)(:LENS(mfile(i)))
    enddo
    WRITE (Io1,'(A)') file(1:LENS(file))
50  CLOSE (Io1)
    WRITE (*,9025) file(1:LENS(file)), file(1:LENS(file))
60  WRITE (*,9035)
    READ (*,'(A1)') yn
    call moverelative(-2)
    call clpart 
    IF (yn.EQ.'Y' .OR. yn.EQ.'y') then
	    string='notepad '//path(1:lens(path))//'\'//file(1:lens(file))//'.out'
	    !call system(string)
	    call docreate(string)
	endif
    CALL WRAPUP
  !
70  continue
    RETURN
9000 FORMAT (/,' Do you want to adjust the .pat file to approximate',/, &
       ' charge balance in the input data?',/,' <Enter> = no')
9005 FORMAT (A1)
9010 FORMAT ('   Processing well: ',I2)
9015 FORMAT ('  Running aqueous model -- please wait')
9020 FORMAT (' Error in well #',I3)
9025 FORMAT (/,1X,A,'.pat created.',/,1X,A, &
       '.out contains the output from the speciation.',/)
9030 FORMAT (A)
9035 FORMAT (' Do you want to look at the speciation results? (Y/N)')
end subroutine RUNWATEQ

!
!
!
SUBROUTINE CONVDATA(I,ERROR)
  IMPLICIT NONE
  REAL dat(45)
  INTEGER jj, itrans(4:30), I, lpspec
  LOGICAL ERROR
  !
  REAL Dbdata
  COMMON /DB    / Dbdata(50,45)
  INTEGER Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
  COMMON /INT4DB/ Dbsfg(50,45), Idefault(5), Iu(50,4), Nwlls,  &
       Totwell, Tot(50)
  CHARACTER Wllnms*80, Address*40, Lat*40, Formation*17
  COMMON /CHAR1 / Wllnms(50), Address(50,5), Lat(50), Formation(50)
  DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
       Gamma(250), Totmol(30), Consta, Constb, Constc,  &
       Constf, Constr, Speclk(250), Ppm(250),  &
       Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
       Epmcat, Epman, C1save
  COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
       Totmol, Consta, Constb, Constc, Constf, Constr,  &
       Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
       Epmcat, Epman, C1save
  CHARACTER*80 Wellname
  COMMON /WMCHAR/ Wellname
  INTEGER Coralk, Unitflag, Pecalc, Idaves
  COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
  !
  DATA itrans/7, 9, 10, 11, 21, 22, 37, 18, 20, 15, 12, 4, 13, 23,  &
       17, 25, 14, 19, 16, 24, 38, 6, 35, 26, 3, 0, 0/
  !
  ! we can't let undefined temperatures and ph's go...
  !
  IF (Dbsfg(I,1).LT.0 .OR. Dbsfg(I,2).LT.0) THEN
     WRITE (*,9000) I
     ERROR = .TRUE.
     RETURN
  END IF
  DO jj = 1, 45
     dat(jj) = Dbdata(I,jj)
     IF (Dbsfg(I,jj).LT.0) dat(jj) = 0
  enddo
  !
  !   handle default density (1, not 0)
  IF (dat(28).EQ.0.) dat(28) = 1.
  !
  !   find value for eh
  Eh = dat(8)
  Pecalc = Iu(I,2)
  IF (Pecalc.NE.1) Eh = 9.9D0
  IF (Pecalc.EQ.4 .AND. (dat(6).LE.0.0.OR.dat(13).LE.0.0)) &
       Pecalc = 0
  IF (Pecalc.EQ.0 .AND. (dat(21).GT.0.0.OR.dat(22).GT.0.0)) &
       WRITE (*,9005) Wllnms(I)(5:36)
  IF (Pecalc.EQ.0) THEN
     Pecalc = 1
     Eh = 0.0D0
  END IF

  !   Tritium must not be negative.
  IF (dat(5).LT.0.0 .AND. Dbsfg(I,5).GE.0) THEN
     dat(5) = 0
     WRITE (*,9010) Wllnms(I)(5:36)
  END IF

  Ph = dat(2)
  Tempc = dat(1)
  Density = dat(28)
  Coralk = Iu(I,3)
  IF (Coralk.EQ.3) THEN
     Coralk = 0
     IF (Iu(I,1).EQ.2 .OR. Iu(I,1).EQ.3) dat(4) = dat(4)*1.2193
  END IF
  Unitflag = Iu(I,1)
  Idaves = Iu(I,4)
  Wellname = Wllnms(I)(5:36)
  DO lpspec = 1, 250
     Mol(lpspec) = 0.0D0
     Ppm(lpspec) = 0.0D0
     Logmol(lpspec) = 0.0D0
  enddo
  DO lpspec = 4, 30
     IF (itrans(lpspec).EQ.0) THEN
        Ppm(lpspec) = 0
     ELSE
        Ppm(lpspec) = dat(itrans(lpspec))
     END IF
  enddo
  RETURN
9000 FORMAT (/,' ERROR at well',I3, &
       ': Temperature and pH must be defined.')
9005 FORMAT (' WARNING - Eh set to 0 for ',A)
9010 FORMAT (' Tritium negative: saved as 0: ',A)
end subroutine CONVDATA
!
!
!
      DOUBLE PRECISION FUNCTION CALCMOL(ISPEC,IBASE)
      DOUBLE PRECISION anum
      INTEGER ISPEC, IBASE, i
      CHARACTER*256 ident_string
!
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
!
      INTRINSIC DLOG10
!
      ident_string='$Id: wateqfp.f 304 2005-05-13 21:30:19Z dlpark $'
!   Calculate numerator
      anum = Speclk(ISPEC)
      DO 10 i = 1, Nspecspec(ISPEC)
        IF (Alpha(Specspec(ISPEC,i)).LT.1.0D-40) THEN
          CALCMOL = 0.0D0
          GOTO 20
        END IF
        anum = anum + Speccoef(ISPEC,i)*DLOG10(Alpha(Specspec(ISPEC,i)))
   10 CONTINUE
!   These logs should never have errors, because the alphas have been
!   checked for zero above.
      anum = anum - DLOG10(Gamma(ISPEC))
      anum = anum - DLOG10(Mol(IBASE))
      CALCMOL = 10.0D0**anum
   20 RETURN
      END
!
!
!
      DOUBLE PRECISION FUNCTION GETCOEF(ISPEC,IBASE)
      INTEGER ISPEC, IBASE, i
!
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
!
      GETCOEF = 0.0
      DO 10 i = 1, Nspecspec(ISPEC)
        IF (Specspec(ISPEC,i).EQ.IBASE) THEN
          GETCOEF = Speccoef(ISPEC,i)
          GOTO 20
        END IF
   10 CONTINUE
   20 RETURN
      END
!
!
!
      SUBROUTINE CALCSIS
      INTEGER lpreac, lp
      DOUBLE PRECISION liap, lk, si, esi, iap, k, dummy1
!
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      INTEGER Coralk, Unitflag, Pecalc, Idaves
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
      INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
!
      INTRINSIC DLOG10, DABS
!
      WRITE (Iw1,9000)
      WRITE (Iw1,9005)
      dummy1 = (298.15D0-Tempk)/(298.15D0*Tempk*Constc*Constr)
      DO 20 lpreac = 1, Numreacs
        IF (Reacmflg(lpreac).EQ.0) THEN
          lk = Reack(lpreac) - Reach(lpreac)*dummy1
        ELSE
          lk = Reactemp(lpreac,1) + Reactemp(lpreac,2) &
              *Tempk + Reactemp(lpreac,3)/Tempk + Reactemp(lpreac,4) &
              *DLOG10(Tempk) + Reactemp(lpreac,5)/Tempk**2
        END IF
        liap = 0.0D0
        DO 10 lp = 1, Nreacspec(lpreac)
          IF (Reacspec(lpreac,lp).EQ.2 .AND. DABS(Pe(Pecalc)).GT.50.0D0) &
             GOTO 20
          IF (Alpha(Reacspec(lpreac,lp)).LT.1.0D-40) GOTO 20
          liap = liap + DLOG10(Alpha(Reacspec(lpreac,lp))) &
                *Reaccoef(lpreac,lp)
   10   CONTINUE
        si = liap - lk
        IF (si.GT.1.0D38 .OR. liap.GT.1.0D38 .OR. lk.GT.1.0D38) THEN
          WRITE (Iw1,9015) lpreac, Reacname(lpreac), liap, lk, si
        ELSE
          esi = 10**si
          iap = 10**liap
          k = 10**lk
          WRITE (Iw1,9010) lpreac, Reacname(lpreac), iap, k, liap, lk,  &
                          esi, si
        END IF
!
   20 CONTINUE
      RETURN
!
 9000 FORMAT (//)
 9005 FORMAT (//,6X,'PHASE',6X,'IAP',7X,'KT',6X,'LOG IAP',2X,'LOG KT', &
             4X,'IAP/KT',2X,'LOG IAP/KT',/)
 9010 FORMAT (1X,I3,1X,A8,2(1PE10.3),2(1X,0PF8.3),1X,1PE10.3,1X,0PF8.3)
 9015 FORMAT (1X,I3,1X,A8,21X,2(F8.3,1X),9X,F10.3)
      END
!
!
!
      SUBROUTINE INITIALIZE(FNAME)
      CHARACTER*40 FNAME
!
      INTEGER Iw1, Iw2, ilen, lpspec, LENS
      INTEGER Icase, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBCHAR/ Totname, Reacname, Specname
!
      EXTERNAL LENS
!
      ilen = LENS(FNAME)
!   Open .out file
      OPEN (UNIT=Iw1,FILE=FNAME(1:ilen)//'.out',STATUS='UNKNOWN',ERR=20)
      CLOSE (UNIT=Iw1,STATUS='DELETE',ERR=20)
      OPEN (UNIT=Iw1,FILE=FNAME(1:ilen)//'.out',STATUS='NEW',ERR=20)
!   Open .pat file
      OPEN (UNIT=Iw2,FILE=FNAME(1:ilen)//'.pat',STATUS='UNKNOWN',ERR=20)
      CLOSE (UNIT=Iw2,STATUS='DELETE',ERR=20)
      OPEN (UNIT=Iw2,FILE=FNAME(1:ilen)//'.pat',STATUS='NEW',ERR=20)
!      WRITE (Iw2,9000)
      WRITE (Iw2,'("2.14",T20,"# File format")')
      DO 10 lpspec = 1, 250
        Specname(lpspec) = ' '
   10 CONTINUE
      RETURN
   20 STOP 'Error opening .out and .pat files'
 9000 FORMAT ('ORDER IS:  WELL,',/,'C, S, CA, AL, MG, NA, K, CL,',/, &
             'F, SI, BR, B, BA, LI, SR, FE,',/, &
             'MN, N, P, TEMP, H2S, SO4, N15, RS of N,',/, &
             'DOX, HCO3, PH, H2CO3*, CO3, CARBONATES, FE2+, FE3+',/, &
             'MN2+, MN3+, MN6+, MN7+, CH4, DOC, RS OF DOC, BLANK',/, &
             'C13, C14, SR87, D, O-18, TRITIUM, 34SSO4, 34SH2S',/)
      END
!
!
!
      SUBROUTINE ITERATE(ICORT,ERROR)
      LOGICAL ERROR, done
      INTEGER maxiters, iter, ICORT, iunitsave, itercb
      EXTERNAL ITERSET, ITERLOOP
      DOUBLE PRECISION factpos
      INTEGER Coralk, Unitflag, Pecalc, Idaves
      COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
!
      ERROR = .FALSE.
      maxiters = 200
      factpos = 1.0
      iunitsave = Unitflag
      itercb = 0
   10 itercb = itercb + 1
      CALL ITERSET(ICORT,factpos)
      done = .FALSE.
      iter = 0
   20 iter = iter + 1
      CALL ITERLOOP(done,iter,maxiters)
      IF (.NOT.done .AND. iter.LT.maxiters) GOTO 20
      IF (iter.LE.3) GOTO 20
      IF (iter.EQ.maxiters .AND. .NOT.done) ERROR = .TRUE.
      CALL CHARGE_BALANCE(ICORT,done,factpos,itercb)
      Unitflag = 5
      IF (itercb.EQ.maxiters .AND. .NOT.done) ERROR = .TRUE.
      IF (.NOT.ERROR .AND. .NOT.done) GOTO 10
      Unitflag = iunitsave
      CALL OUTPUT
      RETURN
      END
!
!
!
      SUBROUTINE ITERGAM
!
!   Compute gammas
!
!   Called each time through iteration
      INTEGER lpspec, lpinner
      DOUBLE PRECISION c1, xmuhl, gamlog, dummy1, lalpha
!
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      INTEGER Coralk, Unitflag, Pecalc, Idaves
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
!
      INTRINSIC SQRT, DABS, DLOG10
!
      c1 = 0.0D0
      Xmu = 0.0D0
      DO 10 lpspec = 1, 250
        IF (lpspec.NE.2 .AND. lpspec.NE.3) THEN
          IF (Specname(lpspec).NE.' ') THEN
!      Don't include computed O2 value if DOX number was given.
            IF (lpspec.NE.Lexcept(14) .OR. Totmol(Lexcept(13)).LE.0.0D0) &
               THEN
              Xmu= Xmu + (0.5D0*Mol(lpspec)*Specz(lpspec)*Specz(lpspec))
              c1 = c1 + Mol(lpspec)
            END IF
          END IF
        END IF
   10 CONTINUE
      xmuhl = SQRT(Xmu)
      DO 20 lpspec = 1, 250
        IF (Specname(lpspec).NE.' ') THEN
          IF (Specgflg(lpspec).NE.0) THEN
!         Use WATEQ Debye-Huckel
            gamlog = -Consta*Specz(lpspec) &
                    **2*xmuhl/(1.0D0+Constb*Specacon(lpspec)*xmuhl) &
                    + Specbcon(lpspec)*Xmu
          ELSE IF (DABS(Specdha(lpspec)).LT.1.D-3 .OR. Idaves.EQ.1) THEN
!            Use Davies equation
            gamlog = -Consta*Specz(lpspec) &
                    **2D0*(xmuhl/(1+xmuhl)-0.3D0*Xmu)
          ELSE
!            Use extended Debye-Huckel
            gamlog = -Consta*Specz(lpspec) &
                    **2.0D0*xmuhl/(1.0D0+Constb*Specdha(lpspec)*xmuhl)
          END IF
          IF (Specz(lpspec).EQ.0.0D0) gamlog = 0.1D0*Xmu
          IF (lpspec.EQ.2 .OR. lpspec.EQ.3) gamlog = 0.0D0
          Gamma(lpspec) = 0.0D0
          IF (gamlog.LT.80.0D0) Gamma(lpspec) = 10.0D0**gamlog
        END IF
   20 CONTINUE
      DO 30 lpspec = 4, 250
        Alpha(lpspec) = Gamma(lpspec)*Mol(lpspec)
   30 CONTINUE
 
!   activity of water (and related)
      Alpha(3) = 1.0D0 - (1.7D-2*c1)
      Alpha(1) = 10.0D0**(-Ph)
      Mol(1) = Alpha(1)/Gamma(1)
 
!   activity of E
      Pe(2) = 0.0D0
      Pe(3) = 0.0D0
      IF (Totmol(Lexcept(13)).GT.0.0D0) THEN
        Pe(2) = -0.25D0*Speclk(Lexcept(9)) - Ph - 0.5D0*DLOG10(Alpha(3)) &
               + 0.25D0*DLOG10(Totmol(Lexcept(13)))
        Pe(3) = 11.385D0 - Ph - 0.5D0*DLOG10(Alpha(3)) &
               + 0.25D0*DLOG10(Totmol(Lexcept(13)))
      END IF
      dummy1 = (298.15D0-Tempk)/(298.15D0*Tempk*Constc*Constr)
      IF (Alpha(Lexcept(5)).GT.0.0D0 .AND. Alpha(Lexcept(6)).GT.0.0D0) &
         THEN
        Pe(4) = ((40.644D0+65.44D0*dummy1)-DLOG10(Alpha(Lexcept(5))) &
               -4.0D0*DLOG10(Alpha(3))+DLOG10(Alpha(Lexcept(6)))- &
               10.0D0*Ph)/8.0D0
      ELSE
        Pe(4) = 0.0D0
      END IF
!
      Mol(2) = 10.0D0**(-Pe(Pecalc))
      Gamma(2) = 1.0D0
      Alpha(2) = Mol(2)*Gamma(2)
!
      DO 70 lpspec = 31, 250
        IF (Specname(lpspec).NE.' ') THEN
          DO 40 lpinner = 1, Nspecspec(lpspec)
            IF (Specspec(lpspec,lpinner).GT.3) GOTO 70
   40     CONTINUE
          lalpha = Speclk(lpspec)
          DO 50 lpinner = 1, Nspecspec(lpspec)
            IF (Alpha(Specspec(lpspec,lpinner)).LE.1.0D-40) THEN
              Alpha(lpspec) = 0.0D0
              GOTO 60
            END IF
            lalpha = lalpha + Speccoef(lpspec,lpinner) &
                    *DLOG10(Alpha(Specspec(lpspec,lpinner)))
   50     CONTINUE
          Alpha(lpspec) = 10.0D0**lalpha
   60     Mol(lpspec) = Alpha(lpspec)/Gamma(lpspec)
        END IF
   70 CONTINUE
!
      RETURN
      END
!
!
!
      SUBROUTINE ITERLOOP(DONE,ITER,MAXITERS)
      LOGICAL DONE
      INTEGER lpspec, lplist, icurr, lpalk, ITER, MAXITERS
      DOUBLE PRECISION denom, testmol, CALCMOL, GETCOEF, coef
!
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      INTEGER Coralk, Unitflag, Pecalc, Idaves
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
!
      EXTERNAL ITERGAM, CALCMOL, GETCOEF
      INTRINSIC DABS
!
!   Compute gammas
      CALL ITERGAM
!
!   For each master species
      DO 40 lpspec = 4, 30
        IF (Specname(lpspec).NE.' ') THEN
          Alpha(lpspec) = Mol(lpspec)*Gamma(lpspec)
!      Handle alkspec if we are doing carbon, and alkalinity was entered
          IF (lpspec.EQ.Lexcept(1) .AND. Coralk.LT.2) THEN
            denom = Specalk(lpspec)
          ELSE
            denom = 1.0D0
          END IF
          DO 10 lplist = 1, Speclist(lpspec,0)
            icurr = Speclist(lpspec,lplist)
            Mol(icurr) = CALCMOL(icurr,lpspec)
            coef = GETCOEF(icurr,lpspec)
!            if (Totname(lpspec).eq."F") then
!               write(*,*) "   ", Specname(icurr), Mol(icurr), coef
!            endif
!         Handle alkspec if we are doing carbon, and alk was entered
            IF (lpspec.EQ.Lexcept(1) .AND. Coralk.LT.2) THEN
              denom = denom + Mol(icurr)*Specalk(icurr)
            ELSE
              denom = denom + Mol(icurr)*coef
            END IF
   10     CONTINUE
          IF (lpspec.NE.Lexcept(1) .OR. Coralk.GT.0) THEN
            Mol(lpspec) = Totmol(lpspec)/denom
          ELSE
!         Uncorrected alkalinity, need to subtract non-carbonate
!         alkalinity
            testmol = Totmol(lpspec)
            DO 20 lpalk = 1, Lnoncalk(0)
              testmol = testmol - Mol(Lnoncalk(lpalk)) &
                       *Specalk(Lnoncalk(lpalk))
   20       CONTINUE
            Mol(lpspec) = testmol/denom
          END IF
!
          IF (Mol(lpspec).LT.0.0D0) Mol(lpspec) = 0.0D0
          Alpha(lpspec) = Mol(lpspec)*Gamma(lpspec)
          DO 30 lplist = 1, Speclist(lpspec,0)
            icurr = Speclist(lpspec,lplist)
            Mol(icurr) = Mol(icurr)*Mol(lpspec)
            Alpha(icurr) = Mol(icurr)*Gamma(icurr)
   30     CONTINUE
        END IF
   40 CONTINUE
      DONE = .TRUE.
      DO 80 lpspec = 4, 30
        IF (lpspec.NE.Lexcept(1) .OR. Coralk.EQ.2) THEN
!         Most cases
          testmol = Mol(lpspec)
          DO 50 lplist = 1, Speclist(lpspec,0)
            icurr = Speclist(lpspec,lplist)
            coef = GETCOEF(icurr,lpspec)
            testmol = testmol + Mol(Speclist(lpspec,lplist))*coef
   50     CONTINUE
        ELSE
!         Alkalinity
          testmol = 0.0D0
          DO 60 lplist = 1, Lcalk(0)
            testmol= testmol + Mol(Lcalk(lplist))*Specalk(Lcalk(lplist))
   60     CONTINUE
          IF (Coralk.EQ.0) THEN
            DO 70 lplist = 1, Lnoncalk(0)
              testmol = testmol + Mol(Lnoncalk(lplist)) &
                       *Specalk(Lnoncalk(lplist))
   70       CONTINUE
          END IF
        END IF
        IF (DABS(Totmol(lpspec)-testmol).GT.1.0D-8*Totmol(lpspec)) THEN
          DONE = .FALSE.
          IF (ITER.EQ.MAXITERS) WRITE (*,*) Totname(lpspec),  &
                                           Totmol(lpspec), testmol
        END IF
   80 CONTINUE
      RETURN
      END
!
!
!
      SUBROUTINE ITERSET(ICORT,FACTPOS)
!   Set up initial values
      INTEGER lpspec, i, ICORT
      DOUBLE PRECISION dummy1, dummy2, dummy3, dummy4, c1, c2
!
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save, FACTPOS, factneg
      INTEGER Coralk, Unitflag, Pecalc, Idaves
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
!
      INTRINSIC DLOG10, DABS, DSQRT, SQRT
!
!C      WRITE (Iw1,9000)
!C      WRITE (Iw1,9005) Wellname
      Tempk = Tempc + 273.15D0
!
!   Set Reaction K values
!
      Constc = 2.302585092D0
      Constf = 23.0603D0
      Constr = 1.98719D-3
 
      Pe(0) = 100.0D0
      IF (Eh.GT.9.0D0) THEN
        Pe(1) = 100.0D0
      ELSE
        Pe(1) = Eh/(Constc*Constr*Tempk/Constf)
      END IF
      dummy1 = (298.15D0-Tempk)/(298.15D0*Tempk*Constc*Constr)
      DO 10 lpspec = 31, 250
        IF (Specname(lpspec).NE.' ') THEN
          IF (Speckflg(lpspec).EQ.0) THEN
            Speclk(lpspec) = Specrk(lpspec) - Spech(lpspec)*dummy1
          ELSE
            Speclk(lpspec) = Spectemp(lpspec,1) + Spectemp(lpspec,2) &
                            *Tempk + Spectemp(lpspec,3) &
                            /Tempk + Spectemp(lpspec,4)*DLOG10(Tempk) &
                            + Spectemp(lpspec,5)/Tempk**2
          END IF
        END IF
   10 CONTINUE
!
!   Convert well units
!
      IF (Unitflag.LT.0 .OR. Unitflag.GT.5) &
          STOP 'Error - bad Units flag.'
!
      c1 = 0.0D0
!
      IF (Unitflag.GE.0 .AND. Unitflag.LE.3) THEN
        DO 20 lpspec = 4, 30
          IF (Specname(lpspec).NE.' ') THEN
            IF (Unitflag.EQ.0) Ppm(lpspec) = Ppm(lpspec)*Spcgfw(lpspec)
            IF (Unitflag.EQ.1) THEN
              IF (Specz(Specinp(lpspec)).EQ.0.0D0) THEN
                Ppm(lpspec) = Ppm(lpspec)*Spcgfw(lpspec)
              ELSE
                Ppm(lpspec) = Ppm(lpspec)*Spcgfw(lpspec) &
                             /DABS(Specz(Specinp(lpspec)))
              END IF
            END IF
            IF (Unitflag.LE.2) Ppm(lpspec) = Ppm(lpspec)/Density
            c1 = c1 + Ppm(lpspec)
          END IF
   20   CONTINUE
        C1save = 1.0D0 - 1.0D-6*c1
        c1 = 1.0D0/C1save
!
        DO 30 lpspec = 4, 30
          IF (Specname(lpspec).NE.' ') THEN
            IF (Spcgfw(lpspec).GT.0.0D0) Totmol(lpspec) = Ppm(lpspec) &
               *c1/(1.0D3*Spcgfw(lpspec))
            IF (Totmol(lpspec).GT.1.0D-40) Logmol(lpspec) &
               = DLOG10(Totmol(lpspec))
            Grams(lpspec) = Ppm(lpspec)*Density
!
!   leave total molality in ppm for charge balance option
!
            Ppm(lpspec) = Totmol(lpspec)
          END IF
   30   CONTINUE
      ELSE
!      unitflag is equalt to 4 (mmol/kg H2O) or 5 (mol/kg)
        IF (Unitflag.EQ.4) THEN
          DO 40 lpspec = 4, 30
            Ppm(lpspec) = Ppm(lpspec)/1.0D3
   40     CONTINUE
        END IF
        DO 60 i = 1, 3
          c2 = 0.0D0
          c1 = 1.0D0 - c1*1.0D-6
          DO 50 lpspec = 4, 30
            IF (Specname(lpspec).NE.' ') THEN
              Totmol(lpspec) = Ppm(lpspec)
              c2 = c2 + Totmol(lpspec)*Spcgfw(lpspec)*1.0D3*c1
            END IF
   50     CONTINUE
          c1 = c2
   60   CONTINUE
        c1 = (1.0D0-c1*1.0D-6)
        C1save = c1
        DO 70 lpspec = 4, 30
          IF (Specname(lpspec).NE.' ') THEN
            Grams(lpspec) = Totmol(lpspec)*1.0D3*Spcgfw(lpspec) &
                           *c1*Density
            IF (Totmol(lpspec).GT.1.0D-40) Logmol(lpspec) &
               = DLOG10(Totmol(lpspec))
          END IF
   70   CONTINUE
      END IF
      IF (ICORT.EQ.1) THEN
!         totpos = 0.0D0
!         totneg = 0.0D0
!         DO 80 lpspec = 4, 30
!            IF (Specname(lpspec).NE.' ') THEN
!               IF (Specz(Specinp(lpspec)).LT.0.0D0) THEN
!                  totneg = totneg-Specz(Specinp(lpspec))*Totmol(lpspec)
!               ELSE
!                  IF (Specz(Specinp(lpspec)).GT.0.0D0) totpos = totpos+ &
!                      Specz(Specinp(lpspec))*Totmol(lpspec)
!               END IF
!            END IF
!   80    CONTINUE
!         IF (totpos+totneg.GT.0) THEN
!            factpos = DSQRT(totneg/totpos)
!            factneg = DSQRT(totpos/totneg)
        factneg = 1.0D0/FACTPOS
        DO 90 lpspec = 4, 30
          IF (Specname(lpspec).NE.' ') THEN
            IF (Specz(Specinp(lpspec)).LT.0.0D0) THEN
              Totmol(lpspec) = Totmol(lpspec)*factneg
            ELSE IF (Specz(Specinp(lpspec)).GT.0D0) THEN
              Totmol(lpspec) = Totmol(lpspec)*FACTPOS
            END IF
          END IF
   90   CONTINUE
!      END IF
        c1 = 0.0D0
        DO 110 i = 1, 3
          c2 = 0.0D0
          c1 = 1.0D0 - c1*1.0D-6
          DO 100 lpspec = 4, 30
            IF (Specname(lpspec).NE.' ') c2 = c2 + Totmol(lpspec) &
                                             *Spcgfw(lpspec)*1.0D3*c1
  100     CONTINUE
          c1 = c2
  110   CONTINUE
        c1 = (1.0D0-c1*1.0D-6)
        C1save = c1
        DO 120 lpspec = 4, 30
          IF (Specname(lpspec).NE.' ') THEN
            Grams(lpspec) = Totmol(lpspec)*1.0D3*Spcgfw(lpspec) &
                           *c1*Density
            IF (Totmol(lpspec).GT.1.0D-40) Logmol(lpspec) &
               = DLOG10(Totmol(lpspec))
          END IF
  120   CONTINUE
      END IF
      Tds = 0.0D0
      DO 130 lpspec = 4, 30
        Mol(lpspec) = Totmol(lpspec)
        Tds = Tds + Grams(lpspec)
  130 CONTINUE
!
      Epmcat = 10.0D0**(0.0D0-Ph)
      Epman = 10.0D0**(Ph-14.0D0)
      DO 140 lpspec = 4, 30
        IF (Specname(lpspec).NE.' ') THEN
          IF (Specz(Specinp(lpspec)).GT.0.0D0) THEN
            Epmcat = Epmcat + Specz(Specinp(lpspec))*Totmol(lpspec)*c1
          ELSE
            Epman = Epman - Specz(Specinp(lpspec))*Totmol(lpspec)*c1
          END IF
        END IF
  140 CONTINUE
      Epmcat = Epmcat*1.0D3
      Epman = Epman*1.0D3
!
!      WRITE (Iw1,9000)
!      WRITE (Iw1,9010)
!      WRITE (Iw1,9015) Tempc, Ph, Epmcat, Epman
!      WRITE (Iw1,9020) 1.0D3*Totmol(Lexcept(13)), 9.9, 9.9, Unitflag, &
!                      Coralk, Pecalc, Idaves, Eh, Pe(1)
!      WRITE (Iw1,9000)
!      WRITE (Iw1,9025)
!      DO 150 lpspec = 4, 30
!        IF (Mol(lpspec).GT.0.0D0) WRITE (Iw1,9030) &
!                                      Specname(Specinp(lpspec)), &
!                                      Specz(Specinp(lpspec)), &
!                                      Mol(lpspec), Logmol(lpspec), &
!                                      Grams(lpspec)
!  150 CONTINUE
!      WRITE (Iw1,9000)
!      WRITE (Iw1,9000)
!
!   Find temp-dependent Debye-Huckel constants
!
      dummy1 = 374.11D0 - Tempc
      dummy2 = dummy1**(1.0D0/3.0D0)
      dummy3 = SQRT((1.0D0+0.1342489D0*dummy2-(3.946263D-3*dummy1)) &
              /(3.1975D0-.3151548D0*dummy2-1.203374D-3*dummy1+ &
              7.48908D-13*dummy1**4.0D0))
      IF (Tempk.LT.373.15D0) THEN
        dummy4 = 87.74D0 - Tempc*(Tempc*(1.41D-6*Tempc-9.398D-4) &
                +.4008D0)
      ELSE
        dummy4 = 5321.D0/Tempk + 233.76D0 -  &
                Tempk*(Tempk*(8.292D-7*Tempk-1.417D-3)+.9297D0)
      END IF
      dummy4 = SQRT(dummy4*Tempk)
      Consta = 18246.D2*dummy3/dummy4**3D0
      Constb = 50.29D0*dummy3/dummy4
!
      RETURN
!
 9000 FORMAT (//)
 9005 FORMAT ('1',(A80),//)
 9010 FORMAT (30X,'----------------',/,30X,'INITIAL SOLUTION',/,30X, &
             '----------------',///)
 9015 FORMAT (10X,'TEMPERATURE = ',F6.2,' DEGREES C    PH = ',F6.3,/, &
             10X,'ANALYTICAL EPMCAT = ',F8.3,'    ANALYTICAL EPMAN = ', &
             F8.3,//)
 9020 FORMAT (5X,'***** OXIDATION - REDUCTION *****',///,1X, &
             'DISSOLVED OXYGEN = ',F6.3,' MMOLES/KG H2O',/,1X, &
             'EH MEASURED WITH CALOMEL = ',F7.4,' VOLTS',9X, &
             'FLAG CORALK PECALC IDAVES',/,1X, &
             'MEASURED EH OF ZOBELL SOLUTION = ',F7.4,' VOLTS',5X,I1, &
             4X,I1,6X,I1,6X,I1,/,1X,'CORRECTED EH = ',F7.4,' VOLTS',/, &
             1X,'PE COMPUTED FROM CORRECTED EH = ',F7.3,/)
 9025 FORMAT (15X,'*** TOTAL CONCENTRATIONS OF INPUT SPECIES ***',//, &
             25X,'TOTAL',13X,'LOG TOTAL',12X,'TOTAL',/,8X,'SPECIES',8X, &
             'MOLALITY',12X,'MOLALITY',11X,'MG/LITRE',/,8X,'-------', &
             8X,'--------',12X,'--------',11X,'--------',/)
 9030 FORMAT (8X,A8,F5.1,1X,1PE12.5,9x,0PF9.4,8X,1PE12.5)
      END
!
!
!
      SUBROUTINE MAKESPLIST
      LOGICAL carbon
      INTEGER lpspec, lplist, ispec, inext
!
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
!
      DO 10 lpspec = 1, 30
        Speclist(lpspec,0) = 0
   10 CONTINUE
      Lcalk(0) = 0
      Lnoncalk(0) = 0
      DO 30 lplist = 1, 250
        IF (Specname(lplist).NE.' ') THEN
          IF (lplist.GT.30) THEN
!         For each master species in the defining reaction for this
!         species, add this species to the master species' list of
!         dependent reactions.
            carbon = .FALSE.
            DO 20 lpspec = 1, Nspecspec(lplist)
              ispec = Specspec(lplist,lpspec)
              inext = Speclist(ispec,0) + 1
              Speclist(ispec,0) = inext
              Speclist(ispec,inext) = lplist
!            Check for carbon, in case there is specalk
              IF (ispec.EQ.Lexcept(1)) carbon = .TRUE.
   20       CONTINUE
          END IF
          IF (Specalk(lplist).NE.0.0) THEN
            IF (lplist.LE.30) THEN
              IF (lplist.EQ.Lexcept(1)) THEN
                Lcalk(0) = Lcalk(0) + 1
                Lcalk(Lcalk(0)) = lplist
              ELSE
                Lnoncalk(0) = Lnoncalk(0) + 1
                Lnoncalk(Lnoncalk(0)) = lplist
              END IF
            ELSE IF (carbon) THEN
              Lcalk(0) = Lcalk(0) + 1
              Lcalk(Lcalk(0)) = lplist
            ELSE
              Lnoncalk(0) = Lnoncalk(0) + 1
              Lnoncalk(Lnoncalk(0)) = lplist
            END IF
          END IF
        END IF
   30 CONTINUE
      RETURN
      END
!
!
!
      SUBROUTINE OUTWRITE
      INTEGER lpspec, lplist
      DOUBLE PRECISION elect, calccat, calcan, pco2, logpco2, po2, pch4,  &
                      co2tot, sumalk, sumcalk
      REAL cimb
!
      INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      CHARACTER*80 Wellname
      INTEGER Coralk, Unitflag, Pecalc, Idaves
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMCHAR/ Wellname
      COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
!
      EXTERNAL CALCSIS
      INTRINSIC DLOG10
!
      elect = 0.0D0
      calccat = 0.0D0
      calcan = 0.0D0
      DO 10 lpspec = 1, 250
        IF (Specname(lpspec).NE.' ') THEN
          IF (lpspec.NE.2 .AND. lpspec.NE.3) THEN
            elect = elect + Specz(lpspec)*Mol(lpspec)
            IF (Specz(lpspec).GT.0.0D0) THEN
              calccat = calccat + Specz(lpspec)*Mol(lpspec)*C1save
            ELSE
              calcan = calcan - Specz(lpspec)*Mol(lpspec)*C1save
            END IF
          END IF
        END IF
   10 CONTINUE
      calcan = calcan*1.0D3
      calccat = calccat*1.0D3
      cimb = 100.0D0*(calccat-calcan)/(calccat+calcan)
      elect = elect*1.0D3
      co2tot = Mol(Lexcept(1))
      DO 20 lplist = 1, Speclist(Lexcept(1),0)
        co2tot = co2tot + Mol(Speclist(Lexcept(1),lplist))
   20 CONTINUE
!
      sumalk = 0.0D0
      sumcalk = 0.0D0
      DO 30 lpspec = 1, Lcalk(0)
        sumalk = sumalk + Mol(Lcalk(lpspec))*Specalk(Lcalk(lpspec))
        sumcalk = sumcalk + Mol(Lcalk(lpspec))*Specalk(Lcalk(lpspec))
   30 CONTINUE
      DO 40 lpspec = 1, Lnoncalk(0)
        sumalk= sumalk + Mol(Lnoncalk(lpspec))*Specalk(Lnoncalk(lpspec))
   40 CONTINUE
!
      pco2 = Alpha(Lexcept(4)) &
            /10.0D0**(108.3865D0+.01985076D0*Tempk-6919.53D0/Tempk- &
            40.45154D0*DLOG10(Tempk)+669365.0D0/(Tempk**2))
      logpco2 = 0.0D0
      IF (pco2.GT.1.0D-40) logpco2 = DLOG10(pco2)
      po2 = Alpha(Lexcept(9)) &
           /10.0D0**(-2.96D0+1.844D0*(298.15D0-Tempk)/298.15D0*Tempk &
           *Constc*Constr)
      pch4 = 0.0D0
      IF (Alpha(Lexcept(3)).GT.1.0D-40) &
         pch4 = 10.0D0**((30.741D0+57.435D0*(298.15D0-Tempk)/(298.15D0 &
                *Tempk*Constc*Constr))-8.0D0*Pe(Pecalc)-9.0D0*Ph-3.0D0 &
                *DLOG10(Alpha(3))+DLOG10(Alpha(Lexcept(3))))
!
      WRITE (Iw1,9000) Wellname
      WRITE (Iw1,9005)
      WRITE (Iw1,9005)
      WRITE (Iw1,9010) Alpha(3), Epmcat, calccat, Ph, pco2, Epman,  &
                      calcan, logpco2, po2, Eh, Pe(Pecalc), Tempc,  &
                      pch4, Pe(4), co2tot, Pe(2), Density, Pe(3), Xmu,  &
                      Tds, sumalk*1.0D3, sumcalk*1.0D3, elect, cimb
      WRITE (Iw1,9015) Pe(Pecalc), Pe(Pecalc)*Constc*Constr*Tempk/Constf
      WRITE (Iw1,9020)
!
      DO 50 lpspec = 1, 250
        IF (Specname(lpspec).NE.' ' .AND. Mol(lpspec).GE.1.0D-38) &
           WRITE (Iw1,9025) lpspec, Specname(lpspec), Specz(lpspec),  &
                            Mol(lpspec), Alpha(lpspec),  &
                            DLOG10(Alpha(lpspec)), Gamma(lpspec)
   50 CONTINUE
!
      CALL CALCSIS
!
      RETURN
!
 9000 FORMAT (/,1X,A80)
 9005 FORMAT (//)
 9010 FORMAT (//,22X,'****DESCRIPTION OF SOLUTION ****',//,10X, &
             'ANALYT.   COMP.',9X,'PH',11X,'ACTIVITY H2O = ',F7.4,/, &
             ' EPMCAT ',F8.2,F10.2,6X,F6.3,9X,'PCO2= ',1PE13.6,/, &
             ' EPMAN  ',0PF8.2,F10.2,21X,'LOG PCO2 = ',F8.4,/,30X, &
             'TEMPERATURE',6X,'PO2 = ',1PE13.6,/,1X,'EH = ',0PF6.4,2X, &
             'PE = ',F7.3,4X,F6.2,' DEG C',5X,'PCH4 = ',1PE13.6,/,1X, &
             'PE CALC S = ',0PF8.3,26X,'CO2 TOT = ',1PE13.6,/,1X, &
             'PE CALC DOX=',0PF7.3,9X,'IONIC STRENGTH',4X,'DENSITY = ', &
             F8.4,/,1X,'PE SATO DOX=',F7.3,9X,1PE13.6,5X,'TDS = ', &
             0PF9.1,'MG/L',/,1X,'TOT ALK = ',1PE10.3,' MEQ',22X, &
             'CARB ALK',' = ',E10.3,' MEQ',/,1X,'ELECT   = ',e10.3, &
             ' MEQ',22X,'CHARGE IMBALANCE = ',0PF6.1,'%'/)
 9015 FORMAT (' IN COMPUTING THE DISTRIBUTION OF SPECIES,'/,' PE = ', &
             F7.3,5X,'EQUIVALENT EH =',F7.3,'VOLTS',//)
 9020 FORMAT (///,25X,'-----------------------',/,25X, &
             'DISTRIBUTION OF SPECIES',/,25X,'-----------------------', &
             //,2X,'I',2X,'SPECIES',9X,'MOLALITY',3X,'ACTIVITY',4X, &
             'LOG ACT',4X,'GAMMA',/)
 9025 FORMAT (1X,I3,1X,A8,F3.0,3X,1PE11.4,1X,1PE11.4,1X,0PF7.3,1X, &
             1PE11.4)
      END
!
!
!
      SUBROUTINE PATWRITE(WELLNUM,ERROR)
      INTEGER lpspec, lplist, WELLNUM, lpinner, list, trans(50),  &
             trsfg(50), i, j
      LOGICAL flagh, ERROR
      DOUBLE PRECISION patmol, patrs, patiso, mco3, mhco3, mh2co3, rs,  &
                      rscoef
      CHARACTER*1 sfg(50), UPCS
      CHARACTER*2 name
      CHARACTER*9 fmt
      CHARACTER*80 line
      REAL Dbdata(50,45), valu, aaa(50), a2, a3, a6, a7
      INTEGER Dbsfg(50,45), Nwlls, Totwell, Tot(50), Idefault(5),  &
             Iu(50,4)
      COMMON /DB    / Dbdata
      COMMON /INT4DB/ Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
      INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      CHARACTER*80 Wellname
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMCHAR/ Wellname
!
      EXTERNAL UPCS
      INTRINSIC DABS, ABS
!
      DATA trans/20, 21, 22, 3, 5, 6, 7, 16, 17, 4, 13, 15, 10, 8, 1, 2,  &
          18, 12, 19, 9, 14, 11, 23, 24, 25, 37, 38, 25, 29, 30, 31,  &
          32, 33, 34, 35, 36, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47,  &
          48, 0, 0, 0/
      DATA trsfg/4, 13, 7, 37, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19,  &
          20, 21, 22, 23, 25, 1, 6, 13, 41, 40, 3, 4, 2, 4, 4, 4, 21,  &
          21, 22, 22, 22, 22, 35, 26, 26, 0, 29, 30, 36, 33, 34, 5, 31,  &
          32, 0, 0/
      WRITE (Iw2,9005) Wellname
      DO 10 lplist = 1, 50
        sfg(lplist) = '*'
        aaa(lplist) = 0.0
   10 CONTINUE
      DO 60 lpspec = 4, 30
        IF (Specname(lpspec).NE.' ') THEN
          name(1:1) = UPCS(Totname(lpspec)(1:1))
          name(2:2) = UPCS(Totname(lpspec)(2:2))
!
!    NO3
          IF (lpspec.EQ.Lexcept(7)) THEN
            patmol = Totmol(Lexcept(7)) + Totmol(Lexcept(8)) &
                    + Totmol(Lexcept(10))
            aaa(trans(17)) = patmol*1.0D3
            patrs = Totmol(Lexcept(7))*Specthsp(Lexcept(7))
            patrs = patrs + Totmol(Lexcept(8))*Specthsp(Lexcept(8))
            patrs = patrs + Totmol(Lexcept(10))*Specthsp(Lexcept(10))
            IF (patmol.GT.0.0D0) patrs = patrs/patmol
            aaa(trans(24)) = patrs
            patiso = Totmol(Lexcept(7))*Dbdata(WELLNUM,40)
            patiso = patiso + Totmol(Lexcept(8))*Dbdata(WELLNUM,41)
            patiso = patiso + Totmol(Lexcept(10))*Dbdata(WELLNUM,39)
            IF (patmol.GT.0.0D0) patiso = patiso/patmol
            aaa(trans(23)) = patiso
!
!    Other Nitrogen
          ELSE IF (lpspec.EQ.Lexcept(8) .OR. lpspec.EQ.Lexcept(10)) THEN
!       do nothing, these are handled in NO3 case.
!
!    SO4
          ELSE IF (lpspec.EQ.Lexcept(6)) THEN
            patmol = Totmol(Lexcept(6)) + Totmol(Lexcept(5))
            aaa(trans(16)) = patmol*1.0D3
            aaa(trans(3)) = Totmol(Lexcept(6))*1.0D3
            aaa(trans(2)) = Totmol(Lexcept(5))*1.0D3
            aaa(trans(46)) = Dbdata(WELLNUM,31)
            aaa(trans(47)) = Dbdata(WELLNUM,32)
!
!    Other Sulfur
          ELSE IF (lpspec.EQ.Lexcept(5)) THEN
!      do nothing, this is handled in SO4 case.
!
!    Iron or Manganese
          ELSE IF (name.EQ.'FE' .OR. name.EQ.'MN') THEN
            aaa(trans(lpspec)) = Totmol(lpspec)*1.0D3
            a2 = Mol(lpspec)
            a3 = 0.0
            a6 = 0.0
            a7 = 0.0
            DO 30 lplist = 1, Speclist(lpspec,0)
              list = Speclist(lpspec,lplist)
              rs = Specthsp(list)
              DO 20 lpinner = 1, Nspecspec(list)
                IF (Specspec(list,lpinner).EQ.lpspec) THEN
                  rscoef = Speccoef(list,lpinner)
                ELSE
                  rs = rs - Speccoef(list,lpinner) &
                      *Specthsp(Specspec(list,lpinner))
                END IF
   20         CONTINUE
              rs = rs/rscoef
              IF (DABS(rs-3.0D0).LT.1.0D-3) THEN
                a3 = a3 + Mol(list)*rscoef
              ELSE IF (DABS(rs-6.0D0).LT.1.0D-3) THEN
                a6 = a6 + Mol(list)*rscoef
              ELSE IF (DABS(rs-7.0D0).LT.1.0D-3) THEN
                a7 = a7 + Mol(list)*rscoef
              ELSE
                a2 = a2 + Mol(list)*rscoef
              END IF
   30       CONTINUE
            IF (name.EQ.'FE') THEN
              aaa(trans(31)) = a2*1.0E3
              aaa(trans(32)) = a3*1.0E3
            ELSE
              aaa(trans(33)) = a2*1.0E3
              aaa(trans(34)) = a3*1.0E3
              aaa(trans(35)) = a6*1.0E3
              aaa(trans(36)) = a7*1.0E3
            END IF
!
!    Carbon
          ELSE IF (lpspec.EQ.Lexcept(1)) THEN
            mco3 = Mol(Lexcept(2))
            mhco3 = 0.0D0
            mh2co3 = 0.0D0
            DO 50 lplist = 1, Speclist(lpspec,0)
              list = Speclist(lpspec,lplist)
              IF (list.EQ.Lexcept(4)) THEN
                mh2co3 = Mol(list)
              ELSE
                flagh = .FALSE.
                DO 40 lpinner = 1, Nspecspec(list)
                  IF (Specspec(list,lpinner).EQ.1) flagh = .TRUE.
                  IF (Specspec(list,lpinner).EQ.Lexcept(1)) &
                     rscoef = Speccoef(list,lpinner)
   40           CONTINUE
                IF (flagh) THEN
                  mhco3 = mhco3 + Mol(list)*rscoef
                ELSE
                  mco3 = mco3 + Mol(list)*rscoef
                END IF
              END IF
   50       CONTINUE
            patmol = mh2co3 + mhco3 + mco3 + Mol(Lexcept(11)) &
                    + Mol(Lexcept(12))
            aaa(trans(15)) = patmol*1.0D3
            aaa(trans(40)) = Dbdata(WELLNUM,29)
            aaa(trans(41)) = Dbdata(WELLNUM,30)
            aaa(29) = mco3*1.0D3
            aaa(26) = mhco3*1.0D3
            aaa(28) = mh2co3*1.0D3
!         RS of DOC
            aaa(trans(39)) = Dbdata(WELLNUM,45)
!    The Rest
          ELSE
            aaa(trans(lpspec)) = Totmol(lpspec)*1.0D3
          END IF
        END IF
   60 CONTINUE
      aaa(trans(1)) = Tempc
      aaa(27) = Ph
      aaa(trans(30)) = aaa(26) + aaa(28) + aaa(29)
      aaa(trans(42)) = Dbdata(WELLNUM,36)
      aaa(trans(43)) = Dbdata(WELLNUM,33)
      aaa(trans(44)) = Dbdata(WELLNUM,34)
      aaa(trans(45)) = Dbdata(WELLNUM,5)
!
! =======================
! significant figures (defined or not, mostly)
      DO 70 lplist = 1, 48
        IF (trsfg(lplist).NE.0) THEN
          IF (Dbsfg(WELLNUM,trsfg(lplist)).NE.-1) sfg(lplist) = ' '
        END IF
   70 CONTINUE
      IF (Dbsfg(WELLNUM,23).NE.-1 .OR. Dbsfg(WELLNUM,24).NE.-1 .OR.  &
         Dbsfg(WELLNUM,38).NE.-1) THEN
        sfg(18) = ' '
        sfg(24) = ' '
        IF (Dbsfg(WELLNUM,39).NE.-1 .OR. Dbsfg(WELLNUM,40).NE.-1 .OR.  &
           Dbsfg(WELLNUM,41).NE.-1) sfg(23) = ' '
      END IF
!
      DO 100 i = 0, 5
        line = ' '
        DO 90 j = 1, 8
          valu = aaa(i*8+j)
          IF (ABS(valu).LT.1.0E-5) aaa(i*8+j) = 0.0
          IF ((i*8+j.EQ.26.OR.i*8+j.EQ.28.OR.i*8+j.EQ.29) .AND.  &
             valu.LT.0.0) THEN
            WRITE (*,9000) Wellname
            READ (*,9005) line
            ERROR = .TRUE.
            CLOSE (UNIT=Iw2,STATUS='DELETE',ERR=80)
   80       RETURN
          END IF
          fmt = '(F8.5,A1)'
          IF (valu.GT.99 .OR. valu.LT.-9.9) fmt = '(F8.4,A1)'
          IF (valu.GT.999 .OR. valu.LT.-99.9) fmt = '(F8.3,A1)'
          IF (valu.GT.9999 .OR. valu.LT.-999.9) fmt = '(F8.2,A1)'
          IF (valu.GT.99999 .OR. valu.LT.-9999.9) fmt = '(F8.1,A1)'
          IF (valu.GT.999999 .OR. valu.LT.-99999.9) fmt = '(F8.0,A1)'
          WRITE (line(j*9-8:j*9),fmt) aaa(i*8+j), sfg(i*8+j)
   90   CONTINUE
        IF (i.EQ.5) WRITE (line(77:79),'(i3)') Tot(WELLNUM)
        WRITE (Iw2,9005) line
  100 CONTINUE
!
      RETURN
 9000 FORMAT (//,' The Carbon data for well ''',A,'''',/, &
             ' is invalid (less than zero.)',//, &
      ' Please check the data, or if necessary, enter the total carbon' &
      ,/,' value for this well.',//, &
      ' Hit <Enter> to return to the main DB screen. (.PAT file', &
      ' not written)')
 9005 FORMAT (A)
      END
!
!
!
      SUBROUTINE PATWRITE214(WELLNUM,ERROR)
      IMPLICIT NONE
      INTEGER lpspec, lplist, WELLNUM, lpinner, list, trans(50),  &
             trsfg(50), i, j
      LOGICAL flagh, ERROR
      DOUBLE PRECISION patmol, patrs, patiso, mco3, mhco3, mh2co3, rs,  &
                      rscoef
      CHARACTER*1 sfg(50), UPCS
      CHARACTER*2 name
      CHARACTER*32 fmt
      CHARACTER*80 line
      INTEGER k
      REAL Dbdata(50,45), valu, aaa(50), a2, a3, a6, a7
      INTEGER Dbsfg(50,45), Nwlls, Totwell, Tot(50), Idefault(5),  &
             Iu(50,4)
      COMMON /DB    / Dbdata
      COMMON /INT4DB/ Dbsfg, Idefault, Iu, Nwlls, Totwell, Tot
      INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      CHARACTER*80 Wellname
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMCHAR/ Wellname
!
      EXTERNAL UPCS
      INTRINSIC DABS, ABS
!
      DATA trans/20, 21, 22, 3, 5, 6, 7, 16, 17, 4, 13, 15, 10, 8, 1, 2,  &
          18, 12, 19, 9, 14, 11, 23, 24, 25, 37, 38, 25, 29, 30, 31,  &
          32, 33, 34, 35, 36, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47,  &
          48, 0, 0, 0/
      DATA trsfg/4, 13, 7, 37, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19,  &
          20, 21, 22, 23, 25, 1, 6, 13, 41, 40, 3, 4, 2, 4, 4, 4, 21,  &
          21, 22, 22, 22, 22, 35, 26, 26, 0, 29, 30, 36, 33, 34, 5, 31,  &
          32, 0, 0/
      CHARACTER*16 words(48)
      DATA words/ &
          'C         ','S         ','CA        ','AL        ', &
          'MG        ','NA        ','K         ','CL        ', &
          'F         ','SI        ','BR        ','B         ', &
          'BA        ','LI        ','SR        ','FE        ', &
          'MN        ','N         ','P         ','TEMP      ', &
          'H2S       ','SO4       ','N15       ','RS of N   ', &
          'DOX       ','HCO3      ','PH        ','H2CO3*    ', &
          'CO3       ','CARBONATES','FE2+      ','FE3+      ', &
          'MN2+      ','MN3+      ','MN6+      ','MN7+      ', &
          'CH4       ','DOC       ','RS OF DOC ','BLANK     ', &
          'C13       ','C14       ','SR87      ','D         ', &
          'O-18      ','TRITIUM   ','34SSO4    ','34SH2S    '/
      WRITE (Iw2,9005) Wellname
      DO 10 lplist = 1, 50
        sfg(lplist) = '*'
        aaa(lplist) = 0.0
   10 CONTINUE
      DO 60 lpspec = 4, 30
        IF (Specname(lpspec).NE.' ') THEN
          name(1:1) = UPCS(Totname(lpspec)(1:1))
          name(2:2) = UPCS(Totname(lpspec)(2:2))
!
!    NO3
          IF (lpspec.EQ.Lexcept(7)) THEN
            patmol = Totmol(Lexcept(7)) + Totmol(Lexcept(8)) &
                    + Totmol(Lexcept(10))
            aaa(trans(17)) = patmol*1.0D3
            patrs = Totmol(Lexcept(7))*Specthsp(Lexcept(7))
            patrs = patrs + Totmol(Lexcept(8))*Specthsp(Lexcept(8))
            patrs = patrs + Totmol(Lexcept(10))*Specthsp(Lexcept(10))
            IF (patmol.GT.0.0D0) patrs = patrs/patmol
            aaa(trans(24)) = patrs
            patiso = Totmol(Lexcept(7))*Dbdata(WELLNUM,40)
            patiso = patiso + Totmol(Lexcept(8))*Dbdata(WELLNUM,41)
            patiso = patiso + Totmol(Lexcept(10))*Dbdata(WELLNUM,39)
            IF (patmol.GT.0.0D0) patiso = patiso/patmol
            aaa(trans(23)) = patiso
!
!    Other Nitrogen
          ELSE IF (lpspec.EQ.Lexcept(8) .OR. lpspec.EQ.Lexcept(10)) THEN
!       do nothing, these are handled in NO3 case.
!
!    SO4
          ELSE IF (lpspec.EQ.Lexcept(6)) THEN
            patmol = Totmol(Lexcept(6)) + Totmol(Lexcept(5))
            aaa(trans(16)) = patmol*1.0D3
            aaa(trans(3)) = Totmol(Lexcept(6))*1.0D3
            aaa(trans(2)) = Totmol(Lexcept(5))*1.0D3
            aaa(trans(46)) = Dbdata(WELLNUM,31)
            aaa(trans(47)) = Dbdata(WELLNUM,32)
!
!    Other Sulfur
          ELSE IF (lpspec.EQ.Lexcept(5)) THEN
!      do nothing, this is handled in SO4 case.
!
!    Iron or Manganese
          ELSE IF (name.EQ.'FE' .OR. name.EQ.'MN') THEN
            aaa(trans(lpspec)) = Totmol(lpspec)*1.0D3
            a2 = Mol(lpspec)
            a3 = 0.0
            a6 = 0.0
            a7 = 0.0
            DO 30 lplist = 1, Speclist(lpspec,0)
              list = Speclist(lpspec,lplist)
              rs = Specthsp(list)
              DO 20 lpinner = 1, Nspecspec(list)
                IF (Specspec(list,lpinner).EQ.lpspec) THEN
                  rscoef = Speccoef(list,lpinner)
                ELSE
                  rs = rs - Speccoef(list,lpinner) &
                      *Specthsp(Specspec(list,lpinner))
                END IF
   20         CONTINUE
              rs = rs/rscoef
              IF (DABS(rs-3.0D0).LT.1.0D-3) THEN
                a3 = a3 + Mol(list)*rscoef
              ELSE IF (DABS(rs-6.0D0).LT.1.0D-3) THEN
                a6 = a6 + Mol(list)*rscoef
              ELSE IF (DABS(rs-7.0D0).LT.1.0D-3) THEN
                a7 = a7 + Mol(list)*rscoef
              ELSE
                a2 = a2 + Mol(list)*rscoef
              END IF
   30       CONTINUE
            IF (name.EQ.'FE') THEN
              aaa(trans(31)) = a2*1.0E3
              aaa(trans(32)) = a3*1.0E3
            ELSE
              aaa(trans(33)) = a2*1.0E3
              aaa(trans(34)) = a3*1.0E3
              aaa(trans(35)) = a6*1.0E3
              aaa(trans(36)) = a7*1.0E3
            END IF
!
!    Carbon
          ELSE IF (lpspec.EQ.Lexcept(1)) THEN
            mco3 = Mol(Lexcept(2))
            mhco3 = 0.0D0
            mh2co3 = 0.0D0
            DO 50 lplist = 1, Speclist(lpspec,0)
              list = Speclist(lpspec,lplist)
              IF (list.EQ.Lexcept(4)) THEN
                mh2co3 = Mol(list)
              ELSE
                flagh = .FALSE.
                DO 40 lpinner = 1, Nspecspec(list)
                  IF (Specspec(list,lpinner).EQ.1) flagh = .TRUE.
                  IF (Specspec(list,lpinner).EQ.Lexcept(1)) &
                     rscoef = Speccoef(list,lpinner)
   40           CONTINUE
                IF (flagh) THEN
                  mhco3 = mhco3 + Mol(list)*rscoef
                ELSE
                  mco3 = mco3 + Mol(list)*rscoef
                END IF
              END IF
   50       CONTINUE
            patmol = mh2co3 + mhco3 + mco3 + Mol(Lexcept(11)) &
                    + Mol(Lexcept(12))
            aaa(trans(15)) = patmol*1.0D3
            aaa(trans(40)) = Dbdata(WELLNUM,29)
            aaa(trans(41)) = Dbdata(WELLNUM,30)
            aaa(29) = mco3*1.0D3
            aaa(26) = mhco3*1.0D3
            aaa(28) = mh2co3*1.0D3
!         RS of DOC
            aaa(trans(39)) = Dbdata(WELLNUM,45)
!    The Rest
          ELSE
            aaa(trans(lpspec)) = Totmol(lpspec)*1.0D3
          END IF
        END IF
   60 CONTINUE
      aaa(trans(1)) = Tempc
      aaa(27) = Ph
      aaa(trans(30)) = aaa(26) + aaa(28) + aaa(29)
      aaa(trans(42)) = Dbdata(WELLNUM,36)
      aaa(trans(43)) = Dbdata(WELLNUM,33)
      aaa(trans(44)) = Dbdata(WELLNUM,34)
      aaa(trans(45)) = Dbdata(WELLNUM,5)
!
! =======================
! significant figures (defined or not, mostly)
      DO 70 lplist = 1, 48
        IF (trsfg(lplist).NE.0) THEN
          IF (Dbsfg(WELLNUM,trsfg(lplist)).NE.-1) sfg(lplist) = ' '
        END IF
   70 CONTINUE
      IF (Dbsfg(WELLNUM,23).NE.-1 .OR. Dbsfg(WELLNUM,24).NE.-1 .OR.  &
         Dbsfg(WELLNUM,38).NE.-1) THEN
        sfg(18) = ' '
        sfg(24) = ' '
        IF (Dbsfg(WELLNUM,39).NE.-1 .OR. Dbsfg(WELLNUM,40).NE.-1 .OR.  &
           Dbsfg(WELLNUM,41).NE.-1) sfg(23) = ' '
      END IF
!
      do k = 1, 48
         valu = aaa(k)
         IF (ABS(valu).LT.1.0E-6) aaa(k) = 0.0
         IF ((k.EQ.26.OR.k.EQ.28.OR.k.EQ.29) .AND.  &
             valu.LT.0.0) THEN
            WRITE (*,9000) Wellname
            READ (*,9005) line
            ERROR = .TRUE.
            CLOSE (UNIT=Iw2,STATUS='DELETE',ERR=80)
   80       RETURN
         END IF
         fmt = '(F14.5,A1,T20,"# ",A)'
         IF (valu.GT.99 .OR. valu.LT.-9.9)  &
             fmt = '(F14.4,A1,T20,"# ",A)'
         IF (valu.GT.999 .OR. valu.LT.-99.9)  &
             fmt = '(F14.3,A1,T20,"# ",A)'
         IF (valu.GT.9999 .OR. valu.LT.-999.9)  &
             fmt = '(F14.2,A1,T20,"# ",A)'
         IF (valu.GT.99999 .OR. valu.LT.-9999.9)  &
             fmt = '(F14.1,A1,T20,"# ",A)'
         IF (valu.GT.999999 .OR. valu.LT.-99999.9)  &
             fmt = '(F14.0,A1,T20,"# ",A)'
         IF (k .eq. 43)  &
             fmt = '(F14.8,A1,T20,"# ",A)'
         WRITE (Iw2,fmt) aaa(k), sfg(k), words(k)
      enddo
      WRITE (Iw2,'(i15,T20,"# Well number")') Tot(WELLNUM)
!
      RETURN
 9000 FORMAT (//,' The Carbon data for well ''',A,'''',/, &
             ' is invalid (less than zero.)',//, &
      ' Please check the data, or if necessary, enter the total carbon' &
      ,/,' value for this well.',//, &
      ' Hit <Enter> to return to the main DB screen. (.PAT file', &
      ' not written)')
 9005 FORMAT (A)
      END
!
!
!
      SUBROUTINE RDEXCEPT(IR1)
      CHARACTER*80 line
      INTEGER i, IR1, iloop
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
!
      Numexcept = 14
!   lexcept: 1=Carbon 2=CO3 3=HCO3 4=H2CO3 5=HS 6=SO4 7=NO3 8=NH4 9=O2 10=N2
!            11=CH4 12=DOC 13=DOX(entered) 14=O2(computed)
      DO 10 i = 1, Numexcept
        Lexcept(i) = 0
   10 CONTINUE
      READ (IR1,9000,ERR=20) (Lexcept(iloop),iloop=1,Numexcept)
      READ (IR1,9010,ERR=20,END=20) line
      IF (line.EQ.' ') RETURN
   20 WRITE (*,9005) line
      STOP
 9000 FORMAT (20I4)
 9005 FORMAT (' Error in SPECIAL portion of datafile:',/,1X,A)
 9010 FORMAT (A)
      END
!
!
!
      SUBROUTINE READELEM(IR1)
      INTEGER IR1, index
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
!
      CHARACTER*80 line
!
   10 READ (IR1,9000,ERR=20,END=20) line
      IF (line.EQ.' ') RETURN
      READ (line,9010,ERR=20) index
      IF (index.GE.4 .AND. index.LE.30) THEN
        READ (line,9005,ERR=20) Totname(index), Spcgfw(index),  &
                               Specinp(index)
        GOTO 10
      END IF
   20 WRITE (*,9015) line
      STOP
 9000 FORMAT (A80)
 9005 FORMAT (A2,13X,F10.0,1X,I2)
 9010 FORMAT (10X,I2)
 9015 FORMAT (' Error in datafile:',/,1X,A)
      END
!
!
!
      SUBROUTINE READFILE(silent)
      USE IFWIN
      USE filenames
      IMPLICIT none
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      CHARACTER*MAX_PATH datafile, exename, buffer
      INTEGER(LPSTR) filepart
      INTEGER i
      CHARACTER*8 card(5), line
      INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      LOGICAL batch, opened
      CHARACTER*10 stat
      CHARACTER*80 query, UPCS80
      INTEGER LENS
      EXTERNAL LENS, UPCS80
      DATA card/'ELEMENTS', 'SPECIES ', 'LOOK MIN', 'SPECIAL ',  &
          'END     '/
      logical silent
!
!!!!! DATA VALUES NEED TO BE ZEROED OUT BEFORE ANY READING IS DONE.
!
      Numreacs = 0
!
!     READ FILE NAMES FROM FILE FNAMES.FIL
!     IF DOES NOT EXIST, PROCEED INTERACTIVELY
!
      INQUIRE (FILE='FNAMES.FIL',EXIST=batch)
      IF (batch) THEN
        OPEN (UNIT=Iscr2,FILE='FNAMES.FIL',STATUS='old')
        opened = .TRUE.
      ELSE
        opened = .FALSE.
      END IF
!
!                               DATA FILE FOR WATEQFP
!
    if (.not. silent) then 
    
        ! Get name of executable
        i = GetModuleFileName (GetModuleHandle(NULL_CHARACTER), exename, MAX_PATH)

        ! Strip netpathxl.exe off end
        i = index(exename, "\", .TRUE.)
        datafile = exename(1:i) // "..\database\db.dat" // char(0)
        i = GetFullPathName(datafile, MAX_PATH, buffer, filepart)
        datafile = buffer
        
        stat = 'old'
        query = 'Enter name of db thermodynamic data file. '
        !datafile = 'c:/programs/netpathxl/database/db.dat'
        CALL OPENIT(query,datafile,Ir,stat,batch,Iscr2)
        excel_wateq_filename = datafile
    ELSE
        WRITE(*,*) "Using wateq data file: ", excel_wateq_filename(1:lens(excel_wateq_filename))
        OPEN (UNIT=Ir,FILE=excel_wateq_filename,STATUS=STAT,ERR=20)       
    ENDIF
    IF (opened) CLOSE (Iscr2)
!
!   Loop to read keyword data blocks
!
   10 READ (Ir,9000,ERR=20,END=20) line
      line = UPCS80(line)
      IF (line.EQ.card(1)) THEN
        CALL READELEM(Ir)
      ELSE IF (line.EQ.card(2)) THEN
        CALL READSPEC(Ir)
      ELSE IF (line.EQ.card(3)) THEN
        CALL READREAC(Ir)
      ELSE IF (line.EQ.card(4)) THEN
        CALL RDEXCEPT(Ir)
      ELSE IF (line.EQ.card(5)) THEN
        CALL MAKESPLIST
        CLOSE (Ir)
        RETURN
      END IF
      GOTO 10
!
   20 WRITE (*,9005) datafile(1:LENS(datafile)), line
      STOP
 9000 FORMAT (A8)
 9005 FORMAT (' Error in file: ',A,', text is: ',A)
      END
!
!
!
      SUBROUTINE READREAC(IR1)
      INTEGER IR1, i, j
      CHARACTER*80 line
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
!
   10 READ (IR1,9020,ERR=20,END=20) line
      IF (line.EQ.' ') RETURN
      Numreacs = Numreacs + 1
      i = Numreacs
!
      READ (line,9000,ERR=20,END=20) Reacname(i), Nreacspec(i),  &
                                    Reacopv(i), Reack(i), Reach(i),  &
                                    Reacmflg(i), Reacsi(i)
      IF (Nreacspec(i).LE.10 .AND. Nreacspec(i).GE.1) THEN
!
!   The next statement should read either one or two lines, depending on
!   whether nreacspec is greater than 5 or not.  It is standard fortran,
!   but may cause problems, and need to be split into two lines.
        READ (IR1,9005,ERR=30,END=30) (Reacspec(i,j),Reaccoef(i,j),j=1, &
                                     Nreacspec(i))
!
        IF (Reacmflg(i).EQ.1) READ (IR1,9010,ERR=40,END=40) &
                                   (Reactemp(i,j),j=1,5)
        GOTO 10
      END IF
!
   20 WRITE (*,9015) line
      GOTO 50
   30 WRITE (*,*) 'Error in reaction specification.'
      GOTO 50
   40 WRITE (*,*) 'Error in temp-dependent reaction K.'
   50 STOP
 9000 FORMAT (A8,2X,I2,3X,3F10.0,5X,I1,9X,F10.0)
 9005 FORMAT (5(I4,F11.0))
 9010 FORMAT (5F12.0)
 9015 FORMAT (1X,'Minerals: Error in data file:',/,1X,A)
 9020 FORMAT (A80)
      END
!
!
!
      SUBROUTINE READSPEC(IR1)
      INTEGER IR1, index, loop
      CHARACTER*80 line
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
!
   10 READ (IR1,9025,ERR=20,END=20) line
      IF (line.EQ.' ') RETURN
!
!   read index number
      READ (line,9000,ERR=20) index
!
!   read second line
      READ (IR1,9005,ERR=20) Specname(index), Nspecspec(index),  &
                            Speckflg(index), Specgflg(index),  &
                            Specz(index), Specthsp(index),  &
                            Specdha(index), Specacon(index),  &
                            Specbcon(index), Specalk(index)
!
!   read third line
      IF (Speckflg(index).EQ.1) THEN
        READ (IR1,9010,END=20,ERR=20) Specrk(index), Spech(index),  &
                                     (Spectemp(index,loop),loop=1,5)
      ELSE
        READ (IR1,9010,ERR=20,END=20) Specrk(index), Spech(index)
      END IF
!
!   read fourth line,loop
      READ (IR1,9015,ERR=20,END=20) (Specspec(index,loop),Speccoef(index &
                                   ,loop),loop=1,Nspecspec(index))
      GOTO 10
!
!   Error: abort the program.
   20 WRITE (*,9020) line
      STOP
 9000 FORMAT (i3)
 9005 FORMAT (a8,2x,i3,i1,i1,6F10.3)
 9010 FORMAT (2F10.3,5E12.5)
 9015 FORMAT (6(i3,f7.3))
 9020 FORMAT (' Error in data file:',/,1x,a)
 9025 FORMAT (a)
      END
!
!
!
      SUBROUTINE WRAPUP
      INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      CLOSE (Iw1)
      CLOSE (Iw2)
      RETURN
      END
!
!
!
      SUBROUTINE OPENIT(QUERY,FNAME,IUNIT,STAT,BATCH,ISCR)
!
!     Opens file on specified unit after soliciting file name.
!
      CHARACTER*(*) QUERY, FNAME, STAT
      INTEGER IUNIT, ISCR
      LOGICAL BATCH
!
!     QUERY - STRING TO PRINT FOR SOLICITING FILE NAME
!     FNAME - INPUT DEFAULT NAME, OUTPUT NAME OF FILE OPENED
!     IUNIT - UNIT NUMBER TO OPEN
!     STAT  - STATUS OF FILE TO OPEN
!     BATCH - .TRUE. IF READING FROM INIT
!             .FALSE. IF QUERYING INTERACTIVIELY
!
!     ISCR  - UNIT NUMBER FOR FILE CONTAINING FILE NAMES
!
      CHARACTER*80 fnamet
      LOGICAL found
      INTEGER n, LENS
      EXTERNAL LENS
!
!                           BATCH READING OF FILE NAME
!
      IF (BATCH) THEN
        READ (ISCR,9000,END=10) FNAME
        GOTO 20
      END IF
!
!                           ERROR IN FILE NAME
!
   10 BATCH = .FALSE.
!
!                           INTERACTIVE SOLICITING OF FILE NAME
!
   20 IF (.NOT.BATCH) THEN
        WRITE (*,9005) QUERY(:LENS(QUERY))
        n = LENS(FNAME)
        IF (n.NE.0) WRITE (*,9010) FNAME(:n)
        READ (*,9000) fnamet
	  call moverelative(-3)
	  call clpart 
        IF (LENS(fnamet).NE.0) FNAME = fnamet
      END IF
      n = LENS(FNAME)
!
!                           CHECK STAT='OLD' EXIST
!
      IF (STAT.EQ.'OLD') THEN
        INQUIRE (FILE=FNAME(:n),EXIST=found)
        IF (.NOT.found) THEN
          WRITE (*,9015) FNAME(:n)
          WRITE (*,9025)
          GOTO 10
        END IF
      END IF
!
!                           CHECK STAT='NEW' EXIST
!
   30 IF (STAT.EQ.'NEW') THEN
        INQUIRE (FILE=FNAME(:n),EXIST=found)
        IF (found) THEN
          WRITE (*,9030) FNAME(:n)
          READ (*,9000) fnamet
          IF (fnamet.NE.' ') THEN
            FNAME = fnamet
            n = LENS(FNAME)
            GOTO 30
          ELSE
            STAT = 'OLD'
          END IF
        END IF
      END IF
!
!                           OPEN FILE
!
      OPEN (UNIT=IUNIT,FILE=FNAME(:n),STATUS=STAT,ERR=40)
!
!                          RETURN
!
      IF (.NOT.BATCH) WRITE (*,9025)
      RETURN
!
!                           ERROR OPENING FILE
!
   40 WRITE (*,9020) FNAME(:n)
      GOTO 10
!
 9000 FORMAT (A)
 9005 FORMAT (1X,A)
 9010 FORMAT (' Default: ',A)
 9015 FORMAT (' Error: file not found: ',/,1X,A)
 9020 FORMAT (' Error: error opening file: ',/,1X,A)
 9025 FORMAT (1X)
 9030 FORMAT (/,' File exists: ',A,/, &
             ' Type new filename or <Enter> to overwrite.')
      END
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
!
!
!
      SUBROUTINE CHARGE_BALANCE(ICORT,DONE,FPOS,ITER)
      LOGICAL DONE
      INTEGER lpspec, ICORT, ITER
      DOUBLE PRECISION CALCMOL, calccat, calcan
!
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
!
      DOUBLE PRECISION fcb(100), cb(100), acb, bcb, elect, FPOS
      DOUBLE PRECISION bestcb(2), bestfact(2)
      SAVE fcb, cb, bestcb, bestfact
      EXTERNAL ITERGAM, CALCMOL
      INTRINSIC DABS
!
!   calculate electrical balance
!
      IF (ICORT.NE.1) THEN
        DONE = .TRUE.
        FPOS = 1.0
        RETURN
      END IF
 
      elect = 0.0D0
      calccat = 0.0D0
      calcan = 0.0D0
      DO 10 lpspec = 1, 250
        IF (Specname(lpspec).NE.' ') THEN
          IF (lpspec.NE.2 .AND. lpspec.NE.3) THEN
            elect = elect + Specz(lpspec)*Mol(lpspec)
            IF (Specz(lpspec).GT.0.0D0) THEN
              calccat = calccat + Specz(lpspec)*Mol(lpspec)*C1save
            ELSE
              calcan = calcan - Specz(lpspec)*Mol(lpspec)*C1save
            END IF
          END IF
        END IF
   10 CONTINUE
      IF (DABS(elect).LT.1D-8) THEN
        DONE = .TRUE.
      ELSE
        DONE = .FALSE.
        IF (ITER.EQ.1) THEN
          fcb(ITER) = FPOS
          cb(ITER) = elect
          bestcb(1) = cb(ITER)
          bestfact(1) = fcb(ITER)
          FPOS = 1.01D0
        ELSE
!           fcb(ITER) = fpos*fcb(ITER-1)
          fcb(ITER) = FPOS
          cb(ITER) = elect
          IF (ITER.EQ.2) THEN
            bestcb(2) = cb(ITER)
            bestfact(2) = fcb(ITER)
          ELSE IF (DABS(cb(ITER)).LT.DABS(bestcb(1))) THEN
            bestcb(1) = cb(ITER)
            bestfact(1) = fcb(ITER)
          ELSE
            bestcb(2) = cb(ITER)
            bestfact(2) = fcb(ITER)
          END IF
! fcb is cumulative factor
          IF ((bestfact(1)-bestfact(2)).NE.0.0) THEN
            acb = (bestcb(1)-bestcb(2))/(bestfact(1)-bestfact(2))
            bcb = bestcb(1) - acb*bestfact(1)
            FPOS = -bcb/acb
          ELSE
            FPOS = bestfact(1)
          END IF
        END IF
      END IF
!      write(*,*) iter
!      write(*,*) bestfact(1), bestcb(1)
!      write(*,*) bestfact(2), bestcb(2)
!      write(*,*)
      RETURN
      END
!
!
!
      SUBROUTINE OUTPUT()
      INTEGER lpspec
!
      INTEGER Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      COMMON /FUNITS/ Icase, Iw1, Iw2, Ir, Iex1, Io1, Iscr2
      INTEGER Lexcept(20), Numexcept, Numreacs, Nreacspec(200),  &
             Reacmflg(200), Reacspec(200,10), Speckflg(250),  &
             Specgflg(250), Specspec(250,10), Speclist(30,0:200),  &
             Nspecspec(250), Lcalk(0:100), Lnoncalk(0:100),  &
             Specinp(4:30)
      DOUBLE PRECISION Spcgfw(4:30), Reacopv(200), Reack(200),  &
                      Reach(200), Reacsi(200), Reaccoef(200,10),  &
                      Reactemp(200,5), Specz(250), Specthsp(250),  &
                      Specdha(250), Specacon(250), Specbcon(250),  &
                      Specalk(250), Specrk(250), Spech(250),  &
                      Spectemp(250,5), Speccoef(250,10)
      CHARACTER*8 Reacname(200), Specname(250)
      CHARACTER*2 Totname(4:30)
      COMMON /WDBINT/ Lexcept, Numexcept, Numreacs, Nreacspec, Reacmflg,  &
                     Reacspec, Speckflg, Specgflg, Specspec, Speclist,  &
                     Nspecspec, Lcalk, Lnoncalk, Specinp
      COMMON /WDBCHAR/ Totname, Reacname, Specname
      COMMON /WDBREAL/ Spcgfw, Reacopv, Reack, Reach, Reacsi, Reaccoef,  &
                      Reactemp, Specz, Specthsp, Specdha, Specacon,  &
                      Specbcon, Specalk, Specrk, Spech, Spectemp,  &
                      Speccoef
      DOUBLE PRECISION Ph, Tempc, Tempk, Density, Mol(250), Alpha(250),  &
                      Gamma(250), Totmol(30), Consta, Constb, Constc,  &
                      Constf, Constr, Speclk(250), Ppm(250),  &
                      Grams(4:30), Logmol(250), Tds, Pe(0:4), Eh, Xmu,  &
                      Epmcat, Epman, C1save
      CHARACTER*80 Wellname
      INTEGER Coralk, Unitflag, Pecalc, Idaves
      COMMON /WMOL  / Ph, Tempc, Tempk, Density, Mol, Alpha, Gamma,  &
                     Totmol, Consta, Constb, Constc, Constf, Constr,  &
                     Speclk, Ppm, Grams, Logmol, Tds, Pe, Eh, Xmu,  &
                     Epmcat, Epman, C1save
      COMMON /WMCHAR/ Wellname
      COMMON /WMINT / Coralk, Unitflag, Pecalc, Idaves
!
      INTRINSIC DLOG10, DABS, DSQRT, SQRT
!
      WRITE (Iw1,9000)
      WRITE (Iw1,9005) Wellname
      WRITE (Iw1,9000)
      WRITE (Iw1,9010)
      WRITE (Iw1,9015) Tempc, Ph, Epmcat, Epman
      WRITE (Iw1,9020) 1.0D3*Totmol(Lexcept(13)), 9.9, 9.9, Unitflag,  &
                      Coralk, Pecalc, Idaves, Eh, Pe(1)
      WRITE (Iw1,9000)
      WRITE (Iw1,9025)
      DO 10 lpspec = 4, 30
        IF (Mol(lpspec).GT.0.0D0) WRITE (Iw1,9030) &
                                        Specname(Specinp(lpspec)),  &
                                        Specz(Specinp(lpspec)),  &
                                        Totmol(lpspec), Logmol(lpspec),  &
                                        Grams(lpspec)
   10 CONTINUE
      WRITE (Iw1,9000)
      WRITE (Iw1,9000)
      RETURN
!
 9000 FORMAT (//)
 9005 FORMAT ('1',(A80),//)
 9010 FORMAT (30X,'----------------',/,30X,'INITIAL SOLUTION',/,30X, &
             '----------------',///)
 9015 FORMAT (10X,'TEMPERATURE = ',F6.2,' DEGREES C    PH = ',F6.3,/, &
             10X,'ANALYTICAL EPMCAT = ',F8.3,'    ANALYTICAL EPMAN = ', &
             F8.3,//)
 9020 FORMAT (5X,'***** OXIDATION - REDUCTION *****',///,1X, &
             'DISSOLVED OXYGEN = ',F6.3,' MMOLES/KG H2O',/,1X, &
             'EH MEASURED WITH CALOMEL = ',F7.4,' VOLTS',9X, &
             'FLAG CORALK PECALC IDAVES',/,1X, &
             'MEASURED EH OF ZOBELL SOLUTION = ',F7.4,' VOLTS',5X,I1, &
             4X,I1,6X,I1,6X,I1,/,1X,'CORRECTED EH = ',F7.4,' VOLTS',/, &
             1X,'PE COMPUTED FROM CORRECTED EH = ',F7.3,/)
 9025 FORMAT (15X,'*** TOTAL CONCENTRATIONS OF INPUT SPECIES ***',//, &
             25X,'TOTAL',13X,'LOG TOTAL',12X,'TOTAL',/,8X,'SPECIES',8X, &
             'MOLALITY',12X,'MOLALITY',11X,'MG/LITRE',/,8X,'-------', &
             8X,'--------',12X,'--------',11X,'--------',/)
 9030 FORMAT (8X,A8,F5.1,1X,1PE12.5,9x,0PF9.4,8X,1PE12.5)
      END

