SUBROUTINE checklang($APP)
    USE ADOBJECTS
    USE EXCEL
    USE IFCOM
    USE MSO
    IMPLICIT NONE
    EXTERNAL UPCS

    INTEGER(INT_PTR_KIND()), INTENT(IN)     :: $APP
    INTEGER(INT_PTR_KIND()) :: langsettings
    INTEGER(4)              :: status
    INTEGER                 :: lid
    CHARACTER*20            :: ans
    
    langsettings = $Application_GetLanguageSettings($APP, status)
    IF (langsettings == 0) THEN
        WRITE (*, '(" Unable to get LanguageSettings object; Aborting")')
        CALL EXIT()
    END IF
    
    lid = LanguageSettings_GetLanguageID(langsettings, msoLanguageIDUI, status)
    IF (lid .NE. msoLanguageIDEnglishUS) THEN
		WRITE (*,*) ""
		WRITE (*,*) "Warning: NetpathXL may not work correctly using the currently selected language."
		WRITE (*,*) "Please select US English in the Excel language settings."
		WRITE (*,*) "Hit <Q> to quit, or <Enter> to continue"
		READ  (*,'(A)') ans
        IF (ans.EQ.'Q' .OR. ans.EQ.'q') THEN
            CALL RELEASEOBJECTS()
            CALL EXIT()
        END IF
    END IF
    
    status = COMReleaseObject(langsettings)
END SUBROUTINE checklang
