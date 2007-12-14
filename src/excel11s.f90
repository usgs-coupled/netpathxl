! excel subset
	MODULE EXCEL11
		USE IFWINTY
		USE IFAUTO
		IMPLICIT NONE
		! Module Procedures
		CONTAINS
  			FUNCTION Range_GetValue($OBJECT, RangeValueDataType, $STATUS)
				IMPLICIT NONE
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: RangeValueDataType	
				!DEC$ ATTRIBUTES REFERENCE	:: RangeValueDataType
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				TYPE (VARIANT), VOLATILE :: $RETURN
				TYPE (VARIANT) Range_GetValue
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Value', $RETURN, AUTO_ARG_OUT)
				IF (PRESENT(RangeValueDataType)) CALL AUTOADDARG(invokeargs, '$ARG1', RangeValueDataType, AUTO_ARG_IN)
!!!				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 6, invokeargs)
				$$STATUS = AUTOGETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				Range_GetValue = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION Range_GetValue		
  			!$Application_GetWorkbooks return type is POINTER(p, INTEGER(INT_PTR_KIND()))
			INTEGER(INT_PTR_KIND()) FUNCTION $Application_GetWorkbooks($OBJECT, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN = 0
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Workbooks', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
!!!				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 572, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOGETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				$Application_GetWorkbooks = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION $Application_GetWorkbooks
  			!$Worksheet_GetRange return type is POINTER(p, INTEGER(INT_PTR_KIND()))
			INTEGER(INT_PTR_KIND()) FUNCTION $Worksheet_GetRange($OBJECT, Cell1, Cell2, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN)	:: Cell1	
				!DEC$ ATTRIBUTES REFERENCE	:: Cell1
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Cell2	
				!DEC$ ATTRIBUTES REFERENCE	:: Cell2
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN = 0
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Range', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
				CALL AUTOADDARG(invokeargs, '$ARG1', Cell1, AUTO_ARG_IN)
				IF (PRESENT(Cell2)) CALL AUTOADDARG(invokeargs, '$ARG2', Cell2, AUTO_ARG_IN)
!!!				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 197, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOGETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				$Worksheet_GetRange = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION $Worksheet_GetRange
  			!Workbooks_Add return type is POINTER(p, INTEGER(INT_PTR_KIND()))
			INTEGER(INT_PTR_KIND()) FUNCTION Workbooks_Add($OBJECT, Template, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Template	
				!DEC$ ATTRIBUTES REFERENCE	:: Template
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN = 0
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, '$RETURN', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
				IF (PRESENT(Template)) CALL AUTOADDARG(invokeargs, '$ARG1', Template, AUTO_ARG_IN)
!!!				$$STATUS = AUTOINVOKE($OBJECT, 181, invokeargs)              ! Excel.Application.11
				$$STATUS = AUTOINVOKE($OBJECT, 'Add', invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				Workbooks_Add = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION Workbooks_Add
  			INTEGER(INT_PTR_KIND()) FUNCTION $Workbook_GetActiveSheet($OBJECT, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN = 0
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'ActiveSheet', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
!!!				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 307, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOGETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				$Workbook_GetActiveSheet = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION $Workbook_GetActiveSheet
  			!$Worksheet_GetCells return type is POINTER(p, INTEGER(INT_PTR_KIND()))
			INTEGER(INT_PTR_KIND()) FUNCTION $Worksheet_GetCells($OBJECT, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN = 0
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Cells', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
!!!				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 238, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOGETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				$Worksheet_GetCells = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION $Worksheet_GetCells
  			!Workbooks_Open return type is POINTER(p, INTEGER(INT_PTR_KIND()))
			INTEGER(INT_PTR_KIND()) FUNCTION Workbooks_Open($OBJECT, Filename, UpdateLinks, ReadOnly, Format, Password, WriteResPassword, IgnoreReadOnlyRecommended, Origin, Delimiter, Editable, Notify, Converter, AddToMru, Local, CorruptLoad, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				CHARACTER(LEN=*), INTENT(IN)	:: Filename	! BSTR
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: UpdateLinks	
				!DEC$ ATTRIBUTES REFERENCE	:: UpdateLinks
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: ReadOnly	
				!DEC$ ATTRIBUTES REFERENCE	:: ReadOnly
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Format	
				!DEC$ ATTRIBUTES REFERENCE	:: Format
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Password	
				!DEC$ ATTRIBUTES REFERENCE	:: Password
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: WriteResPassword	
				!DEC$ ATTRIBUTES REFERENCE	:: WriteResPassword
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: IgnoreReadOnlyRecommended	
				!DEC$ ATTRIBUTES REFERENCE	:: IgnoreReadOnlyRecommended
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Origin	
				!DEC$ ATTRIBUTES REFERENCE	:: Origin
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Delimiter	
				!DEC$ ATTRIBUTES REFERENCE	:: Delimiter
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Editable	
				!DEC$ ATTRIBUTES REFERENCE	:: Editable
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Notify	
				!DEC$ ATTRIBUTES REFERENCE	:: Notify
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Converter	
				!DEC$ ATTRIBUTES REFERENCE	:: Converter
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AddToMru	
				!DEC$ ATTRIBUTES REFERENCE	:: AddToMru
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Local	
				!DEC$ ATTRIBUTES REFERENCE	:: Local
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: CorruptLoad	
				!DEC$ ATTRIBUTES REFERENCE	:: CorruptLoad
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN = 0
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, '$RETURN', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
				CALL AUTOADDARG(invokeargs, '$ARG1', Filename, AUTO_ARG_IN, VT_BSTR)
				IF (PRESENT(UpdateLinks)) CALL AUTOADDARG(invokeargs, '$ARG2', UpdateLinks, AUTO_ARG_IN)
				IF (PRESENT(ReadOnly)) CALL AUTOADDARG(invokeargs, '$ARG3', ReadOnly, AUTO_ARG_IN)
				IF (PRESENT(Format)) CALL AUTOADDARG(invokeargs, '$ARG4', Format, AUTO_ARG_IN)
				IF (PRESENT(Password)) CALL AUTOADDARG(invokeargs, '$ARG5', Password, AUTO_ARG_IN)
				IF (PRESENT(WriteResPassword)) CALL AUTOADDARG(invokeargs, '$ARG6', WriteResPassword, AUTO_ARG_IN)
				IF (PRESENT(IgnoreReadOnlyRecommended)) CALL AUTOADDARG(invokeargs, '$ARG7', IgnoreReadOnlyRecommended, AUTO_ARG_IN)
				IF (PRESENT(Origin)) CALL AUTOADDARG(invokeargs, '$ARG8', Origin, AUTO_ARG_IN)
				IF (PRESENT(Delimiter)) CALL AUTOADDARG(invokeargs, '$ARG9', Delimiter, AUTO_ARG_IN)
				IF (PRESENT(Editable)) CALL AUTOADDARG(invokeargs, '$ARG10', Editable, AUTO_ARG_IN)
				IF (PRESENT(Notify)) CALL AUTOADDARG(invokeargs, '$ARG11', Notify, AUTO_ARG_IN)
				IF (PRESENT(Converter)) CALL AUTOADDARG(invokeargs, '$ARG12', Converter, AUTO_ARG_IN)
				IF (PRESENT(AddToMru)) CALL AUTOADDARG(invokeargs, '$ARG13', AddToMru, AUTO_ARG_IN)
				IF (PRESENT(Local)) CALL AUTOADDARG(invokeargs, '$ARG14', Local, AUTO_ARG_IN)
				IF (PRESENT(CorruptLoad)) CALL AUTOADDARG(invokeargs, '$ARG15', CorruptLoad, AUTO_ARG_IN)
!!!				$$STATUS = AUTOINVOKE($OBJECT, 1923, invokeargs)             ! Excel.Application.11
				$$STATUS = AUTOINVOKE($OBJECT, 'Open', invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				Workbooks_Open = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION Workbooks_Open
  			SUBROUTINE $Application_SetVisible($OBJECT, $ARG1, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				LOGICAL(2), INTENT(IN)	:: $ARG1	
				!DEC$ ATTRIBUTES REFERENCE	:: $ARG1
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Visible', $ARG1)
!!!				$$STATUS = AUTOSETPROPERTYBYID($OBJECT, 558, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOSETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE $Application_SetVisible
  			SUBROUTINE Range_SetColumnWidth($OBJECT, $ARG1, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN)	:: $ARG1	
				!DEC$ ATTRIBUTES REFERENCE	:: $ARG1
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'ColumnWidth', $ARG1, AUTO_ARG_IN)
!!!				$$STATUS = AUTOSETPROPERTYBYID($OBJECT, 242, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOSETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE Range_SetColumnWidth
  			SUBROUTINE Range_SetWrapText($OBJECT, $ARG1, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN)	:: $ARG1	
				!DEC$ ATTRIBUTES REFERENCE	:: $ARG1
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'WrapText', $ARG1, AUTO_ARG_IN)
!!!				$$STATUS = AUTOSETPROPERTYBYID($OBJECT, 276, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOSETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE Range_SetWrapText
  			SUBROUTINE Range_SetHorizontalAlignment($OBJECT, $ARG1, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN)	:: $ARG1	
				!DEC$ ATTRIBUTES REFERENCE	:: $ARG1
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'HorizontalAlignment', $ARG1, AUTO_ARG_IN)
!!!				$$STATUS = AUTOSETPROPERTYBYID($OBJECT, 136, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOSETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE Range_SetHorizontalAlignment
  			SUBROUTINE Range_SetLocked($OBJECT, $ARG1, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN)	:: $ARG1	
				!DEC$ ATTRIBUTES REFERENCE	:: $ARG1
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Locked', $ARG1, AUTO_ARG_IN)
!!!				$$STATUS = AUTOSETPROPERTYBYID($OBJECT, 269, invokeargs)     ! Excel.Application.11
				$$STATUS = AUTOSETPROPERTYINVOKEARGS($OBJECT, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE Range_SetLocked
  			SUBROUTINE $Workbook_SaveAs($OBJECT, Filename, FileFormat, Password, WriteResPassword, ReadOnlyRecommended, CreateBackup, AccessMode, ConflictResolution, AddToMru, TextCodepage, TextVisualLayout, Local, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Filename	
				!DEC$ ATTRIBUTES REFERENCE	:: Filename
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: FileFormat	
				!DEC$ ATTRIBUTES REFERENCE	:: FileFormat
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Password	
				!DEC$ ATTRIBUTES REFERENCE	:: Password
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: WriteResPassword	
				!DEC$ ATTRIBUTES REFERENCE	:: WriteResPassword
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: ReadOnlyRecommended	
				!DEC$ ATTRIBUTES REFERENCE	:: ReadOnlyRecommended
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: CreateBackup	
				!DEC$ ATTRIBUTES REFERENCE	:: CreateBackup
				INTEGER, INTENT(IN), OPTIONAL	:: AccessMode	! XlSaveAsAccessMode
				!DEC$ ATTRIBUTES REFERENCE	:: AccessMode
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: ConflictResolution	
				!DEC$ ATTRIBUTES REFERENCE	:: ConflictResolution
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AddToMru	
				!DEC$ ATTRIBUTES REFERENCE	:: AddToMru
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: TextCodepage	
				!DEC$ ATTRIBUTES REFERENCE	:: TextCodepage
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: TextVisualLayout	
				!DEC$ ATTRIBUTES REFERENCE	:: TextVisualLayout
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Local	
				!DEC$ ATTRIBUTES REFERENCE	:: Local
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				IF (PRESENT(Filename)) CALL AUTOADDARG(invokeargs, '$ARG1', Filename, AUTO_ARG_IN)
				IF (PRESENT(FileFormat)) CALL AUTOADDARG(invokeargs, '$ARG2', FileFormat, AUTO_ARG_IN)
				IF (PRESENT(Password)) CALL AUTOADDARG(invokeargs, '$ARG3', Password, AUTO_ARG_IN)
				IF (PRESENT(WriteResPassword)) CALL AUTOADDARG(invokeargs, '$ARG4', WriteResPassword, AUTO_ARG_IN)
				IF (PRESENT(ReadOnlyRecommended)) CALL AUTOADDARG(invokeargs, '$ARG5', ReadOnlyRecommended, AUTO_ARG_IN)
				IF (PRESENT(CreateBackup)) CALL AUTOADDARG(invokeargs, '$ARG6', CreateBackup, AUTO_ARG_IN)
				IF (PRESENT(AccessMode)) CALL AUTOADDARG(invokeargs, '$ARG7', AccessMode)
				IF (PRESENT(ConflictResolution)) CALL AUTOADDARG(invokeargs, '$ARG8', ConflictResolution, AUTO_ARG_IN)
				IF (PRESENT(AddToMru)) CALL AUTOADDARG(invokeargs, '$ARG9', AddToMru, AUTO_ARG_IN)
				IF (PRESENT(TextCodepage)) CALL AUTOADDARG(invokeargs, '$ARG10', TextCodepage, AUTO_ARG_IN)
				IF (PRESENT(TextVisualLayout)) CALL AUTOADDARG(invokeargs, '$ARG11', TextVisualLayout, AUTO_ARG_IN)
				IF (PRESENT(Local)) CALL AUTOADDARG(invokeargs, '$ARG12', Local, AUTO_ARG_IN)
!!!				$$STATUS = AUTOINVOKE($OBJECT, 1925, invokeargs)             ! Excel.Application.11
				$$STATUS = AUTOINVOKE($OBJECT, 'SaveAs', invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE $Workbook_SaveAs
  			SUBROUTINE $Application_Quit($OBJECT, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
!!!				$$STATUS = AUTOINVOKE($OBJECT, 302, invokeargs)              ! Excel.Application.11
				$$STATUS = AUTOINVOKE($OBJECT, 'Quit', invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE $Application_Quit
  			SUBROUTINE $Worksheet_Protect($OBJECT, Password, DrawingObjects, Contents, Scenarios, UserInterfaceOnly, AllowFormattingCells, AllowFormattingColumns, AllowFormattingRows, AllowInsertingColumns, AllowInsertingRows, AllowInsertingHyperlinks, AllowDeletingColumns, AllowDeletingRows, AllowSorting, AllowFiltering, AllowUsingPivotTables, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Password	
				!DEC$ ATTRIBUTES REFERENCE	:: Password
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: DrawingObjects	
				!DEC$ ATTRIBUTES REFERENCE	:: DrawingObjects
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Contents	
				!DEC$ ATTRIBUTES REFERENCE	:: Contents
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: Scenarios	
				!DEC$ ATTRIBUTES REFERENCE	:: Scenarios
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: UserInterfaceOnly	
				!DEC$ ATTRIBUTES REFERENCE	:: UserInterfaceOnly
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowFormattingCells	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowFormattingCells
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowFormattingColumns	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowFormattingColumns
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowFormattingRows	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowFormattingRows
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowInsertingColumns	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowInsertingColumns
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowInsertingRows	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowInsertingRows
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowInsertingHyperlinks	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowInsertingHyperlinks
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowDeletingColumns	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowDeletingColumns
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowDeletingRows	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowDeletingRows
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowSorting	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowSorting
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowFiltering	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowFiltering
				TYPE (VARIANT), INTENT(IN), OPTIONAL	:: AllowUsingPivotTables	
				!DEC$ ATTRIBUTES REFERENCE	:: AllowUsingPivotTables
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				IF (PRESENT(Password)) CALL AUTOADDARG(invokeargs, '$ARG1', Password, AUTO_ARG_IN)
				IF (PRESENT(DrawingObjects)) CALL AUTOADDARG(invokeargs, '$ARG2', DrawingObjects, AUTO_ARG_IN)
				IF (PRESENT(Contents)) CALL AUTOADDARG(invokeargs, '$ARG3', Contents, AUTO_ARG_IN)
				IF (PRESENT(Scenarios)) CALL AUTOADDARG(invokeargs, '$ARG4', Scenarios, AUTO_ARG_IN)
				IF (PRESENT(UserInterfaceOnly)) CALL AUTOADDARG(invokeargs, '$ARG5', UserInterfaceOnly, AUTO_ARG_IN)
				IF (PRESENT(AllowFormattingCells)) CALL AUTOADDARG(invokeargs, '$ARG6', AllowFormattingCells, AUTO_ARG_IN)
				IF (PRESENT(AllowFormattingColumns)) CALL AUTOADDARG(invokeargs, '$ARG7', AllowFormattingColumns, AUTO_ARG_IN)
				IF (PRESENT(AllowFormattingRows)) CALL AUTOADDARG(invokeargs, '$ARG8', AllowFormattingRows, AUTO_ARG_IN)
				IF (PRESENT(AllowInsertingColumns)) CALL AUTOADDARG(invokeargs, '$ARG9', AllowInsertingColumns, AUTO_ARG_IN)
				IF (PRESENT(AllowInsertingRows)) CALL AUTOADDARG(invokeargs, '$ARG10', AllowInsertingRows, AUTO_ARG_IN)
				IF (PRESENT(AllowInsertingHyperlinks)) CALL AUTOADDARG(invokeargs, '$ARG11', AllowInsertingHyperlinks, AUTO_ARG_IN)
				IF (PRESENT(AllowDeletingColumns)) CALL AUTOADDARG(invokeargs, '$ARG12', AllowDeletingColumns, AUTO_ARG_IN)
				IF (PRESENT(AllowDeletingRows)) CALL AUTOADDARG(invokeargs, '$ARG13', AllowDeletingRows, AUTO_ARG_IN)
				IF (PRESENT(AllowSorting)) CALL AUTOADDARG(invokeargs, '$ARG14', AllowSorting, AUTO_ARG_IN)
				IF (PRESENT(AllowFiltering)) CALL AUTOADDARG(invokeargs, '$ARG15', AllowFiltering, AUTO_ARG_IN)
				IF (PRESENT(AllowUsingPivotTables)) CALL AUTOADDARG(invokeargs, '$ARG16', AllowUsingPivotTables, AUTO_ARG_IN)
!!!				$$STATUS = AUTOINVOKE($OBJECT, 2029, invokeargs)             ! Excel.Application.11
				$$STATUS = AUTOINVOKE($OBJECT, 'Protect', invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END SUBROUTINE $Worksheet_Protect
	END MODULE
