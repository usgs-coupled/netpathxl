  ! This software ADOBJS.F90 is part of the Compaq Visual Fortran kit.
  ! Copyright (C) 1997,1999 Digital Equipment Corporation.
  ! All rights reserved.
  ! 
  ! This software is furnished to you under a license by  Digital Equipment
  ! Corporation and must be used only in  accordance with the terms and conditions
  ! of that license,  and only with the inclusion of the above copyright notice.
  ! 
  ! This software is provided as an example that demonstrates a particular
  ! feature/function of the licensed product. The software is not intended to
  ! provide a complete solution to any application problem but rather is provided
  ! as a teaching mechanism.
  ! 
  ! Users may incorporate any part of this sample into their own source after 
  ! making appropriate changes to make it suitable for the intended application.

	MODULE ADOBJECTS
	USE EXCEL11
	IMPLICIT NONE

  ! Object Pointers

	  INTEGER*4 excelapp
	  INTEGER*4 workbooks
	  INTEGER*4 workbook
	  INTEGER*4 worksheets
	  INTEGER*4 worksheet
	  INTEGER*4 range
	  INTEGER*4 charts
	  INTEGER*4 chart
	  INTEGER*4, DIMENSION(1:12) :: cells
	  INTEGER*4 categoryAxis
	  INTEGER*4 valueAxis

  ! BSTRs
	  INTEGER*4 bstr1
	  INTEGER*4 bstr2
	  INTEGER*4 bstr3
  ! Variants
	  TYPE (VARIANT) :: vARG1
	  TYPE (VARIANT) :: vARG2

	CONTAINS

	  SUBROUTINE INITOBJECTS()
		  INTEGER i;

		  excelapp = 0
		  workbooks = 0
		  workbook = 0
		  worksheets = 0
		  worksheet = 0
		  range = 0
		  charts = 0
		  chart = 0
		  DO i=1,12
			cells(i) = 0
		  END DO
		  categoryAxis = 0
		  valueAxis = 0

		  bstr1 = 0
		  bstr2 = 0
		  bstr3 = 0
	
	  END SUBROUTINE
	  
	  SUBROUTINE RELEASEOBJECTS()
	
		  USE IFCOM
	
		  INTEGER*4 status
		  INTEGER i;

		  IF (range /= 0) status = COMRELEASEOBJECT ( range )
		  IF (chart /= 0) status = COMRELEASEOBJECT ( chart )
		  IF (charts /= 0) status = COMRELEASEOBJECT ( charts )
		  IF (worksheets /= 0) status = COMRELEASEOBJECT ( worksheet )
		  IF (worksheet /= 0) status = COMRELEASEOBJECT ( worksheet )
		  IF (workbook /= 0) status = COMRELEASEOBJECT ( workbook )
		  IF (workbooks /= 0) status = COMRELEASEOBJECT ( workbooks )
		  DO i=1,12
			  IF (cells(i) /= 0) status = COMRELEASEOBJECT ( cells(i) )
		  END DO
		  IF (categoryAxis /= 0) status = COMRELEASEOBJECT ( categoryAxis )
		  IF (valueAxis /= 0) status = COMRELEASEOBJECT ( valueAxis )
		  IF (excelapp /= 0) status = COMRELEASEOBJECT ( excelapp )

		  IF (bstr1 /= 0) CALL SysFreeString(bstr1)
		  IF (bstr2 /= 0) CALL SysFreeString(bstr2)
		  IF (bstr3 /= 0) CALL SysFreeString(bstr3)
	
	  END SUBROUTINE
!
!
!
Subroutine setcell_character(cell, string)
      IMPLICIT NONE   

	  INTEGER*4 status
	  character*(*) cell, string


	  call set_range(cell, cell)

	  CALL Check_Status(status, " Unable to get RANGE object")

	  status = AUTOSETPROPERTY (range, "VALUE", string)
	  CALL Check_Status(status, " Unable to set string in cell")

	  return
end subroutine
!
!
!
Subroutine setcell_integer(cell, i)
      IMPLICIT NONE   

	  INTEGER*4 status, i
	  character*(*) cell

	  call set_range(cell, cell)

	  CALL Check_Status(status, " Unable to get RANGE object")

	  status = AUTOSETPROPERTY (range, "VALUE", i)
	  CALL Check_Status(status, " Unable to set string in cell")

	  return
end subroutine
!
!
!
Subroutine setcell_float(cell, f)
      IMPLICIT NONE   

	  INTEGER*4 status
	  REAL f
	  character*(*) cell

	  call set_range(cell, cell)

	  CALL Check_Status(status, " Unable to get RANGE object")

	  status = AUTOSETPROPERTY (range, "VALUE", f)
	  CALL Check_Status(status, " Unable to set string in cell")

	  return
end subroutine
!
!
!
subroutine set_range(str1, str2)
  USE IFCOM
  IMPLICIT NONE   
  character*(*) str1, str2
  INTEGER*4 status

  TYPE (VARIANT) :: vBSTR1
  TYPE (VARIANT) :: vBSTR2

! set variants
  CALL VariantInit(vBSTR1)
  vBSTR1%VT = VT_BSTR
  bstr1 = ConvertStringToBSTR(str1)
  vBSTR1%VU%PTR_VAL = bstr1
  CALL VariantInit(vBSTR2)
  vBSTR2%VT = VT_BSTR
  bstr2 = ConvertStringToBSTR(str2)
  vBSTR2%VU%PTR_VAL = bstr2

! free space
  IF (range /= 0) status = COMRELEASEOBJECT ( range )

! set range
  range = $Worksheet_GetRange(worksheet, vBSTR1, vBSTR2, status)

! check result
  CALL Check_Status(status, " Unable to get RANGE object")

! free space
  status = VariantClear(vBSTR1)
  bstr1 = 0
  status = VariantClear(vBSTR2)
  bstr2 = 0

  return
end subroutine set_range
!
!
!
subroutine set_variant_bool(vARG, bool_value)
  USE IFCOM
  implicit none
  
  type (variant) vARG
  logical bool_value
  integer*4 status
 
  status = VariantClear(vARG)
  CALL VariantInit(vARG)
  vARG%VT = VT_BOOL
  vARG%VU%BOOL_VAL = bool_value
  return
end subroutine set_variant_bool
!
!
!
subroutine set_variant_char(vARG, char_value)
  USE IFCOM
  implicit none
  
  type (variant) vARG
  character*(*) char_value
  integer*4 status
 
  status = VariantClear(vARG)
  CALL VariantInit(vARG)
  vARG%VT = VT_BSTR
  bstr1 = ConvertStringToBSTR(char_value)
  vARG%VU%PTR_VAL = bstr1
  return
end subroutine set_variant_char
!
!
!
subroutine set_variant_int(vARG, int_value)
  USE IFCOM
  implicit none
  
  type (variant) vARG
  integer*4 int_value
  integer*4 status
 
  status = VariantClear(vARG)
  CALL VariantInit(vARG)
  vARG%VT = VT_I4
  vARG%VU%LONG_VAL = int_value
  return
end subroutine set_variant_int




	END MODULE