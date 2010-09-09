!
!
!
subroutine confignetpath
  USE IFQWIN
  USE screen_parameters
  USE version
  implicit none
  !TYPE QWINFO
  !    INTEGER(2) TYPE  ! request type
  !    INTEGER(2) X     ! x coordinate for upper left
  !    INTEGER(2) Y     ! y coordinate for upper left
  !    INTEGER(2) H     ! window height
  !    INTEGER(2) W     ! window width
  ! END TYPE QWINFO

  LOGICAL(4)     xresult, status
  integer*2 i1, i2, i3, i4 
  TYPE (qwinfo)  winfo
  TYPE (windowconfig) wc
  character(25) str1

  ! Set frame window
  winfo%TYPE = QWIN$SET
  winfo%x    = 0
  winfo%y    = 0
  winfo%h    = 600
  winfo%w    = 850
  xresult =     SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)
  winfo%TYPE = QWIN$RESTORE
  xresult =     SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo) ! needed to prevent full screen
  xresult =     SETEXITQQ(QWIN$EXITNOPERSIST) 

 wc%numxpixels  = -1
 wc%numypixels  = -1
 wc%numtextcols = MaxColumns
 wc%numtextrows = MaxRows
 wc%mode = QWIN$SCROLLDOWN
 wc%title= TRIM(VersionNumber)//char(0)
 wc%fontsize = QWIN$EXTENDFONT
 wc%extendfontname = "Lucida Console"//char(0)
 wc%extendfontsize = Z'000B0012'   ! height is 18, width is 11
 wc%extendfontsize = 16 + 10*65536 ! height is 16, width is 10
 wc%extendfontattributes = QWIN$EXTENDFONT_NORMAL
 status = SETWINDOWCONFIG(wc)  ! attempt to set configuration with above values
    ! if attempt fails, set with system estimated values
    if (.NOT.status) status = SETWINDOWCONFIG(wc)
    
 ! need display cursor after setwindowconfig
 call set_color_np
 xresult = DISPLAYCURSOR ($GCURSORON)
 
return
end subroutine confignetpath
