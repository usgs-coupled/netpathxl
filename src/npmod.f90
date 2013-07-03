module filenames
  CHARACTER*256 Efile, root
  CHARACTER*256 Wfile, path, filename
  CHARACTER Phase*8, Eleshort*2, It*1, F*1
  DIMENSION Phase(39), Eleshort(39), It(39), F(39)
  !COMMON /CHAR1 / Phase(39), Eleshort(39), Efile, Pfile, Wfile, &
  !    It(39), F(39)
  DATA Eleshort/'C ', 'S ', 'CA', 'AL', 'MG', 'NA', 'K ', 'CL',  &
     'F ', 'SI', 'BR', 'B ', 'BA', 'LI', 'SR', 'FE', 'MN', 'N ',  &
     'P ', 'RS', 'I1', 'I2', 'I3', 'I4', 'I9', 'D ', '18', 'TR',  &
     'I5', 'I6', 'I7', 'I8', 'I0', 'EX', 'X1', 'X2', 'X3', 'X4',  &
     'X5'/
  character *256 excel_root, excel_filename, excel_path, excel_wfile
  character *256 excel_wateq_filename
  logical excel_file, excel_cb
end module filenames

module version
  CHARACTER*32 ProgramName, VersionNumber, datestr, DBXLName
  DATA ProgramName/'NetpathXL'/
  DATA DBXLName/'DBXL'/
  DATA VersionNumber/'1.3'/
  DATA datestr/'September 8, 2010'/
end module version

module max_size
    PARAMETER MAXWELLS=400
end module max_size

module screen_parameters
    integer*2 MaxRows /500/, MaxColumns/90/
    integer*4 bk_red, bk_green, bk_blue
    integer*4 bk_red_default /0/, bk_green_default /0/, bk_blue_default /255/
    double precision bk_dimmer_default / .25 /, bk_dimmer /0.25/
    integer*4 text_red, text_green, text_blue
    integer*4 text_red_default /255/, text_green_default /255/, text_blue_default /255/
    double precision text_dimmer_default /0.85/, text_dimmer /0.85 /
    contains

 subroutine set_color_np 
 USE IFQWIN
 implicit none
 !interface
 !   subroutine set_color_np
 !   end subroutine set_color_np
 !end interface
 integer *4 color, status
 
 ! set background color
 bk_red = bk_red_default * bk_dimmer;
 bk_green = bk_green_default * bk_dimmer;
 bk_blue = bk_blue_default * bk_dimmer;
 color = bk_red + 256*bk_green + 256*256*bk_blue
 status = SETBKCOLORRGB(color)
 
 ! set text color
 text_red = text_red_default * text_dimmer;
 text_green = text_green_default * text_dimmer;
 text_blue = text_blue_default * text_dimmer;
 color = text_red + 256*text_green + 256*256*text_blue
 status = SETTEXTCOLORRGB(color)
 
 end subroutine set_color_np
 
 
end module screen_parameters
    

module excel_headings
	  character*50, dimension(2, 21) :: headings
	  data headings / &
	    'a1','Netpath Spreadsheet 2.14', &
		'b2','Units flag:', &
		'c2','0 mmol/L', &
		'g2','1 meq/L', &
		'j2','2 mg/L', &
		'l2','3 ppm', &
		'n2','4 mmol/kgw', &
		'b3','Eh flag:', &
		'c3','0 Redox ignored', &
		'g3','1 Eh, volts', &
		'j3','2 DO', &
		'l3','3 Sato DO', &
		'n3','4 SO4/H2S', &
		'b4','Alkalinity/TDIC flag:', &
		'c4','0 Field alk as HCO3', &
		'g4','1 Carbonate alk as HCO3', &
		'j4','2 TDIC as HCO3', &
		'l4','3 Field alk as CaCO3', &
		'b5','Activity coefficient flag:', &
		'c5','0 Debye-Huckel', &
		'g5','1 Davies'/

 	  character*40, dimension(2,62) :: colheadings
	  integer count_colheadings 
	  data count_colheadings /62/
	  data colheadings / &
		'a7','Number', &
		'b7','Well name (required)', &
		'c7','Units Flag', &
		'd7','Eh Flag', &
		'e7','Alk/TDIC Flag', &
		'f7','Act Coef Flag', &
		'g7','Temp', &
		'h7','pH', &
		'i7','Eh', &
		'j7','Ca', &
		'k7','Mg', &
		'l7','Na', &
		'm7','K', &
		'n7','Cl', &
		'o7','SO4 as SO4', &
		'p7','Alk/TDIC', &
		'q7','Fe', &
		'r7','Mn', &
		's7','Al', &
		't7','F', &
		'u7','Si as SiO2', &
		'v7','Br', &
		'w7','B', &
		'x7','Ba', &
		'y7','Li', &
		'z7','Sr', &
		'aa7','NO3 as N', &
		'ab7','NH4 as N', &
		'ac7','P', &
		'ad7','O2(aq) as O2', &
		'ae7','H2S(aq) as S', &
		'af7','N2(aq) as N', &
		'ag7','CH4(aq) as CH4', &
		'ah7','DOC as C', &
		'ai7','RS of DOC', &
		'aj7','C-13 TDIC', &
		'ak7','C-14 TDIC', &
		'al7','S-34 SO4', &
		'am7','S-34 H2S', &
		'an7','H-2', &
		'ao7','H-3', &
		'ap7','O-18', &
		'aq7','Sr-87', &
		'ar7','N-15 N2(aq)', &
		'as7','N-15 NO3', &
		'at7','N-15 NH4', &
		'au7','Density', &
		'av7','Sp Cond', &
		'a408','Maximum number of wells is 400', & ! 47
		'aw7','Field ID', &
		'ax7','ID number', &
		'ay7','Date/time', &
		'az7','Lat/lon', &
		'ba7','Aquifer', &		
		'bb7','Depth', &	
		'bc7','Casing', &		
		'bd7','Elevation', &
		'be7','Address 1', &
		'bf7','Address 2', &
		'bg7','Address 3', &
		'bh7','Address 4', &	
		'bi7','Address 5'/	
end module excel_headings	