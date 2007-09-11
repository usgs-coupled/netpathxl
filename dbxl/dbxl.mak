# Microsoft Developer Studio Generated NMAKE File, Based on dbxl.dsp
!IF "$(CFG)" == ""
CFG=dbxl - Win32 Debug
!MESSAGE No configuration specified. Defaulting to dbxl - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "dbxl - Win32 Release" && "$(CFG)" != "dbxl - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "dbxl.mak" CFG="dbxl - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dbxl - Win32 Release" (based on "Win32 (x86) Standard Graphics Application")
!MESSAGE "dbxl - Win32 Debug" (based on "Win32 (x86) Standard Graphics Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "dbxl - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\dbxl.exe"


CLEAN :
	-@erase "$(INTDIR)\ADOBJS.OBJ"
	-@erase "$(INTDIR)\db.obj"
	-@erase "$(INTDIR)\dbwinscreen.obj"
	-@erase "$(INTDIR)\EXCEL97A.OBJ"
	-@erase "$(INTDIR)\npexcel.obj"
	-@erase "$(INTDIR)\npmod.obj"
	-@erase "$(OUTDIR)\dbxl.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/compile_only /nologo /warn:nofileopt /module:"Release/" /object:"Release/" /libs:qwins 
F90_OBJS=.\Release/
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)\dbxl.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\dbxl.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\dbxl.pdb" /machine:I386 /nodefaultlib:"dfconsol.lib" /out:"$(OUTDIR)\dbxl.exe" 
LINK32_OBJS= \
	"$(INTDIR)\ADOBJS.OBJ" \
	"$(INTDIR)\db.obj" \
	"$(INTDIR)\EXCEL97A.OBJ" \
	"$(INTDIR)\dbwinscreen.obj" \
	"$(INTDIR)\npexcel.obj" \
	"$(INTDIR)\npmod.obj"

"$(OUTDIR)\dbxl.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "dbxl - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\dbxl.exe" "$(OUTDIR)\dbxl.bsc"


CLEAN :
	-@erase "$(INTDIR)\ADOBJECTS.MOD"
	-@erase "$(INTDIR)\ADOBJS.OBJ"
	-@erase "$(INTDIR)\ADOBJS.SBR"
	-@erase "$(INTDIR)\db.obj"
	-@erase "$(INTDIR)\db.sbr"
	-@erase "$(INTDIR)\dbwinscreen.obj"
	-@erase "$(INTDIR)\dbwinscreen.sbr"
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\EXCEL97A.MOD"
	-@erase "$(INTDIR)\EXCEL97A.OBJ"
	-@erase "$(INTDIR)\EXCEL97A.SBR"
	-@erase "$(INTDIR)\excel_headings.mod"
	-@erase "$(INTDIR)\filenames.mod"
	-@erase "$(INTDIR)\npexcel.obj"
	-@erase "$(INTDIR)\npexcel.sbr"
	-@erase "$(INTDIR)\npmod.obj"
	-@erase "$(INTDIR)\npmod.sbr"
	-@erase "$(OUTDIR)\dbxl.bsc"
	-@erase "$(OUTDIR)\dbxl.exe"
	-@erase "$(OUTDIR)\dbxl.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90_PROJ=/browser:"Debug/" /check:bounds /compile_only /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" /libs:qwins 
F90_OBJS=.\Debug/
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\dbxl.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\dbxl.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\ADOBJS.SBR" \
	"$(INTDIR)\db.sbr" \
	"$(INTDIR)\EXCEL97A.SBR" \
	"$(INTDIR)\dbwinscreen.sbr" \
	"$(INTDIR)\npexcel.sbr" \
	"$(INTDIR)\npmod.sbr"

"$(OUTDIR)\dbxl.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\dbxl.pdb" /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /out:"$(OUTDIR)\dbxl.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\ADOBJS.OBJ" \
	"$(INTDIR)\db.obj" \
	"$(INTDIR)\EXCEL97A.OBJ" \
	"$(INTDIR)\dbwinscreen.obj" \
	"$(INTDIR)\npexcel.obj" \
	"$(INTDIR)\npmod.obj"

"$(OUTDIR)\dbxl.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("dbxl.dep")
!INCLUDE "dbxl.dep"
!ELSE 
!MESSAGE Warning: cannot find "dbxl.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "dbxl - Win32 Release" || "$(CFG)" == "dbxl - Win32 Debug"
SOURCE=..\ADOBJS.F90

!IF  "$(CFG)" == "dbxl - Win32 Release"


"$(INTDIR)\ADOBJS.OBJ" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "dbxl - Win32 Debug"

F90_MODOUT=\
	"ADOBJECTS"


"$(INTDIR)\ADOBJS.OBJ"	"$(INTDIR)\ADOBJS.SBR"	"$(INTDIR)\ADOBJECTS.MOD" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\EXCEL97A.MOD"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\db.f90

!IF  "$(CFG)" == "dbxl - Win32 Release"


"$(INTDIR)\db.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "dbxl - Win32 Debug"


"$(INTDIR)\db.obj"	"$(INTDIR)\db.sbr" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\filenames.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\dbwinscreen.f90

!IF  "$(CFG)" == "dbxl - Win32 Release"


"$(INTDIR)\dbwinscreen.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "dbxl - Win32 Debug"


"$(INTDIR)\dbwinscreen.obj"	"$(INTDIR)\dbwinscreen.sbr" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\filenames.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\EXCEL97A.F90

!IF  "$(CFG)" == "dbxl - Win32 Release"


"$(INTDIR)\EXCEL97A.OBJ" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "dbxl - Win32 Debug"

F90_MODOUT=\
	"EXCEL97A"


"$(INTDIR)\EXCEL97A.OBJ"	"$(INTDIR)\EXCEL97A.SBR"	"$(INTDIR)\EXCEL97A.MOD" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\npexcel.F90

!IF  "$(CFG)" == "dbxl - Win32 Release"


"$(INTDIR)\npexcel.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "dbxl - Win32 Debug"


"$(INTDIR)\npexcel.obj"	"$(INTDIR)\npexcel.sbr" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\ADOBJECTS.MOD" "$(INTDIR)\EXCEL97A.MOD" "$(INTDIR)\filenames.mod" "$(INTDIR)\excel_headings.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\npmod.f90

!IF  "$(CFG)" == "dbxl - Win32 Release"


"$(INTDIR)\npmod.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "dbxl - Win32 Debug"

F90_MODOUT=\
	"excel_headings" \
	"filenames"


"$(INTDIR)\npmod.obj"	"$(INTDIR)\npmod.sbr"	"$(INTDIR)\excel_headings.mod"	"$(INTDIR)\filenames.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

