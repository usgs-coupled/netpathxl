NetpathXL 1.1 

Two programs are distributed, DBXL and NetpathXL. They are normally 
installed in the directory c:\program files\usgs\NetpathXL\bin. They 
can be run from the start menu or by browsing for the executables. A
shortcut can be added to the desktop by right-clicking on the executable
and selecting Send To->Desktop.


The program DbXL.exe has two functions (1) to create a new Excel file 
that can be used by NetpathXL.exe and (2) to convert an old .lon file 
to an Excel file. All the basic chemical data are stored in the Excel file.  
Once you have done one of these two functions, you probably will not run 
DbXL.exe much; simply copy and modify Excel files as needed.  The columns 
of the Excel file are fixed and can not be changed.  

NetpathXL.exe will read an old .pat file (created by DB), a new .pat file 
(created by NetpathXL), or the new Excel file (.xls) and do all the usual 
NETPATH calculations. db.dat is used to speciate the data in the Excel 
file at the beginning of running NetpathXL.exe. This file is stored in the 
installation directory and used by default for speciation calculations. 
Netpath.dat is a file of mineral stoichiometries used by NetpathXL.exe. 
NetpathXL looks for this file in the working directory (the directory 
containing the .xls or .pat file. If it is not found, a copy of the 
distributed version of this file, which is stored in the installation 
directory, is copied to the working directory.

Documentation for Netpath is available in the doc subdirectory of the installation 
directory or at http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/netpath.

Report bugs to dlpark@usgs.gov.