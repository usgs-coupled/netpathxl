NetpathXL @VER@                                @RELEASE_DATE@

To install NetpathXL, double-click NetpathXL-@VER@-@REL@.msi as
a user with administrator privileges.

Two programs are distributed, DBXL and NetpathXL. They are normally
installed in the directory c:\program files\usgs\NetpathXL\bin. They
can be run from the start menu or by browsing for the executables. A
shortcut can be added to the desktop by right-clicking on the
executable and selecting Send To->Desktop.

The program DbXL.exe has four functions: (1) to convert a .lon file
(created by Netpath or PHREEQC) to an Excel file, (2) to open an
existing NetpathXL file (.xls), (3) to create a new NetpathXL file
(.xls), and (4) to create a PHREEQC input file (.pqi) from an existing
NetpathXL file (.xls). All the basic chemical data for Netpath
calculations are stored in the Excel file.  Once you have you have
NetpathXL spreadsheet files, simply copy and modify the Excel files as
needed to generate new spreadsheet files.  The columns of the
NetpathXL spreadsheet are fixed and can not be changed.

NetpathXL.exe will read an old .pat file (created by DB), a new .pat
file (created by NetpathXL), a new .pat file created by PHREEQC, or a
NetpathXL spreadsheet file (.xls). Once a file has been read, all the
usual NETPATH calculations can be performed. The file db.dat is used
to speciate the data in the Excel file at the beginning of running
NetpathXL.exe. This database file is stored in the installation
directory. The file Netpath.dat contains mineral stoichiometries used
by NetpathXL.exe.  NetpathXL looks for this file in the working
directory (the directory containing the .xls or .pat file). If it is
not found, the distributed version of this file, which is
stored in the installation directory, is copied to the working
directory. The calculation engine and all model development in
NetpathXL is identical to Netpath.

Documentation for Netpath is available in the doc subdirectory of the
installation directory or at 
http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/netpath.

Report bugs to dlpark@usgs.gov.