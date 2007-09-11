NetpathXL 1.0 (svn 2065)
June 1, 2007

Here are the four files that are necessary to run DbXL and NetpathXL. 

It is probably easiest just to put  all of them in a working directory 
that will contain your basic data and Netpath results.

The program DbXL.exe has two functions (1) to create a new Excel file 
that can be used by NetpathXL.exe and (2) to convert an old .lon file 
to an Excel file. All the basic chemical data are stored in the Excel file.  
Once you have done one of these two functions, you probably will not run 
DbXL.exe much; simply copy and modify Excel files as needed.  The columns 
of the Excel file are fixed and can not be changed.  

NetpathXL.exe will read an old .pat file or the new Excel file and do all 
the usual netpath calculations. db.dat is used to speciate the data in the 
Excel file at the beginning of running NetpathXL.exe. Netpath.dat is a file 
of mineral stoichiometries used by NetpathXL.exe.

Documentation for Netpath is available at 
http://wwwbrr.cr.usgs.gov/projects/GWC_coupled/netpath.

Report bugs to dlpark@usgs.gov.