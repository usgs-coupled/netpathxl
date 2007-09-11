all: dist

dist:
	#rm -rf distribution
	mkdir -p distribution
	cp dbxl/debug/dbxl.exe distribution/DbXL.exe
	cp release/netpathxl.exe distribution/NetpathXl.exe 
	cp netpath.dat db.dat README.TXT distribution
	
