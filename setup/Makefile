CANDLE = candle
CANDLE_FLAGS=\
	-nologo \
	-dFlavor=Release

LIGHT = light
LIGHT_FLAGS=\
	-nologo

IntDir = obj/Release
OutDir = bin/Release

EXAMPLE_PATH=../Examples

EXAMPLES=\
	$(EXAMPLE_PATH)/class.lon \
	$(EXAMPLE_PATH)/class.out \
	$(EXAMPLE_PATH)/class.pat \
	$(EXAMPLE_PATH)/class.xls \
	$(EXAMPLE_PATH)/db.fil \
	$(EXAMPLE_PATH)/model.fil \
	$(EXAMPLE_PATH)/netpath.fil \
	$(EXAMPLE_PATH)/netpath.out \
	$(EXAMPLE_PATH)/np-1a.out \
	$(EXAMPLE_PATH)/np-1b.out \
	$(EXAMPLE_PATH)/np-1c.out \
	$(EXAMPLE_PATH)/np-2.out \
	$(EXAMPLE_PATH)/np-3-1.out \
	$(EXAMPLE_PATH)/np-3-2.out \
	$(EXAMPLE_PATH)/np-3-3a.out \
	$(EXAMPLE_PATH)/np-3-3b.out \
	$(EXAMPLE_PATH)/np-3-4.out \
	$(EXAMPLE_PATH)/np-3-5.out \
	$(EXAMPLE_PATH)/np-3-6.out \
	$(EXAMPLE_PATH)/np-3-7.out \
	$(EXAMPLE_PATH)/np-4.out \
	$(EXAMPLE_PATH)/np-5-1.out \
	$(EXAMPLE_PATH)/np-5-2.out \
	$(EXAMPLE_PATH)/np-5-3.out \
	$(EXAMPLE_PATH)/np-5-4.out \
	$(EXAMPLE_PATH)/np-5-5.out \
	$(EXAMPLE_PATH)/np-6.out \
	$(EXAMPLE_PATH)/np-7.out \
	$(EXAMPLE_PATH)/np-1a.dat \
	$(EXAMPLE_PATH)/np-1b.dat \
	$(EXAMPLE_PATH)/np-1c.dat \
	$(EXAMPLE_PATH)/np-2.dat \
	$(EXAMPLE_PATH)/np-3-1.dat \
	$(EXAMPLE_PATH)/np-3-2.dat \
	$(EXAMPLE_PATH)/np-3-3a.dat \
	$(EXAMPLE_PATH)/np-3-3b.dat \
	$(EXAMPLE_PATH)/np-3-4.dat \
	$(EXAMPLE_PATH)/np-3-5.dat \
	$(EXAMPLE_PATH)/np-3-6.dat \
	$(EXAMPLE_PATH)/np-3-7.dat \
	$(EXAMPLE_PATH)/np-4.dat \
	$(EXAMPLE_PATH)/np-5-1.dat \
	$(EXAMPLE_PATH)/np-5-2.dat \
	$(EXAMPLE_PATH)/np-5-3.dat \
	$(EXAMPLE_PATH)/np-5-4.dat \
	$(EXAMPLE_PATH)/np-5-5.dat \
	$(EXAMPLE_PATH)/np-6.dat \
	$(EXAMPLE_PATH)/np-7.dat

EXECUTABLES=\
	../bin/dbxl.exe \
	../bin/netpathxl.exe	


all : $(OutDir)/netpathxl.msi

$(OutDir)/netpathxl.msi : $(IntDir)/netpathxl.wixobj $(IntDir)/Product.wixobj $(EXAMPLES) $(EXECUTABLES)
	mkdir -p $(OutDir)
	$(LIGHT) $(LIGHT_FLAGS) -out $@ $(IntDir)/netpathxl.wixobj $(IntDir)/Product.wixobj "$(PATH_WIX)\wixui.wixlib" -loc "$(PATH_WIX)\WixUI_en-us.wxl"

$(IntDir)/netpathxl.wixobj : netpathxl.wxs Guids.wxi License.rtf
	mkdir -p $(IntDir)
	$(CANDLE) $(CANDLE_FLAGS) -out $@ netpathxl.wxs

$(IntDir)/Product.wixobj : Product.wxs Guids.wxi
	mkdir -p $(IntDir)
	$(CANDLE) $(CANDLE_FLAGS) -out $@ Product.wxs
	
clean:
	rm -f $(IntDir)/hti.wixobj $(IntDir)/Product.wixobj
