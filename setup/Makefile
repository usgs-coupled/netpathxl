CANDLE = candle
CANDLE_FLAGS=\
	-nologo \
	-dFlavor=Release

LIGHT = light
LIGHT_FLAGS=\
	-nologo

IntDir = obj/Release
OutDir = bin/Release

all : $(OutDir)/netpathxl.msi

$(OutDir)/netpathxl.msi : $(IntDir)/netpathxl.wixobj $(IntDir)/Product.wixobj
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