! MSO.f90

! This module contains the Automation interfaces of the objects defined in 
! C:\Program Files\Common Files\microsoft shared\OFFICE15\MSO.DLL
! Generated by the Fortran Module Wizard on 06/20/17

	MODULE MSO
		USE IFWINTY
		USE IFAUTO
		IMPLICIT NONE
	

		! CLSIDs		
		TYPE (GUID), PARAMETER :: CLSID_MsoEnvelope = &
			GUID(#0006F01A, #0000, #0000, &
			  CHAR('C0'X)//CHAR('00'X)//CHAR('00'X)//CHAR('00'X)// &
			  CHAR('00'X)//CHAR('00'X)//CHAR('00'X)//CHAR('46'X))
		TYPE (GUID), PARAMETER :: CLSID_CustomXMLPart = &
			GUID(#000CDB08, #0000, #0000, &
			  CHAR('C0'X)//CHAR('00'X)//CHAR('00'X)//CHAR('00'X)// &
			  CHAR('00'X)//CHAR('00'X)//CHAR('00'X)//CHAR('46'X))
		TYPE (GUID), PARAMETER :: CLSID_CustomXMLParts = &
			GUID(#000CDB0C, #0000, #0000, &
			  CHAR('C0'X)//CHAR('00'X)//CHAR('00'X)//CHAR('00'X)// &
			  CHAR('00'X)//CHAR('00'X)//CHAR('00'X)//CHAR('46'X))
		TYPE (GUID), PARAMETER :: CLSID_CustomXMLSchemaCollection = &
			GUID(#000CDB0D, #0000, #0000, &
			  CHAR('C0'X)//CHAR('00'X)//CHAR('00'X)//CHAR('00'X)// &
			  CHAR('00'X)//CHAR('00'X)//CHAR('00'X)//CHAR('46'X))
		TYPE (GUID), PARAMETER :: CLSID_CommandBarButton = &
			GUID(#55F88891, #7708, #11D1, &
			  CHAR('AC'X)//CHAR('EB'X)//CHAR('00'X)//CHAR('60'X)// &
			  CHAR('08'X)//CHAR('96'X)//CHAR('1D'X)//CHAR('A5'X))
		TYPE (GUID), PARAMETER :: CLSID_CommandBars = &
			GUID(#55F88893, #7708, #11D1, &
			  CHAR('AC'X)//CHAR('EB'X)//CHAR('00'X)//CHAR('60'X)// &
			  CHAR('08'X)//CHAR('96'X)//CHAR('1D'X)//CHAR('A5'X))
		TYPE (GUID), PARAMETER :: CLSID_CommandBarComboBox = &
			GUID(#55F88897, #7708, #11D1, &
			  CHAR('AC'X)//CHAR('EB'X)//CHAR('00'X)//CHAR('60'X)// &
			  CHAR('08'X)//CHAR('96'X)//CHAR('1D'X)//CHAR('A5'X))
		TYPE (GUID), PARAMETER :: CLSID_CustomTaskPane = &
			GUID(#C5771BE5, #A188, #466B, &
			  CHAR('AB'X)//CHAR('31'X)//CHAR('00'X)//CHAR('A6'X)// &
			  CHAR('A3'X)//CHAR('2B'X)//CHAR('1B'X)//CHAR('1C'X))

		! Enums		
		! MsoAppLanguageID 
			INTEGER, PARAMETER ::	msoLanguageIDInstall = 1	
			INTEGER, PARAMETER ::	msoLanguageIDUI = 2	
			INTEGER, PARAMETER ::	msoLanguageIDHelp = 3	
			INTEGER, PARAMETER ::	msoLanguageIDExeMode = 4	
			INTEGER, PARAMETER ::	msoLanguageIDUIPrevious = 5	
		! MsoLanguageID 
			INTEGER, PARAMETER ::	msoLanguageIDMixed = -2	
			INTEGER, PARAMETER ::	msoLanguageIDNone = 0	
			INTEGER, PARAMETER ::	msoLanguageIDNoProofing = 1024	
			INTEGER, PARAMETER ::	msoLanguageIDAfrikaans = 1078	
			INTEGER, PARAMETER ::	msoLanguageIDAlbanian = 1052	
			INTEGER, PARAMETER ::	msoLanguageIDAmharic = 1118	
			INTEGER, PARAMETER ::	msoLanguageIDArabicAlgeria = 5121	
			INTEGER, PARAMETER ::	msoLanguageIDArabicBahrain = 15361	
			INTEGER, PARAMETER ::	msoLanguageIDArabicEgypt = 3073	
			INTEGER, PARAMETER ::	msoLanguageIDArabicIraq = 2049	
			INTEGER, PARAMETER ::	msoLanguageIDArabicJordan = 11265	
			INTEGER, PARAMETER ::	msoLanguageIDArabicKuwait = 13313	
			INTEGER, PARAMETER ::	msoLanguageIDArabicLebanon = 12289	
			INTEGER, PARAMETER ::	msoLanguageIDArabicLibya = 4097	
			INTEGER, PARAMETER ::	msoLanguageIDArabicMorocco = 6145	
			INTEGER, PARAMETER ::	msoLanguageIDArabicOman = 8193	
			INTEGER, PARAMETER ::	msoLanguageIDArabicQatar = 16385	
			INTEGER, PARAMETER ::	msoLanguageIDArabic = 1025	
			INTEGER, PARAMETER ::	msoLanguageIDArabicSyria = 10241	
			INTEGER, PARAMETER ::	msoLanguageIDArabicTunisia = 7169	
			INTEGER, PARAMETER ::	msoLanguageIDArabicUAE = 14337	
			INTEGER, PARAMETER ::	msoLanguageIDArabicYemen = 9217	
			INTEGER, PARAMETER ::	msoLanguageIDArmenian = 1067	
			INTEGER, PARAMETER ::	msoLanguageIDAssamese = 1101	
			INTEGER, PARAMETER ::	msoLanguageIDAzeriCyrillic = 2092	
			INTEGER, PARAMETER ::	msoLanguageIDAzeriLatin = 1068	
			INTEGER, PARAMETER ::	msoLanguageIDBasque = 1069	
			INTEGER, PARAMETER ::	msoLanguageIDByelorussian = 1059	
			INTEGER, PARAMETER ::	msoLanguageIDBengali = 1093	
			INTEGER, PARAMETER ::	msoLanguageIDBosnian = 4122	
			INTEGER, PARAMETER ::	msoLanguageIDBosnianBosniaHerzegovinaCyrillic = 8218	
			INTEGER, PARAMETER ::	msoLanguageIDBosnianBosniaHerzegovinaLatin = 5146	
			INTEGER, PARAMETER ::	msoLanguageIDBulgarian = 1026	
			INTEGER, PARAMETER ::	msoLanguageIDBurmese = 1109	
			INTEGER, PARAMETER ::	msoLanguageIDCatalan = 1027	
			INTEGER, PARAMETER ::	msoLanguageIDChineseHongKongSAR = 3076	
			INTEGER, PARAMETER ::	msoLanguageIDChineseMacaoSAR = 5124	
			INTEGER, PARAMETER ::	msoLanguageIDSimplifiedChinese = 2052	
			INTEGER, PARAMETER ::	msoLanguageIDChineseSingapore = 4100	
			INTEGER, PARAMETER ::	msoLanguageIDTraditionalChinese = 1028	
			INTEGER, PARAMETER ::	msoLanguageIDCherokee = 1116	
			INTEGER, PARAMETER ::	msoLanguageIDCroatian = 1050	
			INTEGER, PARAMETER ::	msoLanguageIDCzech = 1029	
			INTEGER, PARAMETER ::	msoLanguageIDDanish = 1030	
			INTEGER, PARAMETER ::	msoLanguageIDDivehi = 1125	
			INTEGER, PARAMETER ::	msoLanguageIDBelgianDutch = 2067	
			INTEGER, PARAMETER ::	msoLanguageIDDutch = 1043	
			INTEGER, PARAMETER ::	msoLanguageIDDzongkhaBhutan = 2129	
			INTEGER, PARAMETER ::	msoLanguageIDEdo = 1126	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishAUS = 3081	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishBelize = 10249	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishCanadian = 4105	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishCaribbean = 9225	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishIndonesia = 14345	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishIreland = 6153	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishJamaica = 8201	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishNewZealand = 5129	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishPhilippines = 13321	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishSouthAfrica = 7177	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishTrinidadTobago = 11273	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishUK = 2057	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishUS = 1033	
			INTEGER, PARAMETER ::	msoLanguageIDEnglishZimbabwe = 12297	
			INTEGER, PARAMETER ::	msoLanguageIDEstonian = 1061	
			INTEGER, PARAMETER ::	msoLanguageIDFaeroese = 1080	
			INTEGER, PARAMETER ::	msoLanguageIDFarsi = 1065	
			INTEGER, PARAMETER ::	msoLanguageIDFilipino = 1124	
			INTEGER, PARAMETER ::	msoLanguageIDFinnish = 1035	
			INTEGER, PARAMETER ::	msoLanguageIDBelgianFrench = 2060	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchCameroon = 11276	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchCanadian = 3084	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchCotedIvoire = 12300	
			INTEGER, PARAMETER ::	msoLanguageIDFrench = 1036	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchHaiti = 15372	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchLuxembourg = 5132	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchMali = 13324	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchMonaco = 6156	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchMorocco = 14348	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchReunion = 8204	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchSenegal = 10252	
			INTEGER, PARAMETER ::	msoLanguageIDSwissFrench = 4108	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchWestIndies = 7180	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchZaire = 9228	
			INTEGER, PARAMETER ::	msoLanguageIDFrenchCongoDRC = 9228	
			INTEGER, PARAMETER ::	msoLanguageIDFrisianNetherlands = 1122	
			INTEGER, PARAMETER ::	msoLanguageIDFulfulde = 1127	
			INTEGER, PARAMETER ::	msoLanguageIDGaelicIreland = 2108	
			INTEGER, PARAMETER ::	msoLanguageIDGaelicScotland = 1084	
			INTEGER, PARAMETER ::	msoLanguageIDGalician = 1110	
			INTEGER, PARAMETER ::	msoLanguageIDGeorgian = 1079	
			INTEGER, PARAMETER ::	msoLanguageIDGermanAustria = 3079	
			INTEGER, PARAMETER ::	msoLanguageIDGerman = 1031	
			INTEGER, PARAMETER ::	msoLanguageIDGermanLiechtenstein = 5127	
			INTEGER, PARAMETER ::	msoLanguageIDGermanLuxembourg = 4103	
			INTEGER, PARAMETER ::	msoLanguageIDSwissGerman = 2055	
			INTEGER, PARAMETER ::	msoLanguageIDGreek = 1032	
			INTEGER, PARAMETER ::	msoLanguageIDGuarani = 1140	
			INTEGER, PARAMETER ::	msoLanguageIDGujarati = 1095	
			INTEGER, PARAMETER ::	msoLanguageIDHausa = 1128	
			INTEGER, PARAMETER ::	msoLanguageIDHawaiian = 1141	
			INTEGER, PARAMETER ::	msoLanguageIDHebrew = 1037	
			INTEGER, PARAMETER ::	msoLanguageIDHindi = 1081	
			INTEGER, PARAMETER ::	msoLanguageIDHungarian = 1038	
			INTEGER, PARAMETER ::	msoLanguageIDIbibio = 1129	
			INTEGER, PARAMETER ::	msoLanguageIDIcelandic = 1039	
			INTEGER, PARAMETER ::	msoLanguageIDIgbo = 1136	
			INTEGER, PARAMETER ::	msoLanguageIDIndonesian = 1057	
			INTEGER, PARAMETER ::	msoLanguageIDInuktitut = 1117	
			INTEGER, PARAMETER ::	msoLanguageIDItalian = 1040	
			INTEGER, PARAMETER ::	msoLanguageIDSwissItalian = 2064	
			INTEGER, PARAMETER ::	msoLanguageIDJapanese = 1041	
			INTEGER, PARAMETER ::	msoLanguageIDKannada = 1099	
			INTEGER, PARAMETER ::	msoLanguageIDKanuri = 1137	
			INTEGER, PARAMETER ::	msoLanguageIDKashmiri = 1120	
			INTEGER, PARAMETER ::	msoLanguageIDKashmiriDevanagari = 2144	
			INTEGER, PARAMETER ::	msoLanguageIDKazakh = 1087	
			INTEGER, PARAMETER ::	msoLanguageIDKhmer = 1107	
			INTEGER, PARAMETER ::	msoLanguageIDKirghiz = 1088	
			INTEGER, PARAMETER ::	msoLanguageIDKonkani = 1111	
			INTEGER, PARAMETER ::	msoLanguageIDKorean = 1042	
			INTEGER, PARAMETER ::	msoLanguageIDKyrgyz = 1088	
			INTEGER, PARAMETER ::	msoLanguageIDLatin = 1142	
			INTEGER, PARAMETER ::	msoLanguageIDLao = 1108	
			INTEGER, PARAMETER ::	msoLanguageIDLatvian = 1062	
			INTEGER, PARAMETER ::	msoLanguageIDLithuanian = 1063	
			INTEGER, PARAMETER ::	msoLanguageIDMacedonian = 1071	
			INTEGER, PARAMETER ::	msoLanguageIDMacedonianFYROM = 1071	
			INTEGER, PARAMETER ::	msoLanguageIDMalaysian = 1086	
			INTEGER, PARAMETER ::	msoLanguageIDMalayBruneiDarussalam = 2110	
			INTEGER, PARAMETER ::	msoLanguageIDMalayalam = 1100	
			INTEGER, PARAMETER ::	msoLanguageIDMaltese = 1082	
			INTEGER, PARAMETER ::	msoLanguageIDManipuri = 1112	
			INTEGER, PARAMETER ::	msoLanguageIDMaori = 1153	
			INTEGER, PARAMETER ::	msoLanguageIDMarathi = 1102	
			INTEGER, PARAMETER ::	msoLanguageIDMongolian = 1104	
			INTEGER, PARAMETER ::	msoLanguageIDNepali = 1121	
			INTEGER, PARAMETER ::	msoLanguageIDNorwegianBokmol = 1044	
			INTEGER, PARAMETER ::	msoLanguageIDNorwegianNynorsk = 2068	
			INTEGER, PARAMETER ::	msoLanguageIDOriya = 1096	
			INTEGER, PARAMETER ::	msoLanguageIDOromo = 1138	
			INTEGER, PARAMETER ::	msoLanguageIDPashto = 1123	
			INTEGER, PARAMETER ::	msoLanguageIDPolish = 1045	
			INTEGER, PARAMETER ::	msoLanguageIDBrazilianPortuguese = 1046	
			INTEGER, PARAMETER ::	msoLanguageIDPortuguese = 2070	
			INTEGER, PARAMETER ::	msoLanguageIDPunjabi = 1094	
			INTEGER, PARAMETER ::	msoLanguageIDQuechuaBolivia = 1131	
			INTEGER, PARAMETER ::	msoLanguageIDQuechuaEcuador = 2155	
			INTEGER, PARAMETER ::	msoLanguageIDQuechuaPeru = 3179	
			INTEGER, PARAMETER ::	msoLanguageIDRhaetoRomanic = 1047	
			INTEGER, PARAMETER ::	msoLanguageIDRomanianMoldova = 2072	
			INTEGER, PARAMETER ::	msoLanguageIDRomanian = 1048	
			INTEGER, PARAMETER ::	msoLanguageIDRussianMoldova = 2073	
			INTEGER, PARAMETER ::	msoLanguageIDRussian = 1049	
			INTEGER, PARAMETER ::	msoLanguageIDSamiLappish = 1083	
			INTEGER, PARAMETER ::	msoLanguageIDSanskrit = 1103	
			INTEGER, PARAMETER ::	msoLanguageIDSepedi = 1132	
			INTEGER, PARAMETER ::	msoLanguageIDSerbianBosniaHerzegovinaCyrillic = 7194	
			INTEGER, PARAMETER ::	msoLanguageIDSerbianBosniaHerzegovinaLatin = 6170	
			INTEGER, PARAMETER ::	msoLanguageIDSerbianCyrillic = 3098	
			INTEGER, PARAMETER ::	msoLanguageIDSerbianLatin = 2074	
			INTEGER, PARAMETER ::	msoLanguageIDSesotho = 1072	
			INTEGER, PARAMETER ::	msoLanguageIDSindhi = 1113	
			INTEGER, PARAMETER ::	msoLanguageIDSindhiPakistan = 2137	
			INTEGER, PARAMETER ::	msoLanguageIDSinhalese = 1115	
			INTEGER, PARAMETER ::	msoLanguageIDSlovak = 1051	
			INTEGER, PARAMETER ::	msoLanguageIDSlovenian = 1060	
			INTEGER, PARAMETER ::	msoLanguageIDSomali = 1143	
			INTEGER, PARAMETER ::	msoLanguageIDSorbian = 1070	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishArgentina = 11274	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishBolivia = 16394	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishChile = 13322	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishColombia = 9226	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishCostaRica = 5130	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishDominicanRepublic = 7178	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishEcuador = 12298	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishElSalvador = 17418	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishGuatemala = 4106	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishHonduras = 18442	
			INTEGER, PARAMETER ::	msoLanguageIDMexicanSpanish = 2058	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishNicaragua = 19466	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishPanama = 6154	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishParaguay = 15370	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishPeru = 10250	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishPuertoRico = 20490	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishModernSort = 3082	
			INTEGER, PARAMETER ::	msoLanguageIDSpanish = 1034	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishUruguay = 14346	
			INTEGER, PARAMETER ::	msoLanguageIDSpanishVenezuela = 8202	
			INTEGER, PARAMETER ::	msoLanguageIDSutu = 1072	
			INTEGER, PARAMETER ::	msoLanguageIDSwahili = 1089	
			INTEGER, PARAMETER ::	msoLanguageIDSwedishFinland = 2077	
			INTEGER, PARAMETER ::	msoLanguageIDSwedish = 1053	
			INTEGER, PARAMETER ::	msoLanguageIDSyriac = 1114	
			INTEGER, PARAMETER ::	msoLanguageIDTajik = 1064	
			INTEGER, PARAMETER ::	msoLanguageIDTamil = 1097	
			INTEGER, PARAMETER ::	msoLanguageIDTamazight = 1119	
			INTEGER, PARAMETER ::	msoLanguageIDTamazightLatin = 2143	
			INTEGER, PARAMETER ::	msoLanguageIDTatar = 1092	
			INTEGER, PARAMETER ::	msoLanguageIDTelugu = 1098	
			INTEGER, PARAMETER ::	msoLanguageIDThai = 1054	
			INTEGER, PARAMETER ::	msoLanguageIDTibetan = 1105	
			INTEGER, PARAMETER ::	msoLanguageIDTigrignaEthiopic = 1139	
			INTEGER, PARAMETER ::	msoLanguageIDTigrignaEritrea = 2163	
			INTEGER, PARAMETER ::	msoLanguageIDTsonga = 1073	
			INTEGER, PARAMETER ::	msoLanguageIDTswana = 1074	
			INTEGER, PARAMETER ::	msoLanguageIDTurkish = 1055	
			INTEGER, PARAMETER ::	msoLanguageIDTurkmen = 1090	
			INTEGER, PARAMETER ::	msoLanguageIDUkrainian = 1058	
			INTEGER, PARAMETER ::	msoLanguageIDUrdu = 1056	
			INTEGER, PARAMETER ::	msoLanguageIDUzbekCyrillic = 2115	
			INTEGER, PARAMETER ::	msoLanguageIDUzbekLatin = 1091	
			INTEGER, PARAMETER ::	msoLanguageIDVenda = 1075	
			INTEGER, PARAMETER ::	msoLanguageIDVietnamese = 1066	
			INTEGER, PARAMETER ::	msoLanguageIDWelsh = 1106	
			INTEGER, PARAMETER ::	msoLanguageIDXhosa = 1076	
			INTEGER, PARAMETER ::	msoLanguageIDYi = 1144	
			INTEGER, PARAMETER ::	msoLanguageIDYiddish = 1085	
			INTEGER, PARAMETER ::	msoLanguageIDYoruba = 1130	
			INTEGER, PARAMETER ::	msoLanguageIDZulu = 1077	


		! Module Procedures
		CONTAINS
  			INTEGER(INT_PTR_KIND()) FUNCTION LanguageSettings_GetApplication($OBJECT, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Application', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 1610743808, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				LanguageSettings_GetApplication = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION LanguageSettings_GetApplication
  			INTEGER(4) FUNCTION LanguageSettings_GetCreator($OBJECT, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(4), VOLATILE :: $RETURN
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Creator', $RETURN, AUTO_ARG_OUT)
				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 1610743809, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				LanguageSettings_GetCreator = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION LanguageSettings_GetCreator
  			INTEGER FUNCTION LanguageSettings_GetLanguageID($OBJECT, Id, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER, INTENT(IN)	:: Id	! MsoAppLanguageID
				!DEC$ ATTRIBUTES REFERENCE	:: Id
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER, VOLATILE :: $RETURN
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'LanguageID', $RETURN, AUTO_ARG_OUT)
				CALL AUTOADDARG(invokeargs, '$ARG1', Id)
				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 1, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				LanguageSettings_GetLanguageID = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION LanguageSettings_GetLanguageID
  			LOGICAL(2) FUNCTION LanguageSettings_GetLanguagePreferredForEditing($OBJECT, lid, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER, INTENT(IN)	:: lid	! MsoLanguageID
				!DEC$ ATTRIBUTES REFERENCE	:: lid
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				LOGICAL(2), VOLATILE :: $RETURN
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'LanguagePreferredForEditing', $RETURN, AUTO_ARG_OUT)
				CALL AUTOADDARG(invokeargs, '$ARG1', lid)
				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 2, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				LanguageSettings_GetLanguagePreferredForEditing = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION LanguageSettings_GetLanguagePreferredForEditing
  			INTEGER(INT_PTR_KIND()) FUNCTION LanguageSettings_GetParent($OBJECT, $STATUS)
				IMPLICIT NONE
	
				INTEGER(INT_PTR_KIND()), INTENT(IN)	:: $OBJECT	 ! Object Pointer
				!DEC$ ATTRIBUTES VALUE	:: $OBJECT
				INTEGER(4), INTENT(OUT), OPTIONAL	:: $STATUS	 ! Method status
				!DEC$ ATTRIBUTES REFERENCE			:: $STATUS
				INTEGER(INT_PTR_KIND()), VOLATILE :: $RETURN
				INTEGER(4) $$STATUS
				INTEGER(INT_PTR_KIND()) invokeargs
				invokeargs = AUTOALLOCATEINVOKEARGS()
				CALL AUTOADDARG(invokeargs, 'Parent', $RETURN, AUTO_ARG_OUT, VT_DISPATCH)
				$$STATUS = AUTOGETPROPERTYBYID($OBJECT, 3, invokeargs)
				IF (PRESENT($STATUS)) $STATUS = $$STATUS
				LanguageSettings_GetParent = $RETURN
				CALL AUTODEALLOCATEINVOKEARGS (invokeargs)
			END FUNCTION LanguageSettings_GetParent
	END MODULE
