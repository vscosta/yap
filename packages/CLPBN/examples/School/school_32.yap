/*
total_professors(32).

total_courses(64).

total_students(256).
*/

:- use_module(library(pfl)).

:- source.

:- style_check(all).

:- set_prolog_flag(unknown,error).

:- set_prolog_flag(write_strings,on).

:- ensure_loaded('parschema.pfl').

professor(p0).
professor(p1).
professor(p2).
professor(p3).
professor(p4).
professor(p5).
professor(p6).
professor(p7).
professor(p8).
professor(p9).
professor(p10).
professor(p11).
professor(p12).
professor(p13).
professor(p14).
professor(p15).
professor(p16).
professor(p17).
professor(p18).
professor(p19).
professor(p20).
professor(p21).
professor(p22).
professor(p23).
professor(p24).
professor(p25).
professor(p26).
professor(p27).
professor(p28).
professor(p29).
professor(p30).
professor(p31).


course(c0,p24).
course(c1,p7).
course(c2,p16).
course(c3,p27).
course(c4,p25).
course(c5,p6).
course(c6,p28).
course(c7,p1).
course(c8,p29).
course(c9,p23).
course(c10,p17).
course(c11,p16).
course(c12,p11).
course(c13,p28).
course(c14,p13).
course(c15,p7).
course(c16,p21).
course(c17,p15).
course(c18,p8).
course(c19,p30).
course(c20,p1).
course(c21,p23).
course(c22,p11).
course(c23,p9).
course(c24,p0).
course(c25,p30).
course(c26,p15).
course(c27,p4).
course(c28,p26).
course(c29,p29).
course(c30,p31).
course(c31,p19).
course(c32,p5).
course(c33,p14).
course(c34,p14).
course(c35,p25).
course(c36,p21).
course(c37,p10).
course(c38,p2).
course(c39,p20).
course(c40,p3).
course(c41,p18).
course(c42,p9).
course(c43,p20).
course(c44,p17).
course(c45,p19).
course(c46,p6).
course(c47,p4).
course(c48,p12).
course(c49,p10).
course(c50,p2).
course(c51,p22).
course(c52,p31).
course(c53,p24).
course(c54,p0).
course(c55,p5).
course(c56,p22).
course(c57,p13).
course(c58,p18).
course(c59,p12).
course(c60,p27).
course(c61,p3).
course(c62,p8).
course(c63,p26).


student(s0).
student(s1).
student(s2).
student(s3).
student(s4).
student(s5).
student(s6).
student(s7).
student(s8).
student(s9).
student(s10).
student(s11).
student(s12).
student(s13).
student(s14).
student(s15).
student(s16).
student(s17).
student(s18).
student(s19).
student(s20).
student(s21).
student(s22).
student(s23).
student(s24).
student(s25).
student(s26).
student(s27).
student(s28).
student(s29).
student(s30).
student(s31).
student(s32).
student(s33).
student(s34).
student(s35).
student(s36).
student(s37).
student(s38).
student(s39).
student(s40).
student(s41).
student(s42).
student(s43).
student(s44).
student(s45).
student(s46).
student(s47).
student(s48).
student(s49).
student(s50).
student(s51).
student(s52).
student(s53).
student(s54).
student(s55).
student(s56).
student(s57).
student(s58).
student(s59).
student(s60).
student(s61).
student(s62).
student(s63).
student(s64).
student(s65).
student(s66).
student(s67).
student(s68).
student(s69).
student(s70).
student(s71).
student(s72).
student(s73).
student(s74).
student(s75).
student(s76).
student(s77).
student(s78).
student(s79).
student(s80).
student(s81).
student(s82).
student(s83).
student(s84).
student(s85).
student(s86).
student(s87).
student(s88).
student(s89).
student(s90).
student(s91).
student(s92).
student(s93).
student(s94).
student(s95).
student(s96).
student(s97).
student(s98).
student(s99).
student(s100).
student(s101).
student(s102).
student(s103).
student(s104).
student(s105).
student(s106).
student(s107).
student(s108).
student(s109).
student(s110).
student(s111).
student(s112).
student(s113).
student(s114).
student(s115).
student(s116).
student(s117).
student(s118).
student(s119).
student(s120).
student(s121).
student(s122).
student(s123).
student(s124).
student(s125).
student(s126).
student(s127).
student(s128).
student(s129).
student(s130).
student(s131).
student(s132).
student(s133).
student(s134).
student(s135).
student(s136).
student(s137).
student(s138).
student(s139).
student(s140).
student(s141).
student(s142).
student(s143).
student(s144).
student(s145).
student(s146).
student(s147).
student(s148).
student(s149).
student(s150).
student(s151).
student(s152).
student(s153).
student(s154).
student(s155).
student(s156).
student(s157).
student(s158).
student(s159).
student(s160).
student(s161).
student(s162).
student(s163).
student(s164).
student(s165).
student(s166).
student(s167).
student(s168).
student(s169).
student(s170).
student(s171).
student(s172).
student(s173).
student(s174).
student(s175).
student(s176).
student(s177).
student(s178).
student(s179).
student(s180).
student(s181).
student(s182).
student(s183).
student(s184).
student(s185).
student(s186).
student(s187).
student(s188).
student(s189).
student(s190).
student(s191).
student(s192).
student(s193).
student(s194).
student(s195).
student(s196).
student(s197).
student(s198).
student(s199).
student(s200).
student(s201).
student(s202).
student(s203).
student(s204).
student(s205).
student(s206).
student(s207).
student(s208).
student(s209).
student(s210).
student(s211).
student(s212).
student(s213).
student(s214).
student(s215).
student(s216).
student(s217).
student(s218).
student(s219).
student(s220).
student(s221).
student(s222).
student(s223).
student(s224).
student(s225).
student(s226).
student(s227).
student(s228).
student(s229).
student(s230).
student(s231).
student(s232).
student(s233).
student(s234).
student(s235).
student(s236).
student(s237).
student(s238).
student(s239).
student(s240).
student(s241).
student(s242).
student(s243).
student(s244).
student(s245).
student(s246).
student(s247).
student(s248).
student(s249).
student(s250).
student(s251).
student(s252).
student(s253).
student(s254).
student(s255).


registration(r0,c16,s0).
registration(r1,c10,s0).
registration(r2,c57,s0).
registration(r3,c22,s1).
registration(r4,c55,s1).
registration(r5,c27,s1).
registration(r6,c14,s2).
registration(r7,c52,s2).
registration(r8,c10,s2).
registration(r9,c47,s3).
registration(r10,c16,s3).
registration(r11,c62,s3).
registration(r12,c12,s4).
registration(r13,c11,s4).
registration(r14,c17,s4).
registration(r15,c52,s5).
registration(r16,c1,s5).
registration(r17,c35,s5).
registration(r18,c0,s6).
registration(r19,c7,s6).
registration(r20,c40,s6).
registration(r21,c62,s7).
registration(r22,c16,s7).
registration(r23,c34,s7).
registration(r24,c60,s8).
registration(r25,c39,s8).
registration(r26,c43,s8).
registration(r27,c17,s8).
registration(r28,c55,s9).
registration(r29,c34,s9).
registration(r30,c35,s9).
registration(r31,c48,s10).
registration(r32,c60,s10).
registration(r33,c2,s10).
registration(r34,c54,s11).
registration(r35,c42,s11).
registration(r36,c9,s11).
registration(r37,c11,s11).
registration(r38,c51,s12).
registration(r39,c47,s12).
registration(r40,c4,s12).
registration(r41,c20,s13).
registration(r42,c0,s13).
registration(r43,c9,s13).
registration(r44,c61,s13).
registration(r45,c55,s14).
registration(r46,c5,s14).
registration(r47,c3,s14).
registration(r48,c38,s15).
registration(r49,c1,s15).
registration(r50,c46,s15).
registration(r51,c42,s16).
registration(r52,c27,s16).
registration(r53,c26,s16).
registration(r54,c6,s17).
registration(r55,c27,s17).
registration(r56,c0,s17).
registration(r57,c51,s18).
registration(r58,c63,s18).
registration(r59,c41,s18).
registration(r60,c63,s19).
registration(r61,c18,s19).
registration(r62,c54,s19).
registration(r63,c15,s19).
registration(r64,c3,s20).
registration(r65,c22,s20).
registration(r66,c43,s20).
registration(r67,c17,s21).
registration(r68,c34,s21).
registration(r69,c0,s21).
registration(r70,c42,s22).
registration(r71,c7,s22).
registration(r72,c46,s22).
registration(r73,c52,s23).
registration(r74,c21,s23).
registration(r75,c27,s23).
registration(r76,c60,s23).
registration(r77,c2,s24).
registration(r78,c38,s24).
registration(r79,c32,s24).
registration(r80,c51,s25).
registration(r81,c39,s25).
registration(r82,c49,s25).
registration(r83,c15,s25).
registration(r84,c25,s26).
registration(r85,c59,s26).
registration(r86,c21,s26).
registration(r87,c56,s27).
registration(r88,c43,s27).
registration(r89,c55,s27).
registration(r90,c21,s27).
registration(r91,c57,s28).
registration(r92,c17,s28).
registration(r93,c26,s28).
registration(r94,c53,s29).
registration(r95,c48,s29).
registration(r96,c34,s29).
registration(r97,c50,s29).
registration(r98,c35,s29).
registration(r99,c36,s30).
registration(r100,c48,s30).
registration(r101,c23,s30).
registration(r102,c33,s30).
registration(r103,c28,s31).
registration(r104,c23,s31).
registration(r105,c53,s31).
registration(r106,c32,s32).
registration(r107,c37,s32).
registration(r108,c38,s32).
registration(r109,c27,s32).
registration(r110,c34,s33).
registration(r111,c28,s33).
registration(r112,c43,s33).
registration(r113,c24,s34).
registration(r114,c28,s34).
registration(r115,c45,s34).
registration(r116,c63,s34).
registration(r117,c60,s35).
registration(r118,c9,s35).
registration(r119,c22,s35).
registration(r120,c46,s36).
registration(r121,c44,s36).
registration(r122,c26,s36).
registration(r123,c9,s36).
registration(r124,c62,s37).
registration(r125,c33,s37).
registration(r126,c31,s37).
registration(r127,c6,s37).
registration(r128,c43,s38).
registration(r129,c35,s38).
registration(r130,c56,s38).
registration(r131,c32,s38).
registration(r132,c2,s39).
registration(r133,c56,s39).
registration(r134,c58,s39).
registration(r135,c3,s40).
registration(r136,c29,s40).
registration(r137,c22,s40).
registration(r138,c39,s40).
registration(r139,c32,s41).
registration(r140,c31,s41).
registration(r141,c25,s41).
registration(r142,c36,s42).
registration(r143,c24,s42).
registration(r144,c47,s42).
registration(r145,c15,s43).
registration(r146,c1,s43).
registration(r147,c2,s43).
registration(r148,c37,s44).
registration(r149,c14,s44).
registration(r150,c60,s44).
registration(r151,c62,s45).
registration(r152,c29,s45).
registration(r153,c43,s45).
registration(r154,c46,s45).
registration(r155,c57,s46).
registration(r156,c25,s46).
registration(r157,c46,s46).
registration(r158,c15,s46).
registration(r159,c0,s47).
registration(r160,c33,s47).
registration(r161,c30,s47).
registration(r162,c55,s47).
registration(r163,c43,s48).
registration(r164,c38,s48).
registration(r165,c50,s48).
registration(r166,c51,s49).
registration(r167,c17,s49).
registration(r168,c23,s49).
registration(r169,c18,s50).
registration(r170,c63,s50).
registration(r171,c16,s50).
registration(r172,c60,s51).
registration(r173,c44,s51).
registration(r174,c32,s51).
registration(r175,c59,s52).
registration(r176,c26,s52).
registration(r177,c7,s52).
registration(r178,c42,s52).
registration(r179,c33,s52).
registration(r180,c9,s53).
registration(r181,c5,s53).
registration(r182,c39,s53).
registration(r183,c49,s53).
registration(r184,c50,s54).
registration(r185,c43,s54).
registration(r186,c55,s54).
registration(r187,c14,s55).
registration(r188,c0,s55).
registration(r189,c31,s55).
registration(r190,c47,s55).
registration(r191,c50,s56).
registration(r192,c29,s56).
registration(r193,c42,s56).
registration(r194,c22,s57).
registration(r195,c7,s57).
registration(r196,c59,s57).
registration(r197,c60,s58).
registration(r198,c26,s58).
registration(r199,c23,s58).
registration(r200,c29,s59).
registration(r201,c4,s59).
registration(r202,c38,s59).
registration(r203,c24,s60).
registration(r204,c59,s60).
registration(r205,c9,s60).
registration(r206,c23,s61).
registration(r207,c63,s61).
registration(r208,c22,s61).
registration(r209,c6,s62).
registration(r210,c43,s62).
registration(r211,c30,s62).
registration(r212,c29,s62).
registration(r213,c7,s63).
registration(r214,c19,s63).
registration(r215,c25,s63).
registration(r216,c20,s64).
registration(r217,c19,s64).
registration(r218,c57,s64).
registration(r219,c22,s65).
registration(r220,c55,s65).
registration(r221,c30,s65).
registration(r222,c53,s65).
registration(r223,c32,s66).
registration(r224,c19,s66).
registration(r225,c53,s66).
registration(r226,c53,s67).
registration(r227,c48,s67).
registration(r228,c35,s67).
registration(r229,c15,s68).
registration(r230,c20,s68).
registration(r231,c41,s68).
registration(r232,c49,s69).
registration(r233,c44,s69).
registration(r234,c9,s69).
registration(r235,c30,s70).
registration(r236,c61,s70).
registration(r237,c8,s70).
registration(r238,c31,s71).
registration(r239,c8,s71).
registration(r240,c20,s71).
registration(r241,c18,s71).
registration(r242,c38,s71).
registration(r243,c37,s72).
registration(r244,c0,s72).
registration(r245,c62,s72).
registration(r246,c47,s73).
registration(r247,c53,s73).
registration(r248,c41,s73).
registration(r249,c62,s73).
registration(r250,c16,s74).
registration(r251,c31,s74).
registration(r252,c48,s74).
registration(r253,c11,s74).
registration(r254,c43,s75).
registration(r255,c26,s75).
registration(r256,c22,s75).
registration(r257,c40,s76).
registration(r258,c31,s76).
registration(r259,c2,s76).
registration(r260,c7,s77).
registration(r261,c3,s77).
registration(r262,c63,s77).
registration(r263,c0,s78).
registration(r264,c43,s78).
registration(r265,c57,s78).
registration(r266,c46,s79).
registration(r267,c32,s79).
registration(r268,c1,s79).
registration(r269,c18,s80).
registration(r270,c17,s80).
registration(r271,c14,s80).
registration(r272,c8,s81).
registration(r273,c63,s81).
registration(r274,c56,s81).
registration(r275,c28,s82).
registration(r276,c8,s82).
registration(r277,c9,s82).
registration(r278,c61,s83).
registration(r279,c38,s83).
registration(r280,c60,s83).
registration(r281,c38,s84).
registration(r282,c60,s84).
registration(r283,c63,s84).
registration(r284,c25,s85).
registration(r285,c13,s85).
registration(r286,c20,s85).
registration(r287,c52,s85).
registration(r288,c45,s86).
registration(r289,c17,s86).
registration(r290,c2,s86).
registration(r291,c48,s86).
registration(r292,c0,s86).
registration(r293,c40,s87).
registration(r294,c44,s87).
registration(r295,c41,s87).
registration(r296,c53,s87).
registration(r297,c37,s88).
registration(r298,c47,s88).
registration(r299,c48,s88).
registration(r300,c44,s89).
registration(r301,c32,s89).
registration(r302,c18,s89).
registration(r303,c50,s90).
registration(r304,c26,s90).
registration(r305,c58,s90).
registration(r306,c45,s90).
registration(r307,c0,s91).
registration(r308,c35,s91).
registration(r309,c4,s91).
registration(r310,c4,s92).
registration(r311,c1,s92).
registration(r312,c49,s92).
registration(r313,c42,s92).
registration(r314,c47,s93).
registration(r315,c48,s93).
registration(r316,c17,s93).
registration(r317,c1,s94).
registration(r318,c18,s94).
registration(r319,c35,s94).
registration(r320,c3,s95).
registration(r321,c0,s95).
registration(r322,c38,s95).
registration(r323,c1,s96).
registration(r324,c30,s96).
registration(r325,c52,s96).
registration(r326,c52,s97).
registration(r327,c18,s97).
registration(r328,c55,s97).
registration(r329,c56,s97).
registration(r330,c8,s98).
registration(r331,c41,s98).
registration(r332,c1,s98).
registration(r333,c49,s98).
registration(r334,c35,s99).
registration(r335,c23,s99).
registration(r336,c18,s99).
registration(r337,c36,s100).
registration(r338,c27,s100).
registration(r339,c6,s100).
registration(r340,c7,s101).
registration(r341,c8,s101).
registration(r342,c3,s101).
registration(r343,c35,s101).
registration(r344,c47,s102).
registration(r345,c57,s102).
registration(r346,c20,s102).
registration(r347,c11,s102).
registration(r348,c41,s103).
registration(r349,c4,s103).
registration(r350,c8,s103).
registration(r351,c9,s104).
registration(r352,c37,s104).
registration(r353,c52,s104).
registration(r354,c12,s104).
registration(r355,c13,s105).
registration(r356,c2,s105).
registration(r357,c38,s105).
registration(r358,c44,s106).
registration(r359,c18,s106).
registration(r360,c62,s106).
registration(r361,c2,s107).
registration(r362,c47,s107).
registration(r363,c35,s107).
registration(r364,c29,s108).
registration(r365,c57,s108).
registration(r366,c1,s108).
registration(r367,c43,s108).
registration(r368,c45,s109).
registration(r369,c13,s109).
registration(r370,c29,s109).
registration(r371,c2,s110).
registration(r372,c16,s110).
registration(r373,c19,s110).
registration(r374,c29,s110).
registration(r375,c13,s111).
registration(r376,c1,s111).
registration(r377,c18,s111).
registration(r378,c36,s112).
registration(r379,c42,s112).
registration(r380,c32,s112).
registration(r381,c17,s113).
registration(r382,c35,s113).
registration(r383,c16,s113).
registration(r384,c11,s114).
registration(r385,c50,s114).
registration(r386,c40,s114).
registration(r387,c22,s115).
registration(r388,c31,s115).
registration(r389,c59,s115).
registration(r390,c62,s116).
registration(r391,c1,s116).
registration(r392,c8,s116).
registration(r393,c30,s116).
registration(r394,c23,s117).
registration(r395,c32,s117).
registration(r396,c56,s117).
registration(r397,c12,s117).
registration(r398,c46,s118).
registration(r399,c47,s118).
registration(r400,c25,s118).
registration(r401,c61,s118).
registration(r402,c44,s119).
registration(r403,c49,s119).
registration(r404,c61,s119).
registration(r405,c38,s120).
registration(r406,c8,s120).
registration(r407,c0,s120).
registration(r408,c60,s121).
registration(r409,c45,s121).
registration(r410,c28,s121).
registration(r411,c37,s122).
registration(r412,c4,s122).
registration(r413,c15,s122).
registration(r414,c28,s122).
registration(r415,c4,s123).
registration(r416,c31,s123).
registration(r417,c59,s123).
registration(r418,c42,s124).
registration(r419,c59,s124).
registration(r420,c39,s124).
registration(r421,c21,s125).
registration(r422,c29,s125).
registration(r423,c54,s125).
registration(r424,c28,s126).
registration(r425,c22,s126).
registration(r426,c0,s126).
registration(r427,c61,s127).
registration(r428,c7,s127).
registration(r429,c28,s127).
registration(r430,c1,s128).
registration(r431,c60,s128).
registration(r432,c7,s128).
registration(r433,c52,s129).
registration(r434,c18,s129).
registration(r435,c55,s129).
registration(r436,c4,s130).
registration(r437,c27,s130).
registration(r438,c46,s130).
registration(r439,c39,s130).
registration(r440,c3,s131).
registration(r441,c35,s131).
registration(r442,c29,s131).
registration(r443,c57,s132).
registration(r444,c38,s132).
registration(r445,c34,s132).
registration(r446,c32,s133).
registration(r447,c24,s133).
registration(r448,c1,s133).
registration(r449,c3,s134).
registration(r450,c21,s134).
registration(r451,c25,s134).
registration(r452,c13,s135).
registration(r453,c36,s135).
registration(r454,c35,s135).
registration(r455,c40,s136).
registration(r456,c24,s136).
registration(r457,c5,s136).
registration(r458,c44,s137).
registration(r459,c25,s137).
registration(r460,c2,s137).
registration(r461,c43,s137).
registration(r462,c2,s138).
registration(r463,c19,s138).
registration(r464,c9,s138).
registration(r465,c41,s139).
registration(r466,c46,s139).
registration(r467,c58,s139).
registration(r468,c63,s139).
registration(r469,c22,s140).
registration(r470,c24,s140).
registration(r471,c14,s140).
registration(r472,c37,s140).
registration(r473,c38,s141).
registration(r474,c33,s141).
registration(r475,c9,s141).
registration(r476,c15,s142).
registration(r477,c37,s142).
registration(r478,c26,s142).
registration(r479,c29,s143).
registration(r480,c5,s143).
registration(r481,c42,s143).
registration(r482,c8,s143).
registration(r483,c11,s144).
registration(r484,c29,s144).
registration(r485,c54,s144).
registration(r486,c47,s144).
registration(r487,c54,s145).
registration(r488,c9,s145).
registration(r489,c44,s145).
registration(r490,c60,s146).
registration(r491,c15,s146).
registration(r492,c5,s146).
registration(r493,c39,s147).
registration(r494,c57,s147).
registration(r495,c3,s147).
registration(r496,c41,s147).
registration(r497,c60,s148).
registration(r498,c4,s148).
registration(r499,c39,s148).
registration(r500,c18,s148).
registration(r501,c28,s149).
registration(r502,c18,s149).
registration(r503,c15,s149).
registration(r504,c5,s150).
registration(r505,c29,s150).
registration(r506,c57,s150).
registration(r507,c3,s151).
registration(r508,c35,s151).
registration(r509,c33,s151).
registration(r510,c48,s152).
registration(r511,c41,s152).
registration(r512,c13,s152).
registration(r513,c6,s153).
registration(r514,c42,s153).
registration(r515,c53,s153).
registration(r516,c49,s154).
registration(r517,c19,s154).
registration(r518,c59,s154).
registration(r519,c29,s154).
registration(r520,c40,s155).
registration(r521,c48,s155).
registration(r522,c28,s155).
registration(r523,c34,s156).
registration(r524,c45,s156).
registration(r525,c24,s156).
registration(r526,c28,s156).
registration(r527,c58,s157).
registration(r528,c34,s157).
registration(r529,c1,s157).
registration(r530,c43,s158).
registration(r531,c38,s158).
registration(r532,c57,s158).
registration(r533,c35,s159).
registration(r534,c50,s159).
registration(r535,c60,s159).
registration(r536,c16,s160).
registration(r537,c49,s160).
registration(r538,c47,s160).
registration(r539,c23,s161).
registration(r540,c31,s161).
registration(r541,c27,s161).
registration(r542,c62,s162).
registration(r543,c21,s162).
registration(r544,c23,s162).
registration(r545,c52,s163).
registration(r546,c16,s163).
registration(r547,c58,s163).
registration(r548,c14,s164).
registration(r549,c34,s164).
registration(r550,c16,s164).
registration(r551,c32,s164).
registration(r552,c42,s165).
registration(r553,c18,s165).
registration(r554,c58,s165).
registration(r555,c11,s166).
registration(r556,c18,s166).
registration(r557,c39,s166).
registration(r558,c62,s167).
registration(r559,c24,s167).
registration(r560,c44,s167).
registration(r561,c43,s168).
registration(r562,c60,s168).
registration(r563,c46,s168).
registration(r564,c18,s168).
registration(r565,c17,s169).
registration(r566,c14,s169).
registration(r567,c26,s169).
registration(r568,c59,s170).
registration(r569,c11,s170).
registration(r570,c36,s170).
registration(r571,c15,s170).
registration(r572,c45,s171).
registration(r573,c3,s171).
registration(r574,c48,s171).
registration(r575,c2,s172).
registration(r576,c11,s172).
registration(r577,c9,s172).
registration(r578,c35,s172).
registration(r579,c63,s173).
registration(r580,c46,s173).
registration(r581,c56,s173).
registration(r582,c41,s174).
registration(r583,c44,s174).
registration(r584,c63,s174).
registration(r585,c60,s174).
registration(r586,c38,s175).
registration(r587,c57,s175).
registration(r588,c41,s175).
registration(r589,c13,s175).
registration(r590,c60,s176).
registration(r591,c13,s176).
registration(r592,c29,s176).
registration(r593,c12,s177).
registration(r594,c15,s177).
registration(r595,c33,s177).
registration(r596,c41,s178).
registration(r597,c33,s178).
registration(r598,c34,s178).
registration(r599,c32,s178).
registration(r600,c5,s179).
registration(r601,c14,s179).
registration(r602,c61,s179).
registration(r603,c60,s180).
registration(r604,c58,s180).
registration(r605,c53,s180).
registration(r606,c31,s180).
registration(r607,c51,s181).
registration(r608,c2,s181).
registration(r609,c52,s181).
registration(r610,c63,s181).
registration(r611,c29,s181).
registration(r612,c41,s182).
registration(r613,c19,s182).
registration(r614,c43,s182).
registration(r615,c58,s183).
registration(r616,c16,s183).
registration(r617,c30,s183).
registration(r618,c42,s184).
registration(r619,c63,s184).
registration(r620,c5,s184).
registration(r621,c2,s184).
registration(r622,c30,s185).
registration(r623,c5,s185).
registration(r624,c62,s185).
registration(r625,c8,s186).
registration(r626,c59,s186).
registration(r627,c44,s186).
registration(r628,c33,s187).
registration(r629,c61,s187).
registration(r630,c60,s187).
registration(r631,c38,s188).
registration(r632,c55,s188).
registration(r633,c6,s188).
registration(r634,c1,s189).
registration(r635,c47,s189).
registration(r636,c10,s189).
registration(r637,c52,s190).
registration(r638,c28,s190).
registration(r639,c39,s190).
registration(r640,c42,s191).
registration(r641,c3,s191).
registration(r642,c48,s191).
registration(r643,c44,s192).
registration(r644,c50,s192).
registration(r645,c24,s192).
registration(r646,c24,s193).
registration(r647,c59,s193).
registration(r648,c42,s193).
registration(r649,c6,s194).
registration(r650,c60,s194).
registration(r651,c23,s194).
registration(r652,c3,s195).
registration(r653,c15,s195).
registration(r654,c11,s195).
registration(r655,c45,s196).
registration(r656,c1,s196).
registration(r657,c10,s196).
registration(r658,c51,s197).
registration(r659,c63,s197).
registration(r660,c40,s197).
registration(r661,c7,s198).
registration(r662,c16,s198).
registration(r663,c28,s198).
registration(r664,c8,s199).
registration(r665,c57,s199).
registration(r666,c37,s199).
registration(r667,c4,s200).
registration(r668,c58,s200).
registration(r669,c12,s200).
registration(r670,c39,s200).
registration(r671,c31,s201).
registration(r672,c21,s201).
registration(r673,c50,s201).
registration(r674,c30,s201).
registration(r675,c46,s202).
registration(r676,c12,s202).
registration(r677,c52,s202).
registration(r678,c51,s202).
registration(r679,c63,s203).
registration(r680,c35,s203).
registration(r681,c28,s203).
registration(r682,c51,s203).
registration(r683,c40,s204).
registration(r684,c29,s204).
registration(r685,c1,s204).
registration(r686,c53,s204).
registration(r687,c49,s205).
registration(r688,c7,s205).
registration(r689,c46,s205).
registration(r690,c54,s206).
registration(r691,c36,s206).
registration(r692,c2,s206).
registration(r693,c23,s206).
registration(r694,c14,s207).
registration(r695,c54,s207).
registration(r696,c29,s207).
registration(r697,c50,s208).
registration(r698,c27,s208).
registration(r699,c30,s208).
registration(r700,c59,s209).
registration(r701,c56,s209).
registration(r702,c50,s209).
registration(r703,c17,s210).
registration(r704,c53,s210).
registration(r705,c4,s210).
registration(r706,c53,s211).
registration(r707,c28,s211).
registration(r708,c25,s211).
registration(r709,c15,s211).
registration(r710,c25,s212).
registration(r711,c3,s212).
registration(r712,c30,s212).
registration(r713,c43,s213).
registration(r714,c37,s213).
registration(r715,c16,s213).
registration(r716,c15,s213).
registration(r717,c34,s213).
registration(r718,c46,s214).
registration(r719,c53,s214).
registration(r720,c33,s214).
registration(r721,c22,s215).
registration(r722,c30,s215).
registration(r723,c16,s215).
registration(r724,c20,s216).
registration(r725,c60,s216).
registration(r726,c62,s216).
registration(r727,c25,s216).
registration(r728,c62,s217).
registration(r729,c25,s217).
registration(r730,c40,s217).
registration(r731,c6,s218).
registration(r732,c9,s218).
registration(r733,c18,s218).
registration(r734,c33,s219).
registration(r735,c56,s219).
registration(r736,c63,s219).
registration(r737,c45,s220).
registration(r738,c25,s220).
registration(r739,c62,s220).
registration(r740,c19,s220).
registration(r741,c54,s221).
registration(r742,c24,s221).
registration(r743,c60,s221).
registration(r744,c44,s221).
registration(r745,c8,s222).
registration(r746,c56,s222).
registration(r747,c18,s222).
registration(r748,c43,s223).
registration(r749,c10,s223).
registration(r750,c15,s223).
registration(r751,c17,s223).
registration(r752,c26,s224).
registration(r753,c2,s224).
registration(r754,c6,s224).
registration(r755,c5,s225).
registration(r756,c32,s225).
registration(r757,c23,s225).
registration(r758,c20,s226).
registration(r759,c11,s226).
registration(r760,c2,s226).
registration(r761,c29,s226).
registration(r762,c37,s227).
registration(r763,c12,s227).
registration(r764,c10,s227).
registration(r765,c3,s228).
registration(r766,c47,s228).
registration(r767,c54,s228).
registration(r768,c0,s229).
registration(r769,c10,s229).
registration(r770,c37,s229).
registration(r771,c62,s230).
registration(r772,c19,s230).
registration(r773,c38,s230).
registration(r774,c44,s231).
registration(r775,c25,s231).
registration(r776,c37,s231).
registration(r777,c58,s232).
registration(r778,c42,s232).
registration(r779,c22,s232).
registration(r780,c51,s233).
registration(r781,c8,s233).
registration(r782,c58,s233).
registration(r783,c14,s234).
registration(r784,c0,s234).
registration(r785,c23,s234).
registration(r786,c59,s234).
registration(r787,c5,s235).
registration(r788,c4,s235).
registration(r789,c23,s235).
registration(r790,c21,s236).
registration(r791,c42,s236).
registration(r792,c12,s236).
registration(r793,c57,s237).
registration(r794,c40,s237).
registration(r795,c30,s237).
registration(r796,c26,s238).
registration(r797,c4,s238).
registration(r798,c21,s238).
registration(r799,c8,s239).
registration(r800,c7,s239).
registration(r801,c45,s239).
registration(r802,c47,s239).
registration(r803,c7,s240).
registration(r804,c4,s240).
registration(r805,c0,s240).
registration(r806,c54,s240).
registration(r807,c9,s240).
registration(r808,c11,s241).
registration(r809,c29,s241).
registration(r810,c45,s241).
registration(r811,c58,s241).
registration(r812,c48,s242).
registration(r813,c0,s242).
registration(r814,c51,s242).
registration(r815,c12,s243).
registration(r816,c24,s243).
registration(r817,c59,s243).
registration(r818,c58,s244).
registration(r819,c4,s244).
registration(r820,c52,s244).
registration(r821,c47,s244).
registration(r822,c49,s245).
registration(r823,c43,s245).
registration(r824,c3,s245).
registration(r825,c5,s246).
registration(r826,c44,s246).
registration(r827,c51,s246).
registration(r828,c15,s247).
registration(r829,c58,s247).
registration(r830,c25,s247).
registration(r831,c9,s248).
registration(r832,c23,s248).
registration(r833,c40,s248).
registration(r834,c52,s249).
registration(r835,c59,s249).
registration(r836,c50,s249).
registration(r837,c38,s249).
registration(r838,c10,s250).
registration(r839,c30,s250).
registration(r840,c49,s250).
registration(r841,c34,s251).
registration(r842,c41,s251).
registration(r843,c27,s251).
registration(r844,c34,s252).
registration(r845,c4,s252).
registration(r846,c8,s252).
registration(r847,c23,s253).
registration(r848,c42,s253).
registration(r849,c20,s253).
registration(r850,c30,s254).
registration(r851,c5,s254).
registration(r852,c7,s254).
registration(r853,c61,s254).
registration(r854,c60,s255).
registration(r855,c48,s255).
registration(r856,c0,s255).

