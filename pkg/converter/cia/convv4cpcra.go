package cia

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cpcra2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPCRA.length")
	batchSize := viper.GetInt("server.batchSize")

	// Baca file input
	content, err := utils.ReadAllFile(inputFilename)
	if err != nil {
		utils.Error("Failed to read file", err)
		return false
	}

	totalRecords := utf8.RuneCountInString(content) / recordLength
	var (
		data    string
		counter int
	)

	for i := 0; i <= totalRecords; i++ {
		startIdx := recordLength * i
		endIdx := recordLength * (i + 1)
		record := content[startIdx:endIdx]

		decoded := decodeCPCRA(record)
		if len(decoded) > 0 {
			data += decoded
			counter++
		}

		if counter == batchSize {
			if ok, err := utils.WriteFile(outputFilename, data); !ok {
				utils.Error("Write error: %v", err)
			}
			counter = 0
			data = ""
		}
	}

	// Tulis sisa data jika ada
	if len(data) > 0 {
		if ok, err := utils.WriteFile(outputFilename, data); !ok {
			utils.Error("Write error: %v", err)
			return false
		}
	}

	utils.Info("%s Finished in %s", inputFilename, time.Since(start))
	return true
}

func decodeCPCRA(strBlock string) string {
	return utils.SafeDecode("decodeCPCRA", func() string {
		lineResult := ""
		if utils.Hex2string_comp3(strBlock[0:2])[0:3] != "000" {
			lineResult += utils.Hex2string_comp3(strBlock[0:2])[0:3] + "|"       //CRA_ORG_NBR
			lineResult += utils.Hex2string_comp3(strBlock[2:11])[1:17] + "|"     //CRA-CUST-NBR          PIC S9(16)   	COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[11:16])[0:9] + "|"     //CRA-INCOME            PIC S9(09)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[16:31]) + "|"                //CRA-IC-PP             PIC  X(15)
			lineResult += utils.Hex2string(strBlock[31:46]) + "|"                //CRA-LAST-IC-PP        PIC  X(15)
			lineResult += utils.Hex2string(strBlock[46:48]) + "|"                //CRA-USR-DEF-1         PIC  X(02)
			lineResult += utils.Hex2string(strBlock[48:50]) + "|"                //CRA-USR-DEF-2         PIC  X(02)
			lineResult += utils.Hex2string(strBlock[50:80]) + "|"                //CRA-MOTHER            PIC  X(30)
			lineResult += utils.Hex2string_comp3(strBlock[80:89])[1:17] + "|"    //CRA-SUPCRD-CRD-NBR1a  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[89:98])[1:17] + "|"    //CRA-SUPCRD-CRD-NBR2a  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[98:102])[:7] + "|"     //CRA-SUPCRD-BIRTHDTEa  PIC S9(07)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[102:103]) + "|"              //CRA-SUPCRD-ADDR-TYPEa PIC  X(01)
			lineResult += utils.Hex2string(strBlock[103:118]) + "|"              //CRA-SUPCRD-IC-PPa     PIC  X(15)
			lineResult += utils.Hex2string(strBlock[118:133]) + "|"              //CRA-SUPCRD-RELATIONa  PIC  X(15)
			lineResult += utils.Hex2string(strBlock[133:163]) + "|"              //CRA-SUPCRD-ADDR1a     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[163:193]) + "|"              //CRA-SUPCRD-ADDR2a     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[193:223]) + "|"              //CRA-SUPCRD-ADDR3a     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[223:253]) + "|"              //CRA-SUPCRD-CITYa      PIC  X(30)
			lineResult += utils.Hex2string(strBlock[253:262]) + "|"              //CRA-SUPCRD-POSTALa    PIC  X(09)
			lineResult += utils.Hex2string(strBlock[262:292]) + "|"              //CRA-SUPCRD-EMPLOYERa  PIC  X(30)
			lineResult += utils.Hex2string(strBlock[292:310]) + "|"              //CRA-SUPCRD-HOME-TELa  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[310:328]) + "|"              //CRA-SUPCRD-WORK-TELa  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[328:346]) + "|"              //CRA-SUPCRD-HPHONEa    PIC  X(18)
			lineResult += utils.Hex2string_comp3(strBlock[346:351])[0:9] + "|"   //CRA-SUPCRD-INCOMEa    PIC S9(09)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[351:381]) + "|"              //CRA-SUPCRD-MOTHERa    PIC  X(30)
			lineResult += utils.Hex2string(strBlock[381:382]) + "|"              //CRA-SUPCRD-STa        PIC  X(01)
			lineResult += utils.Hex2string_comp3(strBlock[382:391])[1:17] + "|"  //CRA-SUPCRD-CRD-NBR1b  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[391:400])[1:17] + "|"  //CRA-SUPCRD-CRD-NBR2b  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[400:404])[0:7] + "|"   //CRA-SUPCRD-BIRTHDTEb  PIC S9(07)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[404:405]) + "|"              //CRA-SUPCRD-ADDR-TYPEb PIC  X(01)
			lineResult += utils.Hex2string(strBlock[405:420]) + "|"              //CRA-SUPCRD-IC-PPb     PIC  X(15)
			lineResult += utils.Hex2string(strBlock[420:435]) + "|"              //CRA-SUPCRD-RELATIONb  PIC  X(15)
			lineResult += utils.Hex2string(strBlock[435:465]) + "|"              //CRA-SUPCRD-ADDR1b     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[465:495]) + "|"              //CRA-SUPCRD-ADDR2b     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[495:525]) + "|"              //CRA-SUPCRD-ADDR3b     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[525:555]) + "|"              //CRA-SUPCRD-CITYb      PIC  X(30)
			lineResult += utils.Hex2string(strBlock[555:564]) + "|"              //CRA-SUPCRD-POSTALb    PIC  X(09)
			lineResult += utils.Hex2string(strBlock[564:594]) + "|"              //CRA-SUPCRD-EMPLOYERb  PIC  X(30)
			lineResult += utils.Hex2string(strBlock[594:612]) + "|"              //CRA-SUPCRD-HOME-TELb  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[612:630]) + "|"              //CRA-SUPCRD-WORK-TELb  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[630:648]) + "|"              //CRA-SUPCRD-HPHONEb    PIC  X(18)
			lineResult += utils.Hex2string_comp3(strBlock[648:653])[0:9] + "|"   //CRA-SUPCRD-INCOMEb    PIC S9(09)	COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[653:683]) + "|"              //CRA-SUPCRD-MOTHERb    PIC  X(30)
			lineResult += utils.Hex2string(strBlock[683:684]) + "|"              //CRA-SUPCRD-STb        PIC  X(01)
			lineResult += utils.Hex2string_comp3(strBlock[684:693])[0:9] + "|"   //CRA-SUPCRD-CRD-NBR1c  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[693:702])[1:16] + "|"  //CRA-SUPCRD-CRD-NBR2c  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[702:706])[0:7] + "|"   //CRA-SUPCRD-BIRTHDTEc  PIC S9(07)	COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[706:707]) + "|"              //CRA-SUPCRD-ADDR-TYPEc PIC  X(01)
			lineResult += utils.Hex2string(strBlock[707:722]) + "|"              //CRA-SUPCRD-IC-PPc     PIC  X(15)
			lineResult += utils.Hex2string(strBlock[722:737]) + "|"              //CRA-SUPCRD-RELATIONc  PIC  X(15)
			lineResult += utils.Hex2string(strBlock[737:767]) + "|"              //CRA-SUPCRD-ADDR1c     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[767:797]) + "|"              //CRA-SUPCRD-ADDR2c     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[797:827]) + "|"              //CRA-SUPCRD-ADDR3c     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[827:857]) + "|"              //CRA-SUPCRD-CITYc      PIC  X(30)
			lineResult += utils.Hex2string(strBlock[857:866]) + "|"              //CRA-SUPCRD-POSTALc    PIC  X(09)
			lineResult += utils.Hex2string(strBlock[866:896]) + "|"              //CRA-SUPCRD-EMPLOYERc  PIC  X(30)
			lineResult += utils.Hex2string(strBlock[896:914]) + "|"              //CRA-SUPCRD-HOME-TELc  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[914:932]) + "|"              //CRA-SUPCRD-WORK-TELc  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[932:950]) + "|"              //CRA-SUPCRD-HPHONEc    PIC  X(18)
			lineResult += utils.Hex2string_comp3(strBlock[950:955])[0:9] + "|"   //CRA-SUPCRD-INCOMEc    PIC S9(09)	COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[955:985]) + "|"              //CRA-SUPCRD-MOTHERc    PIC  X(30)
			lineResult += utils.Hex2string(strBlock[985:986]) + "|"              //CRA-SUPCRD-STc        PIC  X(01)
			lineResult += utils.Hex2string_comp3(strBlock[986:995])[1:17] + "|"  //CRA-SUPCRD-CRD-NBR1d  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[995:1004])[1:17] + "|" //CRA-SUPCRD-CRD-NBR2d  PIC S9(16)   	COMP-3		REDEFINES
			lineResult += utils.Hex2string_comp3(strBlock[1004:1008])[0:7] + "|" //CRA-SUPCRD-BIRTHDTEd  PIC S9(07)		COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[1008:1009]) + "|"            //CRA-SUPCRD-ADDR-TYPEd PIC  X(01)
			lineResult += utils.Hex2string(strBlock[1009:1024]) + "|"            //CRA-SUPCRD-IC-PPd     PIC  X(15)
			lineResult += utils.Hex2string(strBlock[1024:1039]) + "|"            //CRA-SUPCRD-RELATIONd  PIC  X(15)
			lineResult += utils.Hex2string(strBlock[1039:1069]) + "|"            //CRA-SUPCRD-ADDR1d     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[1069:1099]) + "|"            //CRA-SUPCRD-ADDR2d     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[1099:1129]) + "|"            //CRA-SUPCRD-ADDR3d     PIC  X(30)
			lineResult += utils.Hex2string(strBlock[1129:1159]) + "|"            //CRA-SUPCRD-CITYd      PIC  X(30)
			lineResult += utils.Hex2string(strBlock[1159:1168]) + "|"            //CRA-SUPCRD-POSTALd    PIC  X(09)
			lineResult += utils.Hex2string(strBlock[1168:1198]) + "|"            //CRA-SUPCRD-EMPLOYERd  PIC  X(30)
			lineResult += utils.Hex2string(strBlock[1198:1216]) + "|"            //CRA-SUPCRD-HOME-TELd  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[1216:1234]) + "|"            //CRA-SUPCRD-WORK-TELd  PIC  X(18)
			lineResult += utils.Hex2string(strBlock[1234:1252]) + "|"            //CRA-SUPCRD-HPHONEd    PIC  X(18)
			lineResult += utils.Hex2string_comp3(strBlock[1252:1257])[0:9] + "|" //CRA-SUPCRD-INCOMEd    PIC S9(09)		COMP-3		REDEFINES
			lineResult += utils.Hex2string(strBlock[1257:1287]) + "|"            //CRA-SUPCRD-MOTHERd    PIC  X(30)
			lineResult += utils.Hex2string(strBlock[1287:1288]) + "|"            //CRA-SUPCRD-STd        PIC  X(01)
			lineResult += utils.Hex2string(strBlock[1288:1500]) + "\r\n"         //FILLER                PIC  X(212)
		}

		return lineResult
	})

}
