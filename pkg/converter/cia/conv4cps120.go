package cia

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cps1202text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPS120.length")
	batchSize := viper.GetInt("server.batchSize")

	// Baca file input
	content, err := utils.ReadAllFile(inputFilename)
	if err != nil {
		utils.Error("Failed to read file: %v", err)
		return false
	}

	totalRecords := utf8.RuneCountInString(content) / recordLength
	var (
		data    string
		counter int
	)

	for line := 0; line <= totalRecords; line++ {
		startIdx := recordLength * line
		endIdx := recordLength * (line + 1)
		record := content[startIdx:endIdx]

		decoded := decodeCPS120(record)
		if len(decoded) > 0 {
			data += decoded
			counter++
		}

		if counter == batchSize {
			if ok, err := utils.WriteFile(outputFilename, data); !ok {
				utils.Error("Write error: %v", err)
				return false
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

func decodeCPS120(strBlock string) string {
	return utils.SafeDecode("decodeCPS120", func() string {
		var lineResult string
		lineResult += utils.Hex2string_comp3(strBlock[0:2])[0:3] + "|"   //CMT-ORGN-NMBR	PIC S999        COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[2:4])[0:3] + "|"   //CMT-TYPE    PIC S999        COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[4:13])[1:17] + "|" //CMT-CARD-NMBR	PIC S9(16)      COMP-3
		// lineResult += utils.Hex2string_comp3(strBlock[13:17])[1:17] + "|"   //CMT-EFFECTIVE-DTE
		lineResult += utils.Hex2string(strBlock[17:19]) + "|"               //CMT-CODE        PIC 99
		lineResult += utils.Hex2string_comp3(strBlock[19:24])[0:9] + "|"    //CMT-AMNT        PIC S9(7)V99    COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[24:28])[0:7] + "|"    //CMT-POSTING-DTE PIC S9(7)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[28:31])[1:5] + "|"    //CMT-SOURCE-CODE PIC S9(4)       COMP-3
		lineResult += utils.Hex2string(strBlock[31:37]) + "|"               //CMT-AUTH-CODE   PIC X(6)
		lineResult += utils.Hex2string_comp3(strBlock[37:39])[0:3] + "|"    //CMT-RN-DTE  PIC S999        COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[39:42])[0:5] + "|"    //CMT-RN-BATCH-NMBR	PIC S9(5)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[42:44])[0:3] + "|"    //CMT-RN-SEQ-NMBR	PIC S999        COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[44:46])[0:3] + "|"    //CMT-MERCH-ORGN	PIC S999        COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[46:51])[0:9] + "|"    //CMT-MERCH-ACCT	PIC S9(9)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[51:54])[0:5] + "|"    //CMT-MERCH-CATEGORY PIC S9(5)       COMP-3
		lineResult += utils.Hex2string(strBlock[54:79]) + "|"               //CMT-DBA-NAME		PIC X(25)
		lineResult += utils.Hex2string(strBlock[79:92]) + "|"               //CMT-DBA-CITY		PIC X(13)
		lineResult += utils.Hex2string(strBlock[92:94]) + "|"               //CMT-DBA-COUNTRY 	PIC X(2)
		lineResult += utils.Hex2string(strBlock[94:96]) + "|"               //CMT-MERCH-STATE	PIC X(2)
		lineResult += utils.Hex2string(strBlock[96:97]) + "|"               //CMT-AUTH-FLAG                PIC X
		lineResult += utils.Hex2string(strBlock[97:98]) + "|"               //CMT-SPLIT-CHAR  PIC X
		lineResult += utils.Hex2string(strBlock[98:99]) + "|"               //CMT-WHSED-FLAG  PIC 9
		lineResult += utils.Hex2string(strBlock[99:101]) + "|"              //CMT-POSTING-FLAG	PIC 99
		lineResult += utils.Hex2string(strBlock[101:103]) + "|"             //CMT-REV-TRAN-ID   PIC 99
		lineResult += utils.Hex2string_comp3(strBlock[103:111])[0:15] + "|" //CMT-TRAN-ID       PIC S9(15)      COMP-3
		lineResult += utils.Hex2string(strBlock[111:134]) + "|"             //CMT-ACQ-REF-NBR   PIC X(23)
		lineResult += utils.Hex2string(strBlock[134:135]) + "|"             //CMT-PYMT-TYPE-IND PIC 9
		lineResult += utils.Hex2string(strBlock[135:136]) + "|"             //CMT-RPS           PIC X(01)
		lineResult += utils.Hex2string(strBlock[136:138]) + "|"             //CMT-CHGBK-RT      PIC X(02)
		lineResult += utils.Hex2string_comp3(strBlock[138:140])[0:3] + "|"  //CMT-AHF-ORIG-CURR-CODE	PIC S9(3)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[140:147])[1:13] + "|" //CMT-AHF-ORIG-CURR-AMT		PIC S9(10)V99   COMP-3
		lineResult += utils.Hex2string(strBlock[147:149]) + "|"             //CMT-AHF-ORIG-CURR-DECIMAL	PIC 99
		lineResult += utils.Hex2string(strBlock[149:150]) + "|"             //CMT-AHF-ONUS-CURR-CONV	PIC 9
		lineResult += utils.Hex2string_comp3(strBlock[150:153])[1:5] + "|"  //visa -- CMTV-ORIG-CPD            PIC S9(4) COMP-3
		//mc -- CMT-MC-SEC-PROTOCOL	PIC X(01)
		//mc -- CMT-MC-CH-AUTHENTIC PIC X(01)
		//mc -- CMT-MC-UCAF-IND		PIC X(01)
		lineResult += utils.Hex2string(strBlock[153:154]) + "|"             //CMT-SETL-IND    PIC X
		lineResult += utils.Hex2string(strBlock[154:155]) + "|"             //CMT-TRAN-TYPE   PIC 9
		lineResult += utils.Hex2string(strBlock[155:156]) + "|"             //CMT-INTL-ACQ	PIC X
		lineResult += utils.Hex2string(strBlock[156:157]) + "|"             //CMT-INTL-ISS	PIC X
		lineResult += utils.Hex2string(strBlock[157:158]) + "|"             //CMT-INTL-MER	PIC X
		lineResult += utils.Hex2string(strBlock[158:159]) + "|"             //FILLER          PIC X
		lineResult += utils.Hex2string(strBlock[159:160]) + "|"             //CMT-CARD-CARD-TYPE 	PIC X
		lineResult += utils.Hex2string_comp3(strBlock[160:169])[1:18] + "|" //CMT-ALIAS-CARDHLD-NBR	PIC S9(17)      COMP-3
		//CMT-AUTH-DATA     REDEFINES CMT-ALIAS-CARDHLD-NBR
		lineResult += utils.Hex2string_comp3(strBlock[160:167])[1:13] + "|" //CMT-AUTH-AMT  PIC S9(12)    COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[167:169])[0:3] + "|"  //CMT-AUTH-CCY  PIC S9(03)    COMP-3
		lineResult += utils.Hex2string(strBlock[169:173]) + "|"             //CMT-ORIG-SOURCE-CODE	PIC 9(4)
		lineResult += utils.Hex2string(strBlock[173:175]) + "|"             //CMT-POS-MODE    PIC X(2)
		//FILLER          PIC X
		lineResult += utils.Hex2string(strBlock[176:177]) + "|"            //CMT-INSTALLMENT-IND          PIC X
		lineResult += utils.Hex2string_comp3(strBlock[177:182])[0:9] + "|" //CMT-XBORDER-FEE        PIC S9(07)V99 COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[182:187])[0:9] + "|" //CMT-MARKUP-AMT         PIC S9(07)V99 COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[187:192])[0:9] + "|" //CMT-CCA-AMT            PIC S9(07)V99 COMP-3
		lineResult += utils.Hex2string(strBlock[192:193]) + "|"            //CMT-DCC-FLAG           PIC X(01)
		lineResult += utils.Hex2string(strBlock[193:194]) + "|"            //CMT-PYMT-AUTH          PIC X(01)

		lineResult += utils.Hex2string_comp3(strBlock[269:276])[0:13] + "|XX" //CMTI-SETL-CURR-AMT	PIC S9(11)V99   COMP-3
		lineResult += utils.Hex2string(strBlock[269:270]) + "|YY"             //CMTV-REIMB-ATT	PIC X
		lineResult += utils.Hex2string_comp3(strBlock[269:271])[0:3] + "|ZZ"  //CMT-ORIG-CURR-CODE	PIC S9(3)       COMP-3

		lineResult += "\r\n"
		return lineResult
	})
}
