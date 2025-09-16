package cia

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cpsmtf2text(inputFilename string) bool {
	start := time.Now()
	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPSMTF.length")
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

		decoded := decodeCPSMTF(record)
		// if line > 10 {
		// 	break
		// }
		if len(decoded) > 0 {
			data += decoded
			counter++
		}

		if counter == batchSize {
			if !utils.WriteAndCheck(outputFilename, data) {
				return false
			}
			counter = 0
			data = ""
		}
	}

	// Tulis sisa data jika ada
	if len(data) > 0 {
		if !utils.WriteAndCheck(outputFilename, data) {
			return false
		}
	}
	utils.Info("✅ %s Finished in %s", inputFilename, time.Since(start))
	return true
}

func decodeCPSMTF(strBlock string) string {
	return utils.SafeDecode("decodeCPSMTF", func() string {
		var lineResult string

		cardType := utils.Hex2string_comp3(strBlock[2:4])[0:3]
		cardNumber := utils.Hex2string_comp3(strBlock[4:13])[1:17]

		if (cardNumber != "0000000000000000") && (cardType != "998") {
			lineResult += utils.Hex2string_comp3(strBlock[0:2])[0:3] + "|"      //CMT-ORGN-NMBR 			PIC S999        COMP-3.
			lineResult += cardType + "|"                                        //CMT-TYPE    				PIC S999        COMP-3.
			lineResult += cardNumber + "|"                                      //CMT-CARD-NMBR  			PIC S9(16)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[13:17])[0:7] + "|"    //CMT-EFFECTIVE-DTE 		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string(strBlock[17:19]) + "|"               //CMT-CODE                  PIC 99.
			lineResult += utils.Hex2string_comp3(strBlock[19:24])[0:9] + "|"    //CMT-AMNT        			PIC S9(7)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[24:28])[0:7] + "|"    //CMT-POSTING-DTE 			PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[28:31])[0:5] + "|"    //CMT-SOURCE-CODE 			PIC S9(4)       COMP-3.
			lineResult += utils.Hex2string(strBlock[31:37]) + "|"               //CMT-AUTH-CODE   			PIC X(6).
			lineResult += utils.Hex2string_comp3(strBlock[37:39])[0:3] + "|"    //	CMT-RN-DTE  			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[39:42])[0:5] + "|"    //	CMT-RN-BATCH-NMBR		PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[42:44])[0:3] + "|"    //	CMT-RN-SEQ-NMBR 		PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[44:46])[0:3] + "|"    //	CMT-MERCH-ORGN			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[46:51])[0:9] + "|"    //	CMT-MERCH-ACCT			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[51:54])[0:5] + "|"    //CMT-MERCH-CATEGORY		PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string(strBlock[54:79]) + "|"               //	CMT-DBA-NAME			PIC X(25).
			lineResult += utils.Hex2string(strBlock[54:65]) + "|"               //		CMT-DBA-AIRLINE		PIC X(11).
			lineResult += utils.Hex2string(strBlock[65:66]) + "|"               //		CMT-AIR-TCKT-DELIM	PIC X.
			lineResult += utils.Hex2string(strBlock[66:79]) + "|"               //		CMT-AIR-TCKT-NBR	PIC X(13).
			lineResult += utils.Hex2string(strBlock[79:92]) + "|"               //	CMT-DBA-CITY			PIC X(13).
			lineResult += utils.Hex2string(strBlock[92:94]) + "|"               // 	CMT-DBA-COUNTRY    		PIC XX.
			lineResult += utils.Hex2string(strBlock[94:96]) + "|"               //CMT-MERCH-STATE			PIC X(2).
			lineResult += utils.Hex2string(strBlock[96:97]) + "|"               //CMT-AUTH-FLAG            	PIC X.
			lineResult += utils.Hex2string(strBlock[97:98]) + "|"               //CMT-SPLIT-CHAR  			PIC X.
			lineResult += utils.Hex2string(strBlock[98:99]) + "|"               //CMT-WHSED-FLAG  PIC 9.
			lineResult += utils.Hex2string(strBlock[99:101]) + "|"              //CMT-POSTING-FLAG			PIC 99.
			lineResult += utils.Hex2string(strBlock[101:103]) + "|"             //CMT-REV-TRAN-ID   		PIC 99.
			lineResult += utils.Hex2string_comp3(strBlock[103:111])[0:15] + "|" //CMT-TRAN-ID       		PIC S9(15)      COMP-3.
			lineResult += utils.Hex2string(strBlock[111:134]) + "|"             //CMT-ACQ-REF-NBR   		PIC X(23).
			lineResult += utils.Hex2string(strBlock[134:135]) + "|"             //CMT-PYMT-TYPE-IND 		PIC 9.
			lineResult += utils.Hex2string(strBlock[135:136]) + "|"             //CMT-RPS           		PIC X(01).
			lineResult += utils.Hex2string(strBlock[136:138]) + "|"             //CMT-CHGBK-RT      		PIC X(02).
			lineResult += utils.Hex2string_comp3(strBlock[138:140])[0:3] + "|"  //		CMT-AHF-ORIG-CURR-CODE		PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[140:147])[0:13] + "|" //		CMT-AHF-ORIG-CURR-AMT  		PIC S9(10)V99   COMP-3.
			lineResult += utils.Hex2string(strBlock[147:150]) + "|"             //		CMT-AHF-ORIG-CURR-DECIMAL	PIC 99.
			lineResult += utils.Hex2string(strBlock[150:151]) + "|"             //		CMT-AHF-ONUS-CURR-CONV      PIC 9.
			lineResult += utils.Hex2string(strBlock[151:152]) + "|"             //		CMT-MC-SEC-PROTOCOL			PIC X(01).
			lineResult += utils.Hex2string(strBlock[152:153]) + "|"             //		CMT-MC-CH-AUTHENTIC			PIC X(01).
			lineResult += utils.Hex2string(strBlock[153:154]) + "|"             //CMT-MC-UCAF-IND					PIC X(01).
			lineResult += utils.Hex2string(strBlock[154:155]) + "|"             //CMT-SETL-IND    					PIC X.
			lineResult += utils.Hex2string(strBlock[151:156]) + "|"             //CMT-TRAN-TYPE   					PIC 9.
			lineResult += utils.Hex2string(strBlock[156:157]) + "|"             //	CMT-INTL-ACQ					PIC X.
			lineResult += utils.Hex2string(strBlock[157:158]) + "|"             //	CMT-INTL-ISS					PIC X.
			lineResult += utils.Hex2string(strBlock[158:159]) + "|"             //	CMT-INTL-MER					PIC X.
			lineResult += utils.Hex2string(strBlock[159:160]) + "|"             //FILLER          PIC X.
			lineResult += utils.Hex2string(strBlock[160:161]) + "|"             //CMT-CARD-CARD-TYPE				PIC X.
			lineResult += utils.Hex2string_comp3(strBlock[160:167])[0:11] + "|" //	CMT-AUTH-AMT  					PIC S9(12)    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[167:169])[0:3] + "|"  //	CMT-AUTH-CCY  					PIC S9(03)    COMP-3.
			lineResult += utils.Hex2string(strBlock[169:172]) + "|"             //CMT-ORIG-SOURCE-CODE       		PIC 9(4).
			lineResult += utils.Hex2string(strBlock[172:176]) + "|"             //CMT-POS-MODE    					PIC X(2).
			lineResult += utils.Hex2string(strBlock[176:177]) + "|"             //CMT-INSTALLMENT-IND         		PIC X.
			lineResult += utils.Hex2string_comp3(strBlock[177:182])[0:9] + "|"  //CMT-XBORDER-FEE        			PIC S9(07)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[182:187])[0:9] + "|"  //CMT-MARKUP-AMT         			PIC S9(07)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[187:192])[0:9] + "|"  //CMT-CCA-AMT           		 	PIC S9(07)V99 COMP-3.
			lineResult += utils.Hex2string(strBlock[192:193]) + "|"             //CMT-DCC-FLAG           			PIC X(01).
			lineResult += utils.Hex2string(strBlock[193:194]) + "|"             //CMT-PYMT-AUTH          			PIC X(01).
			lineResult += utils.Hex2string(strBlock[194:196]) + "|"             //CMT-MULT-SEQ-NBR       			PIC 9(02).
			lineResult += utils.Hex2string(strBlock[196:198]) + "|"             //CMT-MULT-SEQ-TOTAL-CNT 			PIC 9(02).
			lineResult += utils.Hex2string(strBlock[199:199]) + "|"             //CMT-MULT-IND           			PIC X(01).
			lineResult += "\r\n"
		}
		return lineResult
	})
}
