package cia

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

// Cpahf2text reads a CPAHF fixed-length record file,
// decodes each record using the decodeCPAHF function,
// and writes the output in batches defined by the 'server.batchSize' configuration.
//
// This function:
// - Reads input file from 'file.CPAHF.fileCPAHF'
// - Processes records of length 'file.CPAHF.length'
// - Appends decoded output to the same file (or another defined output)
// - Logs the total time taken to process the file
func Cpahf2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPAHF.length")
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

		decoded := decodeCPAHF(record)
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

	utils.Info("%s Finished in %s", inputFilename, time.Since(start))
	return true
}

func decodeCPAHF(strBlock string) string {
	return utils.SafeDecode("decodeCPAHF", func() string {
		var lineResult string
		if val := utils.Hex2string_comp3(strBlock[0:2])[0:3]; val != "000" && val != "999" {
			lineResult += utils.Hex2string_comp3(strBlock[0:2])[0:3] + "|"      //AH-ORG     				PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[2:4])[0:3] + "|"      //AH-TYPE     			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[4:13])[1:17] + "|"    //AH-ACCT    				PIC S9(16)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[13:16])[0:5] + "|"    //AH-SEQ      			PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[16:20])[0:7] + "|"    //AH-EFF-DTE      		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string(strBlock[20:22]) + "|"               //AH-TRANS-CODE   		PIC 99.
			lineResult += utils.Hex2string_comp3(strBlock[22:27])[0:9] + "|"    //AH-AMNT         		PIC S9(7)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[27:31])[0:7] + "|"    //AH-POSTING-DTE  		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[31:34])[1:5] + "|"    //AH-SOURCE       		PIC S9(4)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[34:40])[0:11] + "|"   //AH-REFERENCE    		PIC S9(11)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[40:43])[0:5] + "|"    //AH-MERCH-CATEGORY   	PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string(strBlock[43:83]) + "|"               //AH-DESC         		PIC X(40).
			lineResult += utils.Hex2string(strBlock[83:85]) + "|"               //AH-MERCH-STATE  		PIC XX.
			lineResult += utils.Hex2string(strBlock[85:86]) + "|"               //AH-INTL-FEE-IND 		PIC X.
			lineResult += utils.Hex2string(strBlock[86:109]) + "|"              //AH-ORIG-REF-NBR 		PIC X(23).
			lineResult += utils.Hex2string(strBlock[109:110]) + "|"             //AH-REIMB-ATT    		PIC X.
			lineResult += utils.Hex2string_comp3(strBlock[110:112])[0:3] + "|"  //AH-ORIG-CURR-CODE  		PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[112:119])[1:13] + "|" //AH-ORIG-CURR-AMT		PIC S9(10)V99   COMP-3.
			lineResult += utils.Hex2string(strBlock[119:121]) + "|"             //AH-ORIG-CURR-DECIMAL  	PIC 99.
			lineResult += utils.Hex2string(strBlock[121:122]) + "|"             //AH-ONUS-CURR-CONV       PIC 9
			lineResult += utils.Hex2string(strBlock[122:137]) + "|"             //AH-NM-MERCH-ID         	PIC X(15).
			lineResult += utils.Hex2string(strBlock[137:145]) + "|"             //AH-NM-TERM-ID          	PIC X(08).
			lineResult += utils.Hex2string_comp3(strBlock[145:153])[0:15] + "|" //AH-PS2000-TRAN-ID      	PIC S9(15)   	COMP-3.
			lineResult += utils.Hex2string(strBlock[153:154]) + "|"             //AH-PS2000-RPS          	PIC X.
			lineResult += utils.Hex2string(strBlock[154:155]) + "|"             //AH-MAIL-IND            	PIC X.
			lineResult += utils.Hex2string(strBlock[155:159]) + "|"             //AH-ORIG-CPD            	PIC X(4).
			lineResult += utils.Hex2string(strBlock[159:161]) + "|"             //AH-CHGBK-RT            	PIC X(2).
			lineResult += utils.Hex2string(strBlock[161:163]) + "|"             //AH-POS-MODE            	PIC X(2).
			lineResult += utils.Hex2string_comp3(strBlock[163:168])[0:9] + "|"  //AH-XBORDER-FEE   		PIC S9(07)V99 	COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[168:173])[0:9] + "|"  //AH-MARKUP-AMT    		PIC S9(07)V99 	COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[173:178])[0:9] + "|"  //AH-CCA-AMT       		PIC S9(07)V99 	COMP-3.
			lineResult += utils.Hex2string(strBlock[178:179]) + "|"             //AH-DCC-FLAG      		PIC X(01).
			lineResult += utils.Hex2string(strBlock[179:191])                   //FILLER      			PIC X(12).
			lineResult += "\r\n"
		}

		return lineResult
	})

}
