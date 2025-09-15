package cedc

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Oadctf2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.OADCTF.length")
	batchSize := viper.GetInt("server.batchSize")

	// Baca file input
	content, err := utils.ReadAllFile(inputFilename)
	if err != nil {
		utils.Error("Failed to read file: %v", err)
		return false
	}

	totalRecords := utf8.RuneCountInString(content) / recordLength
	fileLength := len([]byte(content))

	var (
		data    string
		counter int
	)

	for line := 0; line <= totalRecords; line++ {

		startIdx := recordLength * line
		endIdx := recordLength * (line + 1)

		if startIdx >= fileLength {
			break
		}
		if endIdx > fileLength {
			endIdx = fileLength
		}

		record := content[startIdx:endIdx]
		decoded := decodeOADCTF(record)
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

func decodeOADCTF(strBlock string) string {
	return utils.SafeDecode("decodeOADCTF", func() string {
		var lineResult string
		merchNmbr := utils.Hex2string_comp3(strBlock[0:7])[0:13]

		if merchNmbr != "0000000000000" && merchNmbr != "FFFFFFFFFFFFF" {
			lineResult += merchNmbr + "|"                                       //OADCT-MERCH-NBR				PIC S9(13)  	COMP-3.
			lineResult += utils.Hex2string(strBlock[7:15]) + "|"                //OADCT-TERM-ID					PIC X(8).
			lineResult += utils.Hex2string(strBlock[15:16]) + "|"               //OADCT-EDC-IND  				PIC X.
			lineResult += utils.Hex2string(strBlock[16:17]) + "|"               //OADCT-TERM-IND 				PIC X.
			lineResult += utils.Hex2string(strBlock[17:18]) + "|"               //OADCT-ACTION-IND				PIC X.
			lineResult += utils.Hex2string_comp3(strBlock[18:21])[0:5] + "|"    //OADCT-CURRENT-BATCH-NBR 	  	PIC S9(5) 		COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[21:24])[0:5] + "|"    //OADCT-EOD-CUTOFF-BATCH-NBR   	PIC S9(5)  		COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[24:28])[0:7] + "|"    //OADCT-LAST-MAINT-DATE			PIC S9(7)  		COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[28:32])[0:7] + "|"    //OADCT-LAST-SETTLEMENT-DATE		PIC S9(7)  		COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[32:35])[0:5] + "|"    //OADCT-TOTAL-DEBIT-NBR1		PIC S9(5)  		COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[35:43])[0:15] + "|"   // OADCT-TOTAL-DEBIT-AMT1      PIC S9(13)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[43:46])[0:5] + "|"    // OADCT-TOTAL-CREDIT-NBR1     PIC S9(5) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[46:54])[0:15] + "|"   // OADCT-TOTAL-CREDIT-AMT1     PIC S9(13)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[54:57])[0:5] + "|"    // OADCT-TOTAL-DEBIT-NBR2      PIC S9(5) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[57:65])[0:15] + "|"   // OADCT-TOTAL-DEBIT-AMT2      PIC S9(13)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[65:68])[0:5] + "|"    // OADCT-TOTAL-CREDIT-NBR2     PIC S9(5) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[68:76])[0:15] + "|"   // OADCT-TOTAL-CREDIT-AMT2     PIC S9(13)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[76:78])[0:3] + "|"    // OADCT-CUR-TOTAL-DEBIT-NBR   PIC S9(3) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[78:85])[0:13] + "|"   // OADCT-CUR-TOTAL-DEBIT-AMT   PIC S9(11)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[85:87])[0:3] + "|"    // OADCT-CUR-TOTAL-CREDIT-NBR  PIC S9(3) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[87:94])[0:13] + "|"   // OADCT-CUR-TOTAL-CREDIT-AMT  PIC S9(11)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[94:96])[0:3] + "|"    // OADCT-VISA-TOTAL-NBR        PIC S9(3) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[96:103])[0:13] + "|"  // OADCT-VISA-TOTAL-AMOUNT     PIC S9(11)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[103:105])[0:3] + "|"  // OADCT-MC-TOTAL-NBR          PIC S9(3) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[105:112])[0:13] + "|" // OADCT-MC-TOTAL-AMOUNT       PIC S9(11)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[112:114])[0:3] + "|"  // OADCT-PRIVATE-TOTAL-NBR     PIC S9(3) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[114:121])[0:13] + "|" // OADCT-PRIVATE-TOTAL-AMOUNT  PIC S9(11)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[121:125])[0:7] + "|"  // OADCT-T4-PREVIOUS-BATCH-NBR PIC S9(7) COMP-3
			lineResult += utils.Hex2string(strBlock[125:126]) + "|"             // OADCT-TRM-CV2-IND           PIC X(1)
			lineResult += utils.Hex2string(strBlock[126:128]) + "|"             // OADCT-TRM-CV2-RESP          PIC X(2)
			lineResult += utils.Hex2string(strBlock[128:197]) + "|"             // FILLER                      PIC X(69)
			lineResult += utils.Hex2string(strBlock[197:198]) + "|"             // OADCT-TRM-CHIP-IND          PIC X(1)
			lineResult += utils.Hex2string(strBlock[198:199]) + "|"             // OADCT-CLCB-IND              PIC X(1)
			lineResult += utils.Hex2string(strBlock[199:200]) + "\r\n"          // OADCT-LOG-XFER-IND          PIC X(1)
		}
		return lineResult
	})
}
