package cepp

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Eptrans2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.EPTRANS.length")
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

		decoded := decodeEPTRANS(record)
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

func decodeEPTRANS(strBlock string) string {
	return utils.SafeDecode("decodeEPTRANS", func() string {
		var lineResult string
		lineResult += utils.Hex2string_comp3(strBlock[0:2])[0:3] + "|"      //ORG NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[2:4])[0:3] + "|"      //TYPE NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[4:13])[1:17] + "|"    //CARD NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[13:17])[0:7] + "|"    //TRANSACTION DATE
		lineResult += utils.Hex2string_comp3(strBlock[17:21])[0:7] + "|"    //TRANSACTION TIME
		lineResult += utils.Hex2string_comp3(strBlock[21:23])[0:3] + "|"    //MERCHANT ORG
		lineResult += utils.Hex2string_comp3(strBlock[23:28])[0:3] + "|"    //MERCHANT NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[28:31])[1:5] + "|"    //EXP DATE
		lineResult += utils.Hex2string_comp3(strBlock[31:36])[0:9] + "|"    //ITEM PRICE
		lineResult += utils.Hex2string(strBlock[36:38]) + "|"               //PAY-TERM
		lineResult += utils.Hex2string_comp3(strBlock[38:42])[0:7] + "|"    //INTEREST RATE
		lineResult += utils.Hex2string(strBlock[41:44]) + "|"               //INTEREST MOST
		lineResult += utils.Hex2string_comp3(strBlock[44:48])[0:7] + "|"    //FIRST PAY DATE
		lineResult += utils.Hex2string_comp3(strBlock[48:53])[0:9] + "|"    //FIRST PAYMENT AMOUNT
		lineResult += utils.Hex2string_comp3(strBlock[53:57])[0:7] + "|"    //LAST PAY DATE
		lineResult += utils.Hex2string_comp3(strBlock[57:62])[0:9] + "|"    //LAST PAYMENT AMOUNT
		lineResult += utils.Hex2string_comp3(strBlock[62:67])[0:9] + "|"    //MON INSTALLMENT AMT
		lineResult += utils.Hex2string_comp3(strBlock[67:72])[0:9] + "|"    //OUTSTANDING PRINCIPAL
		lineResult += utils.Hex2string_comp3(strBlock[72:77])[0:9] + "|"    //CURR CYCLE INTEREST
		lineResult += utils.Hex2string(strBlock[77:79]) + "|"               //PAID-TERM
		lineResult += utils.Hex2string(strBlock[79:80]) + "|"               //STATUS
		lineResult += utils.Hex2string(strBlock[80:83]) + "|"               //INSTALLMENT PLAN
		lineResult += utils.Hex2string_comp3(strBlock[83:87])[0:7] + "|"    //PURGED DATE
		lineResult += utils.Hex2string(strBlock[87:93]) + "|"               //AUTH CODE
		lineResult += utils.Hex2string(strBlock[93:118]) + "|"              //MERCHANT NAME
		lineResult += utils.Hex2string_comp3(strBlock[118:123])[0:9] + "|"  //OUTS INTEREST
		lineResult += utils.Hex2string(strBlock[123:133]) + "|"             //SSS NUMBER
		lineResult += utils.Hex2string(strBlock[133:134]) + "|"             //COMP-METHOD
		lineResult += utils.Hex2string_comp3(strBlock[134:139])[0:9] + "|"  //HANDLING FEE
		lineResult += utils.Hex2string_comp3(strBlock[139:145])[0:11] + "|" //ACCUM AMT
		lineResult += utils.Hex2string_comp3(strBlock[145:149])[0:7] + "|"  //ACCUM INT
		lineResult += " " + "|"                                             //FILLER
		lineResult += utils.Hex2string(strBlock[150:158]) + "|"             //TERMINAL ID
		lineResult += utils.Hex2string(strBlock[158:160]) + "|"             //WAIVE FORM
		lineResult += utils.Hex2string(strBlock[160:162]) + "|"             //WAIVE TO
		//OLD CARD INFO
		lineResult += utils.Hex2string_comp3(strBlock[162:164])[0:3] + "|"  //ORG NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[164:166])[0:3] + "|"  //TYPE NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[164:175])[0:17] + "|" //CARDHOLDER NUMBER
		//ORG CARD INFO
		lineResult += utils.Hex2string_comp3(strBlock[175:177])[0:3] + "|"  //ORG NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[177:179])[0:3] + "|"  //TYPE NUMBER
		lineResult += utils.Hex2string_comp3(strBlock[179:188])[0:17] + "|" //CARDHOLDER NUMBER
		//TRX IDENTIFIER
		lineResult += " " + "|"
		lineResult += utils.Hex2string_comp3(strBlock[287:296])[0:17] + "|" //XFER NEW CARD
		lineResult += utils.Hex2string(strBlock[296:316]) + "|"             //SIGN ON
		lineResult += utils.Hex2string_comp3(strBlock[316:320])[0:7] + "|"  //MERCHANT NUMBER

		return lineResult
	})
}
