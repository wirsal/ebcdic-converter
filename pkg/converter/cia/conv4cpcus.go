package cia

import (
	"fmt"
	"log"
	"strconv"
	"strings"
	"time"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cpcus2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPCUS.length")
	batchSize := viper.GetInt("server.batchSize")

	// Baca file input
	content, err := utils.ReadAllFile(inputFilename)
	if err != nil {
		utils.Error("Failed to read file", err)
		return false
	}

	var (
		numRecord  int
		startParse int
		data       string
	)

	lineLength := len(content)
	if lineLength == 0 {
		log.Println("File kosong.")
		return false
	}

	for startParse < lineLength {
		numRecord++
		// Fixed length section
		lineParse := content[startParse : startParse+recordLength]
		startParse += recordLength

		// Variable length record
		numVarRecHex := content[startParse : startParse+2]
		numVarRec, _ := strconv.ParseInt(utils.Hex2string_comp3(numVarRecHex), 16, 32)
		numVarRecord := int(numVarRec)

		var lineParseVar string
		if numVarRecord != 0 {
			startVar := startParse + 2
			lineParseVar = content[startVar : startVar+numVarRecord]
		}
		startParse += numVarRecord + 2

		// Decode fixed & variable part
		data += decodeCPCUSFixLength(lineParse)
		varData := decodeCPCUSVarLength(lineParseVar, numVarRecord)

		// Validasi acctNmbr
		acctNumber := utils.Hex2string_comp3(lineParse[2:11])[1:17]
		if acctNumber == "0000000000000000" ||
			acctNumber == "9989989999999998" ||
			acctNumber == "9999999999999999" {
			data = ""
		} else {
			data += varData + "\r\n"
		}

		// Flush if hit batchSize
		if numRecord == batchSize {
			if !utils.WriteAndCheck(outputFilename, data) {
				return false
			}
			numRecord = 0
			data = ""
		}
	}

	// Tulis sisa data
	if len(data) > 0 {
		if !utils.WriteAndCheck(outputFilename, data) {
			return false
		}
	}

	utils.Info("%s Finished in %s", inputFilename, time.Since(start))
	return true
}

func decodeCPCUSFixLength(lineParse string) string {
	return utils.SafeDecode("decodeCPCUSFixLength", func() string {
		var lineResult string
		lineResult += utils.Hex2string_comp3(lineParse[0:2])[0:3] + "|"         // ORG NUMBER
		lineResult += utils.Hex2string_comp3(lineParse[2:11])[1:17] + "|"       // ACCT NUMBER
		lineResult += utils.Hex2string(lineParse[11:12]) + "|"                  // CUSTOMER STATUS
		lineResult += utils.Hex2string(lineParse[12:27]) + "|"                  // CUSTOMER SHORT NAME
		lineResult += utils.Hex2string(lineParse[27:57]) + "|"                  // CUSTOMER CO OWNER
		lineResult += utils.Hex2string(lineParse[57:66]) + "|"                  // ZIP CODE
		lineResult += utils.Hex2string_comp3(lineParse[66:71])[0:9] + "|"       // CREDIT LIMIT
		lineResult += utils.Hex2string_comp3(lineParse[71:74])[1:5] + "|"       // SMSA
		lineResult += utils.Hex2string_comp3(lineParse[74:78])[0:7] + "|"       // CENSUS TRACK
		lineResult += utils.Hex2string(lineParse[78:79]) + "|"                  // TAX ID TYPE
		lineResult += utils.Hex2string(lineParse[79:94]) + "|"                  // TAX ID NUMBER
		lineResult += utils.Hex2string(lineParse[94:95]) + "|"                  // CO TAX ID TYPE
		lineResult += utils.Hex2string(lineParse[95:110]) + "|"                 // CO TAX ID NUMBER
		lineResult += utils.Hex2string_comp3(lineParse[110:114])[0:7] + "|"     // BIRTH DATE
		lineResult += utils.Hex2string_comp3(lineParse[114:118])[0:7] + "|"     // CO BIRTH DATE
		lineResult += utils.Hex2string_comp3(lineParse[118:122])[0:7] + "|"     // DATE LAST MAINT
		lineResult += utils.Hex2string(lineParse[122:140]) + "|"                // HOME PHONE
		lineResult += utils.Hex2string(lineParse[140:158]) + "|"                // CO HOME PHONE
		lineResult += utils.Hex2string(lineParse[158:176]) + "|"                // OFFICE PHONE
		lineResult += utils.Hex2string(lineParse[176:194]) + "|"                // CO OFFICE PHONE
		lineResult += utils.Hex2string(lineParse[194:195]) + "|"                // OFFICE PHONE FLAG
		lineResult += utils.Hex2string(lineParse[195:196]) + "|"                // CO OFFICE PHONE FLAG
		lineResult += utils.Hex2string(lineParse[196:226]) + "|"                // EMPLOYER
		lineResult += utils.Hex2string(lineParse[226:256]) + "|"                // CO EMPLOYER
		lineResult += utils.Hex2string_comp3(lineParse[256:261])[0:9] + "|"     // INSTALL LINE
		lineResult += utils.Hex2string_comp3(lineParse[261:266])[0:9] + "|"     // CASH LINE
		lineResult += utils.ParseComp3SignedMode(lineParse[266:272], "d") + "|" // AVAIL CASH (V99)
		lineResult += utils.Hex2string(lineParse[272:273]) + "|"                // FOREIGN COUNTRY INDICATOR
		lineResult += utils.Hex2string(lineParse[273:275]) + "|"                // BILLING CYCLE
		lineResult += utils.ParseComp3SignedMode(lineParse[275:281], "d") + "|" // AVAILABLE CREDIT (V99)
		lineResult += utils.Hex2string(lineParse[281:282]) + "|"                // CREDIT LIABLE FLAG
		lineResult += utils.Hex2string(lineParse[282:283]) + "|"                // ADDL USAGE
		lineResult += utils.Hex2string(lineParse[283:284]) + "|"                //
		lineResult += utils.Hex2string(lineParse[284:285]) + "|"                //
		lineResult += utils.Hex2string(lineParse[285:286]) + "|"                //
		lineResult += utils.Hex2string(lineParse[286:287]) + "|"                //
		lineResult += utils.Hex2string(lineParse[287:288]) + "|"                //
		lineResult += utils.Hex2string(lineParse[288:289]) + "|"                //
		lineResult += utils.Hex2string(lineParse[289:290]) + "|"                //
		lineResult += utils.Hex2string(lineParse[290:291]) + "|"                //
		lineResult += utils.Hex2string(lineParse[291:292]) + "|"                //
		lineResult += utils.Hex2string(lineParse[292:293]) + "|"                //
		lineResult += utils.Hex2string(lineParse[293:294]) + "|"                //
		lineResult += utils.Hex2string(lineParse[294:295]) + "|"                // ADDL INFO TAG
		lineResult += utils.Hex2string(lineParse[295:299]) + "|"                // MEMBER SINCE
		lineResult += utils.Hex2string_comp3(lineParse[299:301])[0:3] + "|"     // OVERLIMIT (V99)
		lineResult += utils.Hex2string(lineParse[301:303]) + "|"                // FILLER
		lineResult += utils.Hex2string(lineParse[303:333]) + "|"                // EU SURNAME
		lineResult += utils.Hex2string(lineParse[333:338]) + "|"                // EU INITS
		lineResult += utils.Hex2string(lineParse[338:343]) + "|"                // EU TITLE
		lineResult += utils.Hex2string(lineParse[343:344]) + "|"                // EU SEX
		lineResult += utils.Hex2string_comp3(lineParse[344:346])[0:3] + "|"     // EU BEHAV SCORE
		lineResult += utils.Hex2string(lineParse[346:347]) + "|"                // HOME OWNER
		lineResult += utils.Hex2string(lineParse[347:349]) + "|"                // EU TYPE OF RES
		lineResult += utils.Hex2string_comp3(lineParse[349:352])[1:5] + "|"     // EU PER OF RES
		lineResult += utils.Hex2string(lineParse[352:356]) + "|"                // EU ACORN CODE
		lineResult += utils.Hex2string(lineParse[356:360]) + "|"                // EU LEZ CODE
		lineResult += utils.Hex2string(lineParse[360:362]) + "|"                // EU BANK ACCOUNT IND
		lineResult += utils.Hex2string(lineParse[362:363]) + "|"                // EU MARITAL STATUS
		lineResult += utils.Hex2string(lineParse[363:365]) + "|"                // EU NUMBER OF DEPS
		lineResult += utils.Hex2string(lineParse[365:367]) + "|"                // EU OCCPN CODE
		lineResult += utils.Hex2string_comp3(lineParse[367:370])[1:5] + "|"     // EU PER OCCPN
		lineResult += utils.Hex2string(lineParse[370:372]) + "|"                // EU CUSTOMER CLASS
		lineResult += utils.Hex2string(lineParse[372:376]) + "|"                // EU EMPLOYER CODE
		lineResult += utils.Hex2string(lineParse[376:377]) + "|"                // EU DIRECT EMAIL
		lineResult += utils.Hex2string(lineParse[377:378]) + "|"                // EU JNT OWNERSHIP
		lineResult += utils.Hex2string(lineParse[378:379]) + "|"                // EU GUARANTOR
		lineResult += utils.Hex2string_comp3(lineParse[379:384])[0:9] + "|"     // CREDIT LIMIT PERMANENT
		lineResult += utils.Hex2string_comp3(lineParse[384:389])[0:9] + "|"     // CREDIT LIMIT TEMP
		lineResult += utils.Hex2string_comp3(lineParse[389:393])[0:7] + "|"     // CREDIT LIMIT TEMP EFF DATE
		lineResult += utils.Hex2string_comp3(lineParse[393:397])[0:7] + "|"     // CREDIT LIMIT TEMP EXP DATE
		lineResult += utils.Hex2string_comp3(lineParse[397:403])[0:11] + "|"    // AVAILABLE INSTALLMENT
		lineResult += utils.Hex2string(lineParse[403:423]) + "|"                // PLACE OF BIRTH
		lineResult += utils.Hex2string(lineParse[423:436]) + "|"                // APPLICATION ID
		lineResult += utils.Hex2string(lineParse[436:440]) + "|"                // BUSINESS CODE
		lineResult += utils.Hex2string(lineParse[440:442]) + "|"                // EDUCATION
		lineResult += utils.Hex2string(lineParse[442:492]) + "|"                // EMAIL
		lineResult += utils.Hex2string(lineParse[492:495]) + "|"                // FILLER

		return lineResult
	})
}

func decodeCPCUSVarLength(lineParseVar string, numVarRecord int) string {
	return utils.SafeDecode("decodeCPCUSFixLength", func() string {
		dataWithSeparator, startVarParse := "", 0
		if len(lineParseVar) == 0 {
			return dataWithSeparator
		}
		// Inisialisasi 13 kolom kosong
		tempVarResult := make([]string, 13)
		for i := range tempVarResult {
			tempVarResult[i] = " "
		}

		for startVarParse < numVarRecord && startVarParse < 466 {
			// Ambil panjang field variable (1 byte)
			lengthVar, _ := strconv.ParseInt(utils.Hex2string_comp3(lineParseVar[startVarParse:startVarParse+1]), 16, 32)

			// Validasi panjang tidak melebihi batas buffer
			if startVarParse+int(lengthVar)+1 >= numVarRecord {
				break
			}

			// Ambil type ID dari 1 byte berikutnya
			parseType, _ := strconv.Atoi(utils.Hex2string_comp3(lineParseVar[startVarParse+1 : startVarParse+2]))

			// Map parseType ke posisi kolom
			parsePosition := mapParseTypeToPosition(parseType)

			if parsePosition == -1 {
				fmt.Println("Unknown parseType:", parseType)
				startVarParse += int(lengthVar) + 2
				continue
			}

			// Ambil data field variable
			varStart := startVarParse + 2
			varEnd := varStart + int(lengthVar)
			tempVarParse := utils.Hex2string(lineParseVar[varStart:varEnd])

			// Simpan pada posisi yang sesuai
			tempVarResult[parsePosition] = tempVarParse
			startVarParse += int(lengthVar) + 2
		}

		// Format hasil akhir
		for i, value := range tempVarResult {
			if i == 11 || i == 12 {
				tempVarResult[i] = fmt.Sprintf("%-55s", value)
			} else {
				tempVarResult[i] = fmt.Sprintf("%-30s", value)
			}
		}

		dataWithSeparator = strings.Join(tempVarResult, "|")
		return dataWithSeparator
	})
}

func mapParseTypeToPosition(parseType int) int {
	switch parseType {
	case 11:
		return 0
	case 12:
		return 1
	case 13:
		return 2
	case 21:
		return 3
	case 22:
		return 4
	case 23:
		return 5
	case 24:
		return 6
	case 25:
		return 7
	case 26:
		return 8
	case 27:
		return 9
	case 31:
		return 10
	case 41:
		return 11
	case 42:
		return 12
	default:
		return -1
	}
}

func CombineCpcus(file1, file2, output string) bool {
	start := time.Now()
	fileInput1 := utils.GetCurrentResultDir() + "/" + file1
	fileInput2 := utils.GetCurrentResultDir() + "/" + file2
	fileOutput := utils.GetCurrentResultDir() + "/" + output
	err := utils.MergeFiles(fileInput1, fileInput2, fileOutput)
	if err != nil {
		utils.Error("Failed to combine CPCUS files: %v", err)
		return false
	}
	utils.Info("Finished combine CPCUS into %s in %s", output, time.Since(start))
	return true
}
