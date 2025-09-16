package cedc

import (
	"strconv"
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Oadclog2txt(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.OADCLG.length")
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
		decoded := decodeOADCLOG(record)
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

func decodeOADCLOG(strBlock string) string {
	return utils.SafeDecode("decodeOADCLOG", func() string {
		var lineResult string
		head := utils.Hex2string_comp3(strBlock[0:7])[0:14]
		if head != "00000000000000" && head != "FFFFFFFFFFFFFF" {
			lineResult += utils.Hex2string(strBlock[0:8]) + "|"               //OADCL-TERM-ID  PIC X(8)
			lineResult += utils.Hex2string(strBlock[8:13]) + "|"              //OADCL-BATCH-NBR     PIC 9(5)
			lineResult += utils.Hex2string(strBlock[13:14]) + "|"             //OADCL-TXM-IND     PIC X
			lineResult += utils.Hex2string(strBlock[14:20]) + "|"             //OADCL-SYSTEM-TRACE   PIC 9(6)
			lineResult += utils.Hex2string_comp3(strBlock[20:24])[1:7] + "|"  //OADCL-TXN-DATE PIC S9(07) COMP-3		YYMMDD
			lineResult += utils.Hex2string_comp3(strBlock[24:28])[1:7] + "|"  //OADCL-TXN-TIME   PIC S9(07) COMP-3  	HHMMSS
			lineResult += utils.Hex2string(strBlock[28:29]) + "|"             //OADCL-REC-TYPE PIC X
			lineResult += utils.Hex2string(strBlock[29:30]) + "|"             //OADCL-REC-STATUS	PIC X
			lineResult += utils.Hex2string_comp3(strBlock[30:37])[0:13] + "|" //OADCL-MERCH-NBR PIC S9(13)

			switch utils.Hex2string(strBlock[28:29]) {
			case "H":
				lineResult += utils.Hex2string_comp3(strBlock[37:39])[0:3] + "|"  //OADCL-VISA-TOTAL-NBR S9(3)
				lineResult += utils.Hex2string_comp3(strBlock[37:46])[0:13] + "|" //OADCL-VISA-TOTAL-AMOUNT S9(11)V99
				lineResult += utils.Hex2string_comp3(strBlock[46:48])[0:3] + "|"  //OADCL-MC-TOTAL-NBR S9(3)
				lineResult += utils.Hex2string_comp3(strBlock[48:55])[0:13] + "|" //OADCL-MC-TOTAL-AMOUNT S9(11)V99
				lineResult += utils.Hex2string_comp3(strBlock[55:57])[0:3] + "|"  //OADCL-EDC-TOTAL-NBR S9(3)
				lineResult += utils.Hex2string_comp3(strBlock[57:64])[0:13] + "|" //OADCL-EDC-TOTAL-AMOUNT S9(11)V99
				lineResult += utils.Hex2string_comp3(strBlock[64:66])[0:3] + "|"  //OADCL-TERMINAL-TOTAL-NBR S9(3)
				lineResult += utils.Hex2string_comp3(strBlock[66:73])[0:13] + "|" //OADCL-TERMINAL-TOTAL-AMOUNT S9(11)V99
				lineResult += utils.Hex2string_comp3(strBlock[73:75])[0:3] + "|"  //OADCL-PRIVATE-TOTAL-NBR S9(3)
				lineResult += utils.Hex2string_comp3(strBlock[75:82])[0:13] + "|" //OADCL-PRIVATE-TOTAL-AMOUNT S9(11)V99
				lineResult += "0000000" + "|"                                     //OADCL-TERMINAL-BATCH-NBR 9(7)
			case "T":
				lineResult += utils.Hex2string_comp3(strBlock[37:40])[0:5] + "|"  //OADCL-MTI S9(5)
				lineResult += utils.Hex2string(strBlock[40:46]) + "|"             //OADCL-PROC-CODE 9(6)
				lineResult += utils.Hex2string(strBlock[46:48]) + "|"             //OADCL-RESPONSE-CODE XX
				lineResult += utils.Hex2string(strBlock[48:54]) + "|"             //OADCL-APPROVAL-CODE X(6)
				lineResult += utils.Hex2string_comp3(strBlock[54:63])[0:17] + "|" //OADCL-CARDHOLDER-NBR PIC S9(17)
				lineResult += utils.Hex2string(strBlock[63:65]) + "|"             //OADCL-EXPIRY-DATE-MM 99
				lineResult += utils.Hex2string(strBlock[65:67]) + "|"             //OADCL-EXPIRY-DATE-YY 99
				lineResult += utils.Hex2string_comp3(strBlock[67:74])[0:13] + "|" //OADCL-TXN-AMOUNT S9(11)V99
				lineResult += utils.Hex2string_comp3(strBlock[74:81])[0:3] + "|"  //OADCL-ORIGINAL-AMOUNT S9(11)V99
				lineResult += utils.Hex2string(strBlock[81:86]) + "|"             //OADCL-ORIG-TXN-BATCH-NBR 9(5)
				lineResult += utils.Hex2string(strBlock[86:87]) + "|"             //OADCL-ORIG-TXM-IND X
				lineResult += utils.Hex2string(strBlock[87:93]) + "|"             //OADCL-ORIG-TXN-SYSTEM-TRACE 9(6)
				lineResult += utils.Hex2string_comp3(strBlock[93:95])[0:3] + "|"  //OADCL-NII S9(3)
				lineResult += utils.Hex2string(strBlock[95:97]) + "|"             //OADCL-POS-COND-CODE 9(2)
				lineResult += utils.Hex2string(strBlock[97:98]) + "|"             //OADCL-EDC-TRN-TYPE X
			case "Z":
			case "S":
				lineResult += utils.Hex2string_comp3(strBlock[48:50])[0:3] + "|"  //OADCL-TOTAL-DEBIT-NBR S9(3)
				lineResult += utils.Hex2string_comp3(strBlock[50:57])[0:13] + "|" //OADCL-TOTAL-DEBIT-AMOUNT S9(11)V99
				lineResult += utils.Hex2string_comp3(strBlock[57:59])[0:3] + "|"  //OADCL-TOTAL-CREDIT-NBR S9(3)
				lineResult += utils.Hex2string_comp3(strBlock[59:66])[0:13] + "|" //OADCL-TOTAL-CREDIT-AMOUNT S9(11)V99
				lineResult += utils.Hex2string_comp3(strBlock[66:70])[0:7] + "|"  //OADCL-S-TERMINAL-BATCH-NBR 9(7) comp3

			}
			lineResult += utils.Hex2string(strBlock[98:99]) + "|"               //OADCL-MESSAGE-FORMAT X
			lineResult += utils.Hex2string(strBlock[104:107]) + "|"             //OADCL-PRODUCT-CODE 1 X3
			lineResult += utils.Hex2string(strBlock[107:110]) + "|"             //OADCL-PRODUCT-CODE 2 X3
			lineResult += utils.Hex2string(strBlock[110:113]) + "|"             //OADCL-PRODUCT-CODE 3 X3
			lineResult += utils.Hex2string(strBlock[113:116]) + "|"             //OADCL-PRODUCT-CODE 4 X3
			lineResult += utils.Hex2string(strBlock[115:117]) + "|"             //OADCL-B022-POS-EM-PAN X2
			lineResult += utils.Hex2string(strBlock[118:119]) + "|"             //OADCL-B022-POS-EM-PIN-CAP  X
			lineResult += utils.Hex2string(strBlock[119:125]) + "|"             //OADCL-B062-INVOICE-NBR   9(6)
			lineResult += utils.Hex2string(strBlock[125:126]) + "|"             //OADCL-PS2000-AUTH-CHAR-IND  X
			lineResult += utils.Hex2string_comp3(strBlock[126:134])[0:15] + "|" //OADCL-PS2000-TXN-IDENTIFIER S9(15)
			lineResult += utils.Hex2string(strBlock[134:138]) + "|"             //OADCL-PS2000-VLDTN-CODE  X(04)
			lineResult += utils.Hex2string(strBlock[138:139]) + "|"             //OADCL-VISA-AUTH-SRC-CODE   X(01)
			lineResult += utils.Hex2string(strBlock[139:143]) + "|"             //OADCL-BANKNET-DATE  PIC X(04)
			lineResult += utils.Hex2string(strBlock[143:146]) + "|"             //OADCL-FNCL-NTWK-CDE   X(03)
			lineResult += utils.Hex2string(strBlock[146:155]) + "|"             //OADCL-BANKNET-REF-NBR  X(09)
			lineResult += utils.Hex2string(strBlock[155:156]) + "|"             //OADCL-CONNECTION-TYPE   PIC X(01)
			lineResult += utils.Hex2string(strBlock[156:157]) + "|"             //OADCL-INSTL-IND         PIC X(01)
			lineResult += utils.Hex2string(strBlock[157:160]) + "|"             //OADCL-INSTL-PLAN        PIC 9(03)
			lineResult += utils.Hex2string(strBlock[160:162]) + "|"             //OADCL-PAY-TERM          PIC 9(02)
			lineResult += utils.Hex2string(strBlock[162:163]) + "|"             //OADCL-COMPUTE-METHOD    PIC 9(01)
			lineResult += utils.Hex2string_comp3(strBlock[163:167])[0:6] + "|"  //OADCL-INTR-RATE         PIC 9V9(5)
			lineResult += utils.Hex2string(strBlock[167:169]) + "|"             //OADCL-INTR-FREE-MOS     PIC 9(02)
			lineResult += utils.Hex2string_comp3(strBlock[169:174])[0:9] + "|"  //OADCL-FIRST-PAY-AMT     PIC S9(07)V99
			lineResult += utils.Hex2string_comp3(strBlock[174:179])[0:9] + "|"  //OADCL-LAST-PAY-AMT      PIC S9(07)V99
			lineResult += utils.Hex2string_comp3(strBlock[179:184])[0:9] + "|"  //OADCL-MON-INSTL-AMT     PIC S9(07)V99
			lineResult += utils.Hex2string_comp3(strBlock[184:189])[0:9] + "|"  //OADCL-OUTS-PRINCIPAL    PIC S9(07)V99
			lineResult += utils.Hex2string_comp3(strBlock[189:194])[0:9] + "|"  //OADCL-OUTS-INTEREST     PIC S9(07)V99
			lineResult += utils.Hex2string_comp3(strBlock[194:199])[0:9] + "|"  //OADCL-HANDLING-FEE      PIC S9(07)V99
			lineResult += utils.Hex2string(strBlock[199:200]) + "|"             //OADCL-PLAN-TYPE         PIC X(01)
			lineResult += utils.Hex2string_comp3(strBlock[200:202])[0:3] + "|"  //OADCL-B023-CARD-SEQ-NBR PIC S9(3)
			//OADCL-B055-CHIP-DATA-LEN      PIC S9(04)
			//OADCL-B055-CHIP-DATA         PIC X(255)
			lineResult += utils.Hex2string_comp3(strBlock[459:462])[0:5] + "|"  //OADCL-CLCB-PROG-ID      PIC  9(05) comp3
			lineResult += utils.Hex2string_comp3(strBlock[462:469])[0:13] + "|" //OADCL-B063-SALES-AMT  PIC S9(11)V99
			lineResult += utils.Hex2string_comp3(strBlock[469:476])[0:13] + "|" //OADCL-B063-REDEEMED-AMT PIC S9(11)V99
			lineResult += utils.Hex2string_comp3(strBlock[476:483])[0:13] + "|" //OADCL-B063-NET-SALES-AMT  PIC S9(11)V99
			lineResult += utils.Hex2string_comp3(strBlock[483:488])[0:9] + "|"  //OADCL-B063-REDEEMED-PT    PIC S9(09)
			lineResult += utils.Hex2string_comp3(strBlock[488:493])[0:9] + "|"  //OADCL-B063-BAL-PT         PIC S9(09)
			if utils.Hex2string_comp3(strBlock[37:39])[0:3] == "00220" {
				lineResult += "  | |  |  |"
			} else {
				lineResult += utils.Hex2string(strBlock[493:495]) + "|" //OADCL-B063-PROD-CD        PIC  X(02)
				lineResult += utils.Hex2string(strBlock[495:496]) + "|" //OADCL-CLCB-IND                PIC X(01)
				lineResult += utils.Hex2string(strBlock[496:498]) + "|" //OADCL-WAIVE-FR-MOS      PIC 9(02)
				lineResult += utils.Hex2string(strBlock[498:500]) + "|" //OADCL-WAIVE-TO-MOS      PIC 9(02)
			}
			lineResult += utils.Hex2string_comp3(strBlock[500:502])[0:3] + "|" //OADCL-EXT-SVC-CDE             PIC S9(03)
			lineResult += utils.Hex2string(strBlock[502:503]) + "|"            //OADCL-DE22-CRD-DATA-INP-CAP         PIC X(01)
			lineResult += utils.Hex2string(strBlock[503:504]) + "|"            //OADCL-DE22-CRDHDR-AUTH-CAP         PIC X(01)
			lineResult += utils.Hex2string(strBlock[504:505]) + "|"            //OADCL-DE22-CRD-CAPT-CAP         PIC X(01)
			lineResult += utils.Hex2string(strBlock[505:506]) + "|"            //OADCL-DE22-TRM-OPER-ENV         PIC X(01)
			lineResult += utils.Hex2string(strBlock[506:507]) + "|"            //OADCL-DE22-CRDHDR-PRES-DATA         PIC X(01)
			lineResult += utils.Hex2string(strBlock[507:508]) + "|"            //OADCL-DE22-CRD-PRES-DATA         PIC X(01)
			lineResult += utils.Hex2string(strBlock[508:509]) + "|"            //OADCL-DE22-CRD-DATA-INP-MODE        PIC X(01)
			lineResult += utils.Hex2string(strBlock[509:510]) + "|"            //OADCL-DE22-CRDHDR-AUTH-MTD        PIC X(01)
			lineResult += utils.Hex2string(strBlock[510:511]) + "|"            //OADCL-DE22-CRDHDR-AUTH-ENT          PIC X(01)
			lineResult += utils.Hex2string(strBlock[511:512]) + "|"            //OADCL-DE22-CRD-DATA-OUTP-CAP         PIC X(01)
			lineResult += utils.Hex2string(strBlock[512:513]) + "|"            //OADCL-DE22-TRM-DATA-OUTP-CAP         PIC X(01)
			lineResult += utils.Hex2string(strBlock[513:514]) + "|"            //OADCL-DE22-PIN-CAPT-CAP         PIC X(01)
			lineResult += utils.Hex2string(strBlock[514:516]) + "|"            //OADCL-CARD-LEVEL         PIC X(02)

			//OADCL-B054-ADDT-AMT         PIC S9(10)V99 COMP-3
			nonFareStr := utils.Hex2string_comp3(strBlock[516:523])
			trxAmntStr := utils.Hex2string_comp3(strBlock[67:74])
			nonFareAmt, err1 := strconv.Atoi(nonFareStr[0:10])
			trxAmnt, err2 := strconv.Atoi(trxAmntStr[0:13])
			if nonFareStr[:14] != "40404040404040" && err1 == nil && err2 == nil && trxAmnt > nonFareAmt {
				lineResult += nonFareStr[1:13]
			} else {
				lineResult += "000000000000"
			}

			lineResult += "\r\n"
		}
		return lineResult
	})
}
