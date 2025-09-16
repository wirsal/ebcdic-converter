package cia

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cpols2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPS130.length")
	batchSize := viper.GetInt("server.batchSize")

	// Read Inputfile
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
		strBlock := content[startIdx:endIdx]
		output := decodeCPOLS(strBlock)

		line++
		counter++
		if len(output) != 0 {
			data += output
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

// Decode CPOLST and mappring  from EBCDIC to ASCII
func decodeCPOLS(strBlock string) string {
	return utils.SafeDecode("decodeCPOLS", func() string {
		var lineResult string

		// Skip header & trailer
		val := utils.Hex2string_comp3(strBlock[0:2])[0:3]
		if val != "000" && val != "999" {
			lineResult += utils.Hex2string_comp3(strBlock[0:2])[0:3] + "|"   //OLS-ORG-NBR     PIC S9(03)          COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[2:4])[0:3] + "|"   //OLS-TYP-NBR     PIC S9(03)          COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[4:13])[1:17] + "|" //OLS-ACT-NBR     PIC S9(16)          COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[13:15])[0:3] + "|" //OLS-STM-NBR     PIC S9(03)          COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[15:19])[0:7] + "|" //OLS-SEQ-NBR     PIC S9(07)          COMP-3.
			if utils.Hex2string_comp3(strBlock[15:19])[0:7] != "0000000" {
				lineResult += utils.Hex2string_comp3(strBlock[19:23])[0:7] + "|"    //OLS-TX-POSTING-DATE     PIC S9(7)       COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[23:27])[0:7] + "|"    //OLS-TX-EFFECTIVE-DATE   PIC S9(7)       COMP-3.
				lineResult += utils.Hex2string(strBlock[27:29]) + "|"               //OLS-TX-CODE     PIC 9(2).
				lineResult += utils.Hex2string_comp3(strBlock[29:34])[0:9] + "|"    //OLS-TX-AMT      PIC S9(7)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[34:40])[0:11] + "|"   //OLS-TX-REF-NBR  PIC S9(11)      COMP-3.
				lineResult += utils.Hex2string(strBlock[40:63]) + "|"               //OLS-TX-ORIG-REF-NBR  PIC X(23). >>>
				lineResult += utils.Hex2string(strBlock[63:88]) + "|"               //OLS-TX-DESC-NAME     PIC X(25).
				lineResult += utils.Hex2string(strBlock[88:101]) + "|"              //OLS-TX-DESC-CITY     PIC X(13).
				lineResult += utils.Hex2string(strBlock[101:103]) + "|"             //OLS-TX-DESC-COUNTRY  PIC X(2).
				lineResult += utils.Hex2string(strBlock[103:105]) + "|"             //OLS-TX-MERCH-STATE   PIC X(2). >>>
				lineResult += utils.Hex2string(strBlock[105:106]) + "|"             //OLS-TX-INTL-FEE-IND  PIC X.
				lineResult += utils.Hex2string_comp3(strBlock[106:109])[0:5] + "|"  //OLS-TX-MERCH-CAT     PIC S9(5)       COMP-3.
				lineResult += utils.Hex2string(strBlock[109:115]) + "|"             //OLS-TX-AUTH-CODE     PIC X(6).
				lineResult += utils.Hex2string(strBlock[115:116]) + "|"             //OLS-TX-REIMB-ATT     PIC X.
				lineResult += utils.Hex2string_comp3(strBlock[16:118])[0:3] + "|"   //OLS-TX-ORIG-CURR-CODE   PIC S9(3)           COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[118:125])[1:13] + "|" //OLS-TX-ORIG-CURR-AMT    PIC S9(10)V99       COMP-3.
				lineResult += utils.Hex2string(strBlock[125:127]) + "|"             //OLS-TX-ORIG-CURR-DECIMAL  PIC 99.
				lineResult += utils.Hex2string(strBlock[127:128]) + "|"             //OLS-TX-ONUS-CURR-CONV     PIC 9.
				lineResult += utils.Hex2string_comp3(strBlock[128:131])[1:5] + "|"  //OLS-TX-SOURCE-CODE        PIC S9(4)       COMP-3.
				lineResult += utils.Hex2string(strBlock[131:146]) + "|"             //OLS-TX-NM-MERCH-ID        PIC X(15).
				lineResult += utils.Hex2string(strBlock[146:154]) + "|"             //OLS-TX-NM-TERM-ID         PIC X(08).
				lineResult += utils.Hex2string_comp3(strBlock[154:162])[0:15] + "|" //OLS-TX-PS2000-TRAN-ID     PIC S9(15)    COMP-3.
				lineResult += utils.Hex2string(strBlock[162:163]) + "|"             //OLS-TX-PS2000-RPS      PIC X.
				lineResult += utils.Hex2string(strBlock[163:164]) + "|"             //OLS-TX-MAIL-IND      PIC X.
				lineResult += utils.Hex2string(strBlock[164:168]) + "|"             //OLS-TX-ORIG-CPD     PIC X(4).
				lineResult += utils.Hex2string(strBlock[168:170]) + "|"             //OLS-TX-CHGBK-RT     PIC X(2).
				lineResult += utils.Hex2string(strBlock[170:172]) + "|"             //OLS-TX-POS-MODE     PIC X(2).
				lineResult += utils.Hex2string_comp3(strBlock[172:177])[0:9] + "|"  //OLS-TX-XBORDER-FEE  PIC S9(07)V99 COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[177:182])[0:9] + "|"  //OLS-TX-MARKUP-AMT   PIC S9(07)V99 COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[182:197])[0:9] + "|"  //OLS-TX-CCA-AMT      PIC S9(07)V99 COMP-3.
				lineResult += utils.Hex2string(strBlock[187:188]) + "|"             //OLS-TX-DCC-FLAG     PIC X(01).
				lineResult += utils.Hex2string(strBlock[188:200])                   //FILLER      PIC X(12).
				lineResult += "\r\n"
			} else {

				lineResult += utils.Hex2string_comp3(strBlock[19:21])[0:3] + "|"     //OLS-CUST-ORG    PIC S9(3)           COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[21:30])[1:17] + "|"    //OLS-CUST-NBR    PIC S9(16)          COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[30:34])[0:7] + "|"     //OLS-STMT-DATE   PIC S9(7)           COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[34:39])[0:9] + "|"     //OLS-CRLIMIT     PIC S9(9)           COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[39:45])[0:11] + "|"    //OLS-AVAIL-CREDIT    PIC S9(9)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[45:49])[0:7] + "|"     //OLS-PYMT-DUE-DATE   PIC S9(7)       COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[49:51])[0:3] + "|"     //OLS-CYCLE-DAYS      PIC S9(3)       COMP-3.
				lineResult += utils.ParseComp3SignedMode(strBlock[51:57], "d") + "|" //OLS-PREV-BAL        PIC S9(9)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[57:63])[0:11] + "|"    //OLS-CREDIT-AMT      PIC S9(9)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[63:69])[0:11] + "|"    //OLS-DEBIT-AMT       PIC S9(9)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[69:75])[0:11] + "|"    //OLS-CASH-ADV-AMT    PIC S9(9)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[75:81])[0:11] + "|"    //OLS-FINANCE-CHARGE  PIC S9(9)V99    COMP-3.
				lineResult += utils.ParseComp3SignedMode(strBlock[81:87], "d") + "|" //OLS-CURR-BAL        PIC S9(9)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[87:93])[0:11] + "|"    //OLS-MONTHLY-PAYMENT   PIC S9(9)V99    COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[93:99])[0:11] + "|"    //OLS-PAST-DUE-AMT      PIC S9(9)V99    COMP-3..
				lineResult += utils.Hex2string(strBlock[99:100]) + "|"               //OLS-STMT-TYPE         PIC 9.
				lineResult += utils.Hex2string(strBlock[100:101]) + "|"              //OLS-OUT-OF-BAL-FLAG   PIC X.
				lineResult += utils.Hex2string_comp3(strBlock[101:103])[0:3] + "|"   //OLS-SEC-CUST-ORG      PIC S9(3)       COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[103:112])[1:17] + "|"  //OLS-SEC-CUST-NBR      PIC S9(16)      COMP-3.
				lineResult += utils.Hex2string(strBlock[112:113]) + "|"              //OLS-SEC-BLOCK-CODE    PIC X.
				lineResult += utils.Hex2string(strBlock[113:114]) + "|"              //OLS-DUAL-MAILING-FLAG PIC X.
				lineResult += utils.Hex2string(strBlock[114:115]) + "|"              //OLS-SUMMARY-INDICATOR PIC X.
				lineResult += utils.Hex2string(strBlock[115:200]) + ""               //FILLER          PIC X(85).
				lineResult += "\r\n"
			}
		}
		return lineResult
	})
}
