package cia

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cps110input2txt(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPS110.length")
	batchSize := viper.GetInt("server.batchSize")

	// Baca file input
	content, err := utils.ReadAllFile(inputFilename)
	if err != nil {
		utils.Error("Failed to read file", err)
		return false
	}
	totalRecords := utf8.RuneCountInString(content) / recordLength
	fileLength := len([]byte(content))
	var (
		data    string
		counter int
	)

	for i := 0; i <= totalRecords; i++ {
		startIdx := recordLength * i
		endIdx := recordLength * (i + 1)

		if startIdx >= fileLength {
			break
		}
		if endIdx > fileLength {
			endIdx = fileLength
		}
		record := content[startIdx:endIdx]

		decoded := decodeCPS110(record)
		if len(decoded) > 0 {
			data += decoded
			counter++
		}
		// if counter == 10 {
		// 	break
		// }
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

func decodeCPS110(strBlock string) string {
	return utils.SafeDecode("decodeCPS110", func() string {
		var lineResult string
		recType := utils.Hex2string(strBlock[0:1])
		switch recType {
		case "1":
			lineResult += utils.Hex2string(strBlock[0:1]) + "|"                //UI-H-REC-TYPE       PICTURE X.
			lineResult += utils.Hex2string(strBlock[1:2]) + "|"                //UI-H-STATUS         PICTURE 9. -> Must 2 Completed
			lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|"     //UI-H-BATCH-NBR              PIC S9(5)  COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|"     //UI-H-SEQ-NBR                PIC S999   COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[7:9])[0:3] + "|"     //UI-H-NBR-ITEMS              PIC S999   COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[9:15])[0:11] + "|"   //UI-H-AMOUNT                 PIC S9(9)V99   COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[15:22])[1:13] + "|"  //UI-H-MERCH-NBR              PIC S9(12)  COMP-3.
			lineResult += utils.Hex2string(strBlock[22:23]) + "|"              //UI-H-BATCH-REV      PICTURE X.
			lineResult += utils.Hex2string(strBlock[23:24]) + "|"              //UI-H-ATM-BATCH      PICTURE 9.
			lineResult += utils.Hex2string(strBlock[24:27]) + "|"              //UI-H-ATM-BATCH      PICTURE 9.//UI-H-OPERATOR       PICTURE XXX.
			lineResult += utils.Hex2string_comp3(strBlock[27:31])[0:7] + "|"   //UI-H-ATM-BATCH      PICTURE 9.//UI-H-VIC-PROC-DTE PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[31:35])[0:7] + "|"   //UI-H-MC-PROC-DTE PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string(strBlock[35:50]) + "|"              //UI-H-CARD-ACCEPTOR-ID PIC X(15).
			lineResult += utils.Hex2string(strBlock[50:58]) + "|"              //UI-H-TERM-ID        PIC X(8).
			lineResult += utils.Hex2string_comp3(strBlock[58:64])[0:11] + "|"  //UI-H-USER-REF-NMBR PIC S9(11)      COMP-3.
			lineResult += utils.Hex2string(strBlock[64:67]) + "|"              //UI-H-SOURCE-ID      PIC X(3).
			lineResult += utils.Hex2string(strBlock[67:68]) + "|"              //UI-H-HDR-ARITH-ERR-Y-OR-N PIC X.
			lineResult += utils.Hex2string(strBlock[68:70]) + "|"              //UI-H-HDR-ADJ-TRAN-CODE PIC 99.
			lineResult += utils.Hex2string_comp3(strBlock[70:76])[0:11] + "|"  //UI-H-HDR-ADJ-AMT        PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[76:78])[0:4] + "|"   //UI-H-HDR-ADJ-REASON-CODE PIC S9(3) COMP-3.
			lineResult += utils.Hex2string(strBlock[78:113]) + "|"             //UI-H-HDR-ADJ-REASON-DESCR PIC X(35).
			lineResult += utils.Hex2string_comp3(strBlock[113:118])[1:9] + "|" //UI-H-MICREF-START   PIC S9(8)               COMP-3.
			lineResult += utils.Hex2string(strBlock[118:119]) + "|"            //UI-H-SETTLE-IND     PIC 9.

		case "2":
			lineResult += utils.Hex2string(strBlock[0:1]) + "|"            //UI-T-REC-TYPE       PICTURE X.
			lineResult += utils.Hex2string(strBlock[1:2]) + "|"            //UI-T-STATUS         PICTURE 9.
			lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" //UI-T-BATCH-NBR              PIC S9(5) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" //UI-T-SEQ-NBR                PIC S999  COMP-3
			lineResult += utils.Hex2string(strBlock[7:8]) + "|"            //UI-T-MONETARY-RECORD-TYPE PICTURE 9
			switch utils.Hex2string(strBlock[7:8]) {
			case "0":
				//Normal Monetary Record
				lineResult += utils.Hex2string_comp3(strBlock[8:17])[1:17] + "|"       //UI-T-CARDHLD-NBR        PIC S9(16)  COMP-3
				lineResult += utils.Hex2string(strBlock[17:19]) + "|"                  //UI-T-CODE       PICTURE 99.
				lineResult += utils.ParseComp3SignedMode(strBlock[19:24], "c") + "|"   //UI-T-AMOUNT             PIC S9(7)V99 COMP-3
				lineResult += utils.Hex2string_comp3(strBlock[24:28])[1:7] + "|"       //UI-T-DATE               PIC S9(6) COMP-3
				lineResult += utils.Hex2string(strBlock[28:34]) + "|"                  //UI-T-AUTH       PICTURE X(6)
				lineResult += utils.Hex2string(strBlock[34:57]) + "|"                  //UI-T-REFERENCE  PICTURE X(23) <- Bisa 15 bisa 11 lengthnya
				lineResult += utils.Hex2string_comp3(strBlock[57:62])[0:9] + "|"       //UI-T-CASH-BACK	 PICTURE S9(7)V99    COMPUTATIONAL-3.
				lineResult += utils.Hex2string(strBlock[62:65]) + "|"                  //UI-T-SOURCE-ID   PIC X(3).
				lineResult += utils.Hex2string(strBlock[65:105]) + "|"                 //UI-T-DESC       PICTURE X(40).
				lineResult += utils.Hex2string(strBlock[105:106]) + "|"                //UI-T-VISA-MAIL-PHONE-IND PIC X
				lineResult += utils.Hex2string(strBlock[106:119]) + "|"                //UI-T-TICKET-NUMBER PIC X(13)
				lineResult += utils.Hex2string(strBlock[119:120]) + "|"                //UI-T-POS-CAPABILITY	PIC X
				lineResult += utils.Hex2string(strBlock[120:122]) + "|"                //UI-T-POS-ENTRY-MODE	PIC X(2)
				lineResult += utils.Hex2string(strBlock[122:123]) + "|"                //UI-T-CRDHLD-ID-METH  PIC X
				lineResult += utils.Hex2string(strBlock[123:124]) + "|"                //UI-T-AUTH-SRCE  PIC X
				lineResult += utils.Hex2string(strBlock[124:125]) + "|"                //UI-T-CAT-IND    PIC X.
				lineResult += utils.Hex2string(strBlock[125:126]) + "|"                //UI-T-PREPAID-IND PIC X.
				lineResult += utils.Hex2string(strBlock[126:127]) + "|"                //UI-T-ATM-ACCT-SEL PIC X.
				lineResult += utils.Hex2string_comp3(strBlock[127:132])[0:9] + "|"     //UI-T-MICREF-NMBR    PIC S9(8)             COMP-3.
				lineResult += utils.Hex2string(strBlock[132:135]) + "|"                //UI-T-PAN-EXTN       PIC X(3).
				lineResult += utils.Hex2string_comp3(strBlock[135:138])[0:5] + "|"     //UI-T-MERCH-CATEGORY     PIC S9(05)  COMP-3.
				lineResult += utils.Hex2string(strBlock[138:163]) + "|"                //UI-T-PURCH-IDENT              PIC X(25).
				lineResult += utils.Hex2string(strBlock[163:164]) + "|"                //UI-T-MC-SEC-PROTOCOL    PIC  X.
				lineResult += utils.Hex2string(strBlock[164:165]) + "|"                //UI-T-MC-CH-AUTHENTIC    PIC  X.
				lineResult += utils.Hex2string(strBlock[165:169]) + "|"                //UI-T-NCCC-PROC-DTE            PIC 9(4).
				lineResult += utils.Hex2string(strBlock[169:184]) + "|"                //UI-T-AMEX-TRAN-ID           PIC X(15).
				lineResult += utils.Hex2string(strBlock[184:185]) + "|"                //UI-T-MSG-REASON-CODE        PIC  9(01).
				lineResult += utils.Hex2string(strBlock[185:186]) + "|"                //UI-T-FUND-TRANS-IND         PIC  X.
				lineResult += utils.Hex2string(strBlock[186:187]) + "|"                //UI-T-INSTALLMENT-IND        PIC X.
				lineResult += utils.ParseComp3SignedMode(strBlock[187:192], "c") + "|" //UI-T-PRINCIPAL-AMT          PIC S9(7)V99  COMP-3.
				lineResult += utils.Hex2string(strBlock[192:193]) + "|"                //UI-T-MC-UCAF-IND            PIC  X.
				lineResult += utils.Hex2string_comp3(strBlock[193:195])[0:3] + "|"     //FILLER                      PIC  S9(03) COMP-3.
				lineResult += utils.Hex2string(strBlock[195:196]) + "|"                //UI-T-PURCH-IDENT-FMT        PIC  X(01).
				lineResult += utils.Hex2string_comp3(strBlock[196:198])[0:3] + "|"     //UI-T-CARD-SEQ-NBR           PIC  S9(03) COMP-3.
				lineResult += utils.Hex2string_comp3(strBlock[198:200])[0:3] + "|"     //UI-T-EXT-SVC-CDE            PIC  S9(03) COMP-3.
				lineResult += utils.Hex2string(strBlock[200:201]) + "|"                //UI-T-TXN-FEE-CD          PIC  X(01)
				lineResult += utils.Hex2string_comp3(strBlock[201:206])[0:9] + "|"     //UI-T-TXN-FEE             PIC  9(7)V99 COMP-3
				if utils.Hex2string_comp3(strBlock[393:400])[0:12] != "404040404040" { //UI-T-B054-ADDT-AMT       PIC  9(10)V99 COMP-3
					lineResult += utils.Hex2string_comp3(strBlock[393:400])[1:12]
				} else {
					lineResult += "000000000000" + "|"
				}

			case "1":
				//Airline Information Record
				lineResult += "disini 1"
				lineResult += utils.Hex2string(strBlock[8:28]) + "|"             // UI-AI-PASSENGER-NAME PIC X(20)
				lineResult += utils.Hex2string_comp3(strBlock[28:32])[1:7] + "|" // UI-AI-DEPART-DATE PIC S9(6) COMP-3
				lineResult += utils.Hex2string(strBlock[32:35]) + "|"            // UI-AI-ORIG-CITY-AIRPORT-CD PIC X(3)
				lineResult += utils.Hex2string(strBlock[35:37]) + "|"            // UI-AI-CARRIER-CODE1 PIC X(2)
				lineResult += utils.Hex2string(strBlock[37:38]) + "|"            // UI-AI-SERVICE-CLASS1 PIC X
				lineResult += utils.Hex2string(strBlock[38:39]) + "|"            // UI-AI-STOP-OVER-CODE1 PIC X
				lineResult += utils.Hex2string(strBlock[39:42]) + "|"            // UI-AI-DEST-CITY-AIRPORT-CD1 PIC X(3)
				lineResult += utils.Hex2string(strBlock[42:44]) + "|"            // UI-AI-CARRIER-CODE2 PIC X(2)
				lineResult += utils.Hex2string(strBlock[44:45]) + "|"            // UI-AI-SERVICE-CLASS2 PIC X
				lineResult += utils.Hex2string(strBlock[45:46]) + "|"            // UI-AI-STOP-OVER-CODE2 PIC X
				lineResult += utils.Hex2string(strBlock[46:49]) + "|"            // UI-AI-DEST-CITY-AIRPORT-CD2 PIC X(3)
				lineResult += utils.Hex2string(strBlock[49:51]) + "|"            // UI-AI-CARRIER-CODE3 PIC X(2)
				lineResult += utils.Hex2string(strBlock[51:52]) + "|"            // UI-AI-SERVICE-CLASS3 PIC X
				lineResult += utils.Hex2string(strBlock[52:53]) + "|"            // UI-AI-STOP-OVER-CODE3 PIC X
				lineResult += utils.Hex2string(strBlock[53:56]) + "|"            // UI-AI-DEST-CITY-AIRPORT-CD3 PIC X(3)
				lineResult += utils.Hex2string(strBlock[56:58]) + "|"            // UI-AI-CARRIER-CODE4 PIC X(2)
				lineResult += utils.Hex2string(strBlock[58:59]) + "|"            // UI-AI-SERVICE-CLASS4 PIC X
				lineResult += utils.Hex2string(strBlock[59:60]) + "|"            // UI-AI-STOP-OVER-CODE4 PIC X
				lineResult += utils.Hex2string(strBlock[60:63]) + "|"            // UI-AI-DEST-CITY-AIRPORT-CD4 PIC X(3)
				lineResult += utils.Hex2string(strBlock[63:71]) + "|"            // UI-AI-TRAVEL-AGENCY-CODE PIC X(8)
				lineResult += utils.Hex2string(strBlock[71:96]) + "|"            // UI-AI-TRAVEL-AGENCY-NAME PIC X(25)

			case "2":
				//Interchange Data Record
				lineResult += utils.Hex2string_comp3(strBlock[8:15])[0:13] + "|" //UI-IC-AUTH-AMOUNT  PIC S9(11)V99 COMP-3
				lineResult += utils.Hex2string(strBlock[15:18]) + "|"            //UI-IC-AUTH-CURR-CODE PIC X(03)
				lineResult += utils.Hex2string(strBlock[18:22]) + "|"            //UI-IC-AUTH-DATE PIC 9(04).
				lineResult += utils.Hex2string(strBlock[32:195]) + "|"           //UI-IC-SPECIFIC-DATA X(163)
			case "3": //Chip data record 1
			case "4": //Chip data record 2
			case "5": //IXS Extention data
				//IXS Extention data
				lineResult += utils.Hex2string(strBlock[8:14]) + "|"  //UI-E-TIME           PIC X(6).
				lineResult += utils.Hex2string(strBlock[14:26]) + "|" //UI-E-POS            PIC X(12).
				lineResult += utils.Hex2string(strBlock[26:30]) + "|" //UI-E-DTE-EXPIRY     PIC X(4).
				lineResult += utils.Hex2string(strBlock[30:33]) + "|" //UI-E-CARD-SEQ       PIC X(3).
				lineResult += utils.Hex2string(strBlock[33:45]) + "|" //UI-E-RETRVL-REF-NBR PIC X(12).
				lineResult += utils.Hex2string(strBlock[45:51]) + "|" //UI-E-STAN           PIC X(6).
				lineResult += utils.Hex2string(strBlock[51:52]) + "|" //UI-E-CARD-TYPE      PIC X.
				lineResult += utils.Hex2string(strBlock[52:67]) + "|" //UI-E-CPS-TXN-ID     PIC 9(15).
				lineResult += utils.Hex2string(strBlock[67:68]) + "|" //UI-E-CPS-ACI        PIC X.
				lineResult += utils.Hex2string(strBlock[68:72]) + "|" //UI-E-CPS-VAL-CODE   PIC X(4).
			case "9":
				//ITEM ADJUSTMENT RECORD ONLY FOR MERCH DEPOSIT BATCH
				lineResult += utils.Hex2string(strBlock[8:11]) + "|"  //UI-ADJ-ITEM-REASON-CODE PIC 999.
				lineResult += utils.Hex2string(strBlock[11:46]) + "|" //UI-ADJ-ITEM-REASON-DESCR PIC X(35).
			}
		case "3":
			//3. FILE MAINTENANCE
			lineResult += utils.Hex2string(strBlock[0:1]) + "|"    //UI-M-REC-TYPE       PICTURE X.
			lineResult += utils.Hex2string(strBlock[1:4]) + "|"    //UI-M-ORG        PICTURE 999.
			lineResult += utils.Hex2string(strBlock[4:7]) + "|"    //UI-M-TYPE       PICTURE 999.
			lineResult += utils.Hex2string(strBlock[7:23]) + "|"   //UI-M-ACCOUNT    PICTURE 9(16).
			lineResult += utils.Hex2string(strBlock[23:24]) + "|"  //UI-M-FILE       PICTURE X.
			lineResult += utils.Hex2string(strBlock[24:31]) + "|"  //UI-M-JULIAN     PICTURE 9(7).
			lineResult += utils.Hex2string(strBlock[31:37]) + "|"  //UI-M-TIME       PICTURE 9(6).
			lineResult += utils.Hex2string(strBlock[37:40]) + "|"  //UI-M-TRANS      PIC 999.
			lineResult += utils.Hex2string(strBlock[40:44]) + "|"  //UI-M-TERMINAL-ID PIC X(4).
			lineResult += utils.Hex2string(strBlock[44:64]) + "|"  //UI-M-SIGNON-NAME PIC X(20).
			lineResult += utils.Hex2string(strBlock[64:65]) + "|"  //UI-M-SOURCE-CODE PIC X(1).
			lineResult += utils.Hex2string(strBlock[65:120]) + "|" //UI-M-DATA  PICTURE X(55). *  DATA NEEDS TO BE IN CPS121 FORMAT

		case "4":
			//4. MERCHANT AUTHORIZATION ANALYSIS
			lineResult += utils.Hex2string(strBlock[0:1]) + "|"              //UI-A-REC-TYPE       PICTURE X.
			lineResult += utils.Hex2string_comp3(strBlock[1:8])[1:13] + "|"  //UI-A-MERCH-NBR      PIC S9(12) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[8:11])[0:5] + "|"  //UI-A-NBR-VOICE-AUTH PIC S9(5) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[11:14])[0:5] + "|" //UI-A-NBR-POS1-AUTH  PIC S9(5) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[14:17])[0:5] + "|" //UI-A-NBR-POS2-AUTH  PIC S9(5) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[17:20])[0:5] + "|" //UI-A-NBR-POS3-AUTH  PIC S9(5) COMP-3.

		case "A":
			// A. MERCH ADJ BATCH HEADER
			lineResult += utils.Hex2string(strBlock[0:1]) + "|"                // UI-MH-REC-TYPE        PIC X
			lineResult += utils.Hex2string(strBlock[1:2]) + "|"                // UI-MH-STATUS          PIC 9
			lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|"     // UI-MH-BATCH-NBR       PIC S9(5) COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|"     // UI-MH-SEQ-NBR         PIC S999 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[7:9])[0:3] + "|"     // UI-MH-NBR-ITEMS       PIC S999 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[9:15])[0:11] + "|"   // UI-MH-AMOUNT          PIC S9(9)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[15:22])[1:13] + "|"  // UI-MH-MERCH-NBR       PIC S9(12) COMP-3
			lineResult += utils.Hex2string(strBlock[22:23]) + "|"              // UI-MH-BATCH-REV       PIC X
			lineResult += utils.Hex2string(strBlock[23:24]) + "|"              // UI-MH-ATM-BATCH       PIC 9
			lineResult += utils.Hex2string(strBlock[26:29]) + "|"              // UI-MH-OPERATOR        PIC XXX
			lineResult += utils.Hex2string(strBlock[29:33]) + "|"              // UI-MH-VIC-PROC-DTE    PIC X(4)
			lineResult += utils.Hex2string_comp3(strBlock[33:37])[0:7] + "|"   // UI-MH-MC-PROC-DTE     PIC S9(7) COMP-3
			lineResult += utils.Hex2string(strBlock[37:52]) + "|"              // UI-MH-CARD-ACCEPTOR-ID PIC X(15)
			lineResult += utils.Hex2string(strBlock[52:60]) + "|"              // UI-MH-TERM-ID         PIC X(8)
			lineResult += utils.Hex2string_comp3(strBlock[60:66])[0:11] + "|"  // UI-MH-USER-REF-NMBR   PIC S9(11) COMP-3
			lineResult += utils.Hex2string(strBlock[66:69]) + "|"              // UI-MH-SOURCE-ID       PIC X(3)
			lineResult += utils.Hex2string(strBlock[69:70]) + "|"              // UI-MH-HDR-ARITH-ERR-Y-OR-N PIC X
			lineResult += utils.Hex2string(strBlock[70:72]) + "|"              // UI-MH-HDR-ADJ-TRAN-CODE    PIC 99
			lineResult += utils.Hex2string_comp3(strBlock[72:78])[0:11] + "|"  // UI-MH-HDR-ADJ-AMT     PIC S9(9)V99 COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[78:81])[0:3] + "|"   // UI-MH-HDR-ADJ-REASON-CODE PIC S9(3) COMP-3
			lineResult += utils.Hex2string(strBlock[81:116]) + "|"             // UI-MH-HDR-ADJ-REASON-DESCR PIC X(35)
			lineResult += utils.Hex2string_comp3(strBlock[116:121])[1:9] + "|" // UI-MH-MICREF-START    PIC S9(8) COMP-3

		case "B":
			// B. MERCH ADJ TRANSACTION
			lineResult += utils.Hex2string(strBlock[0:1]) + "|"              // UI-MT-REC-TYPE         PICTURE X.
			lineResult += utils.Hex2string(strBlock[1:2]) + "|"              // UI-MT-STATUS           PICTURE 9.
			lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|"   // UI-MT-BATCH-NBR        PIC S9(5) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|"   // UI-MT-SEQ-NBR          PIC S999 COMP-3.
			lineResult += utils.Hex2string(strBlock[7:8]) + "|"              // UI-MT-MONETARY-RECORD-TYPE PIC 9.
			lineResult += utils.Hex2string_comp3(strBlock[8:13])[0:9] + "|"  // UI-MT-MERCH-NBR        PIC S9(9) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[13:15])[0:3] + "|" // UI-MT-NBR-ITEMS        PIC S999 COMP-3.
			lineResult += utils.Hex2string(strBlock[15:19]) + "|"            // FILLER                 PIC X(4).
			lineResult += utils.Hex2string_comp3(strBlock[19:24])[0:9] + "|" // UI-MT-AMOUNT           PIC S9(7)V99 COMP-3.
			lineResult += utils.Hex2string(strBlock[24:25]) + "|"            // UI-MT-CR-DR-IND        PICTURE X.
			lineResult += utils.Hex2string(strBlock[25:26]) + "|"            // UI-MT-SL-RT-IND        PICTURE X.
			lineResult += utils.Hex2string(strBlock[26:27]) + "|"            // UI-MT-ONUS-IND         PICTURE X.
			lineResult += utils.Hex2string(strBlock[27:28]) + "|"            // UI-MT-QUAL-IND         PICTURE X.
			lineResult += utils.Hex2string_comp3(strBlock[28:30])[0:3] + "|" // UI-MT-REASON-CODE      PIC S9(3) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[30:33])[0:5] + "|" // UI-MT-ORIG-BATCH-NBR   PIC S9(5) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[33:37])[0:7] + "|" // UI-MT-ORIG-BATCH-DATE  PIC S9(7) COMP-3.
			lineResult += utils.Hex2string(strBlock[37:39]) + "|"            // UI-MT-CARD-TYPE        PICTURE 99.
			lineResult += utils.Hex2string(strBlock[39:74]) + "|"            // UI-MT-DESC             PICTURE X(35).
			lineResult += utils.Hex2string(strBlock[74:200]) + "|"           // FILLER                 PIC X(126).

		}
		lineResult += "\r\n"
		return lineResult
	})

}
