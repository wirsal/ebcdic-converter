package cia

import (
	"strconv"
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

var cardType int

func Cps123input2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPS123.length")
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

		decoded := decodeCPS123(record)
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

func decodeCPS123(strBlock string) string {
	//	return utils.SafeDecode("decodeCPS110", func() string {
	var lineResult string
	recType := utils.Hex2string(strBlock[0:1])
	switch recType {
	case "1":
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            //TF1-H-REC-TYPE  PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            //TF1-H-STAT      PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" //TF1-H-BATCH-NBR	PIC S9(5)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" //TF1-H-SEQ-NBR		PIC S999        COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            //TF1-H-REC-NBR		PIC 9
		switch utils.Hex2string(strBlock[7:8]) {
		case "0":
			lineResult += utils.Hex2string_comp3(strBlock[8:10])[0:3] + "|"                                          //TF1-H-NBR-ITEMS PIC S999        COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[10:16])[0:11] + "|"                                        //TF1-H-AMOUNT    PICTURE S9(9)V99    COMPUTATIONAL-3
			lineResult += utils.Hex2string_comp3(strBlock[16:23])[1:13] + "|"                                        //TF1-H-MERCH-NBR PIC S9(12)      COMP-3
			lineResult += utils.Hex2string(strBlock[23:24]) + "|"                                                    //TF1-H-BATCH-REV PIC X
			lineResult += utils.Hex2string(strBlock[24:27]) + "|"                                                    //TF1-H-OPERATOR  PIC XXX
			lineResult += defaultIfMatch(utils.Hex2string_comp3(strBlock[27:31])[0:7], "4040404", "0000000") + "|"   //TF1-H-VIC-PROC-DTE	PIC S9(7)       COMP-3
			lineResult += defaultIfMatch(utils.Hex2string_comp3(strBlock[31:35])[0:7], "4040404", "0000000") + "|"   //TF1-H-MC-PROC-DTE		PIC S9(7)       COMP-3
			lineResult += utils.Hex2string(strBlock[35:38]) + "|"                                                    //TF1-H-SOURCE-ID PIC X(3)
			lineResult += utils.Hex2string(strBlock[39:40]) + "|"                                                    //TF1-H-TMP-POS-CAPABILITY	PIC X
			lineResult += utils.Hex2string(strBlock[40:42]) + "|"                                                    //TF1-H-TMP-POS-ENTRY-MODE	PIC XX
			lineResult += utils.Hex2string(strBlock[42:43]) + "|"                                                    //TF1-H-TMP-AUTH-SRCE		PIC X
			lineResult += utils.Hex2string(strBlock[43:44]) + "|"                                                    //TF1-H-TMP-CRDHLD-ID-METH	PIC X
			lineResult += defaultIfMatch(utils.Hex2string_comp3(strBlock[44:49])[1:9], "04040404", "00000000") + "|" //TF1-H-MICREF-START	 PIC S9(8) COMP-3
			lineResult += utils.Hex2string(strBlock[49:50]) + "|"                                                    //TF1-H-MICREF-GEN	PIC X
			lineResult += utils.Hex2string(strBlock[50:51]) + "|"                                                    //TF1-H-SETTLE-IND	PIC 9
			lineResult += utils.Hex2string(strBlock[51:52]) + "|"                                                    //TF1-H-USAGE-IND PIC X
			lineResult += utils.Hex2string(strBlock[52:55]) + "|"                                                    //TF1-H-SEC-OPERATOR	PIC X(3)

		case "1":
		}
	case "2":
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            //TF1-T-REC-TYPE  PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            //TF1-T-STAT      PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" //TF1-T-BATCH-NBR	PIC S9(5)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" //TF1-T-SEQ-NBR		PIC S999        COMP-3
		tf1TRec := utils.Hex2string(strBlock[7:8])                     //TF1-T-REC-NBR		PIC 9
		lineResult += tf1TRec + "|"

		switch tf1TRec {
		case "0": //Normal
			lineResult += utils.Hex2string_comp3(strBlock[8:17])[1:17] + "|" //TF1-T-CARD-NBR  PIC S9(16)      COMP-3
			cardType, _ = strconv.Atoi(utils.Hex2string_comp3(strBlock[8:17])[1:2])
			lineResult += utils.Hex2string(strBlock[17:19]) + "|"                                                      //TF1-T-CODE      PIC 99
			lineResult += utils.Hex2string_comp3(strBlock[19:24])[0:9] + "|"                                           //TF1-T-AMOUNT    PICTURE S9(7)V99    COMPUTATIONAL-3
			lineResult += utils.Hex2string_comp3(strBlock[24:28])[1:7] + "|"                                           //TF1-T-DATE      PIC S9(6)       COMP-3
			lineResult += utils.Hex2string(strBlock[28:34]) + "|"                                                      //TF1-T-AUTH      PIC  X(06)
			lineResult += utils.Hex2string(strBlock[34:37]) + "|"                                                      //TF1-T-SOURCE-ID PIC X(3)
			lineResult += utils.Hex2string(strBlock[37:38]) + "|"                                                      //TF1-T-TMP-POS-CAPABILITY	PIC X
			lineResult += utils.Hex2string(strBlock[38:40]) + "|"                                                      //TF1-T-TMP-POS-ENTRY-MODE	PIC XX
			lineResult += utils.Hex2string(strBlock[40:41]) + "|"                                                      //TF1-T-TMP-AUTH-SRCE		PIC X
			lineResult += utils.Hex2string(strBlock[41:42]) + "|"                                                      //TF1-T-TMP-CRDHLD-ID-METH	PIC X
			lineResult += utils.Hex2string(strBlock[42:43]) + "|"                                                      //TF1-T-VISA-MAIL-PHONE-IND	PIC X
			lineResult += defaultIfMatch(utils.Hex2string_comp3(strBlock[43:48])[1:9], "04040404", "00000000") + "|"   // TF1-T-MICREF-SEQNO  PIC S9(8)       COMP-3
			lineResult += utils.Hex2string(strBlock[48:49]) + "|"                                                      //TF1-T-MSG-REASON-CODE         PIC 9(01)
			lineResult += defaultIfMatch(utils.Hex2string_comp3(strBlock[49:52])[0:5], "40404", "00000") + "|"         //TF1-T-MERCH-CATEGORY          PIC S9(05)  COMP-3
			lineResult += utils.Hex2string(strBlock[52:55]) + "|"                                                      //TF1-T-SEC-OPERATOR			PIC X(3)
			lineResult += utils.Hex2string(strBlock[55:56]) + "|"                                                      //TF1-T-TXN-FEE-CD          PIC X(01)
			lineResult += defaultIfMatch(utils.Hex2string_comp3(strBlock[56:61])[0:9], "404040404", "000000000") + "|" //TF1-T-TXN-FEE             PIC 9(7)V99 COMP-3
		case "7":
			switch cardType {
			case 4:
				lineResult += utils.Hex2string(strBlock[8:9]) + "|"               //TF1-X1-ACI             PIC X
				lineResult += utils.Hex2string(strBlock[9:11]) + "|"              //TF1-X1-POS-MODE        PIC X(02)
				lineResult += utils.Hex2string(strBlock[11:15]) + "|"             //TF1-X1-VALIDATION-CODE PIC X(04)
				lineResult += utils.Hex2string_comp3(strBlock[15:23])[0:15] + "|" //TF1-X1-TRAN-ID         PIC S9(15) COMP-3
				lineResult += utils.Hex2string_comp3(strBlock[23:30])[1:13] + "|" //TF1-X1-AUTH-AMT        PIC S9(12) COMP-3
				lineResult += utils.Hex2string(strBlock[30:32]) + "|"             //TF1-X1-AUTH-RESP       PIC XX
				lineResult += utils.Hex2string(strBlock[32:40]) + "|"             //TF1-X1-TERM-ID         PIC X(08)
				lineResult += utils.Hex2string(strBlock[40:42]) + "|"             //TF1-X1-CARD-LEVEL      PIC X(02)
			case 5, 2:
				//MC
				lineResult += utils.Hex2string(strBlock[8:17]) + "|"             //TF1-MC-BANKNET-REF     PIC X(9)
				lineResult += utils.Hex2string(strBlock[17:19]) + "|"            //TF1-MC-BANKNET-MM  PIC X(2)
				lineResult += utils.Hex2string(strBlock[19:21]) + "|"            //TF1-MC-BANKNET-DD  PIC X(2)
				lineResult += utils.Hex2string(strBlock[21:23]) + "|"            //TF1-MC-EC-MODE         PIC X(2)
				lineResult += utils.Hex2string_comp3(strBlock[23:25])[0:3] + "|" //TF1-MC-EXT-SVC-CDE     PIC S9(03) COMP-3
				lineResult += utils.Hex2string(strBlock[25:27]) + "|"            //TF1-MC-IRD             PIC X(02)
				lineResult += utils.Hex2string(strBlock[27:39]) + "|"            //TF1-MC-IPM-POS-DATA-CODE	PIC X(12)
				lineResult += utils.Hex2string(strBlock[39:42]) + "|"            //TF1-MC-WALLET-ID       PIC X(03)

			case 3:
				lineResult += utils.Hex2string(strBlock[8:20]) + "|" //TF1-JCB-POS-DATA-CODE
			}
		case "8":
			lineResult += utils.Hex2string(strBlock[8:33]) + "|"  //TF1-V-PURCH-IDENT      PIC X(25)
			lineResult += utils.Hex2string(strBlock[33:34]) + "|" //TF1-V-FUND-TRANS-IND          PIC X
			lineResult += utils.Hex2string(strBlock[34:35]) + "|" //TF1-V-PURCH-IDENT-FMT  PIC X(01)
		}
	case "3":
		//RECORD TYPE 3 = TF1-D
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            //TF1-D-REC-TYPE  PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            //TF1-D-STAT      PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" //TF1-D-BATCH-NBR	PIC S9(5)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" //TF1-D-SEQ-NBR		PIC S999        COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            //TF1-D-REC-NBR		PIC 9
		lineResult += utils.Hex2string(strBlock[8:48]) + "|"           //TF1-D-DESCRIPTION	PIC X(40)
		lineResult += utils.Hex2string(strBlock[48:49]) + "|"          //TF1-D-PYMT-AUTH PIC X

	case "4":
		//RECORD TYPE 4 = TF1-R
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            //TF1-R-REC-TYPE  PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            //TF1-R-STAT      PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" //TF1-R-BATCH-NBR 	PIC S9(5)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" //TF1-R-SEQ-NBR		PIC S999        COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            //TF1-R-REC-NBR		PIC 9
		lineResult += utils.Hex2string(strBlock[8:31]) + "|"           //TF1-R-REFERENCE-NBR	PIC X(23)

	case "5":
		//RECORD TYPE 5 = TF1-A1     TIIF AIRLINE
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            //TF1-A1-REC-TYPE PIC  X(01)
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            //TF1-A1-STAT     PIC  X(01)
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" //TF1-A1-BATCH-NBR 	PIC S9(5)       COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" //TF1-A1-SEQ-NBR	PIC S9(3)       COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            //TF1-A1-REC-NBR	PIC 9
		lineResult += utils.Hex2string(strBlock[8:28]) + "|"           //TF1-A1-PASSENGER-NAME	PIC  X(20)
		lineResult += utils.Hex2string(strBlock[28:34]) + "|"          //TF1-A1-DEPART-DATE	PIC  9(06)
		lineResult += utils.Hex2string(strBlock[34:37]) + "|"          //TF1-A1-AIRPORT-CD		PIC  X(03)
		lineResult += utils.Hex2string(strBlock[37:50]) + "|"          //TF1-A1-TICKET-NBR		PIC  X(13)
		lineResult += utils.Hex2string(strBlock[50:51]) + "|"          //TF1-A1-RESTRICTED-TICKET-IND	PIC X

	case "6":
		// RECORD TYPE 6 = TF1-A2 TIIF AIRLINE
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            // TF1-A2-REC-TYPE PIC X(01)
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            // TF1-A2-STAT PIC X(01)
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" // TF1-A2-BATCH-NBR PIC S9(5) COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" // TF1-A2-SEQ-NBR PIC S9(3) COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            // TF1-A2-REC-NBR PIC 9(01)
		lineResult += utils.Hex2string(strBlock[8:10]) + "|"           // CARRIER1 PIC X(02)
		lineResult += utils.Hex2string(strBlock[10:11]) + "|"          // SERVICE1 PIC X(01)
		lineResult += utils.Hex2string(strBlock[11:12]) + "|"          // STOP-OV1 PIC X(01)
		lineResult += utils.Hex2string(strBlock[12:15]) + "|"          // DEST-CD1 PIC X(03)
		lineResult += utils.Hex2string(strBlock[15:17]) + "|"          // CARRIER2 PIC X(02)
		lineResult += utils.Hex2string(strBlock[17:18]) + "|"          // SERVICE2 PIC X(01)
		lineResult += utils.Hex2string(strBlock[18:19]) + "|"          // STOP-OV2 PIC X(01)
		lineResult += utils.Hex2string(strBlock[19:22]) + "|"          // DEST-CD2 PIC X(03)
		lineResult += utils.Hex2string(strBlock[22:24]) + "|"          // CARRIER3 PIC X(02)
		lineResult += utils.Hex2string(strBlock[24:25]) + "|"          // SERVICE3 PIC X(01)
		lineResult += utils.Hex2string(strBlock[25:26]) + "|"          // STOP-OV3 PIC X(01)
		lineResult += utils.Hex2string(strBlock[26:29]) + "|"          // DEST-CD3 PIC X(03)
		lineResult += utils.Hex2string(strBlock[29:31]) + "|"          // CARRIER4 PIC X(02)
		lineResult += utils.Hex2string(strBlock[31:32]) + "|"          // SERVICE4 PIC X(01)
		lineResult += utils.Hex2string(strBlock[32:33]) + "|"          // STOP-OV4 PIC X(01)
		lineResult += utils.Hex2string(strBlock[33:36]) + "|"          // DEST-CD4 PIC X(03)

	case "7":
		// RECORD TYPE 7 = TF1-A3     TIIF AIRLINE
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            // TF1-A3-REC-TYPE               PIC X(01)
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            // TF1-A3-STAT                   PIC X(01)
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" // TF1-A3-BATCH-NBR              PIC S9(5)   COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" // TF1-A3-SEQ-NBR                PIC S9(3)   COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            // TF1-A3-REC-NBR                PIC 9(01)
		lineResult += utils.Hex2string(strBlock[8:16]) + "|"           // TF1-A3-TRAVEL-AGENCY-CD       PIC X(08)
		lineResult += utils.Hex2string(strBlock[16:41]) + "|"          // TF1-A3-TRAVEL-AGENCY-NAME     PIC X(25)
		lineResult += utils.Hex2string(strBlock[41:42]) + "|"          // TF1-A3-RESTRICTED-TICKET-IND  PIC X(01)

	case "A":
		// RECORD TYPE A = TF1-MH MERCH ADJ BATCH HEADER
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"               // TF1-MH-REC-TYPE                PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"               // TF1-MH-STAT                    PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|"    // TF1-MH-BATCH-NBR               PIC S9(5)   COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|"    // TF1-MH-SEQ-NBR                 PIC S999    COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"               // TF1-MH-REC-NBR                 PIC 9
		lineResult += utils.Hex2string_comp3(strBlock[8:10])[0:3] + "|"   // TF1-MH-NBR-ITEMS               PIC S999    COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[10:16])[0:11] + "|" // TF1-MH-AMOUNT                  PIC S9(9)V99 COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[16:18])[0:3] + "|"  // TF1-MH-ORG-NMBR                PIC S999    COMP-3
		lineResult += utils.Hex2string(strBlock[18:19]) + "|"             // TF1-MH-MERCH-NBR               PIC 9(9)    COMP-3 (?)
		lineResult += utils.Hex2string(strBlock[19:20]) + "|"             // TF1-MH-BATCH-REV               PIC X
		lineResult += utils.Hex2string(strBlock[20:21]) + "|"             // TF1-MH-OPERATOR                PIC XXX
		lineResult += utils.Hex2string_comp3(strBlock[21:25])[0:7] + "|"  // TF1-MH-VIC-PROC-DTE            PIC S9(7)   COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[25:29])[0:7] + "|"  // TF1-MH-MC-PROC-DTE             PIC S9(7)   COMP-3
		lineResult += utils.Hex2string(strBlock[29:32]) + "|"             // TF1-MH-SOURCE-ID               PIC X(3)
		lineResult += utils.Hex2string(strBlock[33:34]) + "|"             // TF1-MH-TMP-POS-CAPABILITY      PIC X
		lineResult += utils.Hex2string(strBlock[34:36]) + "|"             // TF1-MH-TMP-POS-ENTRY-MODE      PIC XX
		lineResult += utils.Hex2string(strBlock[36:37]) + "|"             // TF1-MH-TMP-AUTH-SRCE           PIC X
		lineResult += utils.Hex2string(strBlock[37:38]) + "|"             // TF1-MH-TMP-CRDHLD-ID-METH      PIC X
		lineResult += utils.Hex2string_comp3(strBlock[38:43])[1:9] + "|"  // TF1-MH-MICREF-START            PIC S9(8)   COMP-3
		lineResult += utils.Hex2string(strBlock[43:44]) + "|"             // TF1-MH-MICREF-GEN              PIC X
		lineResult += utils.Hex2string(strBlock[46:49]) + "|"             // TF1-MH-SEC-OPERATOR            PIC X(3)

	case "B":
		// RECORD TYPE B = TF1-MT MERCH ADJ TRANSACTION
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"              // TF1-MT-REC-TYPE PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"              // TF1-MT-STAT PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|"   // TF1-MT-BATCH-NBR PIC S9(5) COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|"   // TF1-MT-SEQ-NBR PIC S999 COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"              // TF1-MT-REC-NBR PIC 9
		lineResult += utils.Hex2string_comp3(strBlock[8:13])[0:9] + "|"  // TF1-MT-MER-NBR PIC S9(9) COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[13:15])[0:3] + "|" // TF1-MT-NBR-ITEMS PIC S999 COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[15:20])[0:9] + "|" // TF1-MT-AMOUNT PIC S9(7)V99 COMP-3
		lineResult += utils.Hex2string(strBlock[20:21]) + "|"            // TF1-MT-CR-DR-IND PIC X
		lineResult += utils.Hex2string(strBlock[21:22]) + "|"            // TF1-MT-SL-RT-IND PIC X
		lineResult += utils.Hex2string(strBlock[22:23]) + "|"            // TF1-MT-ONUS-IND PIC X
		lineResult += utils.Hex2string(strBlock[23:24]) + "|"            // TF1-MT-QUAL-IND PIC X
		lineResult += utils.Hex2string_comp3(strBlock[24:26])[0:3] + "|" // TF1-MT-REASON-CODE PIC S999 COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[26:29])[0:5] + "|" // TF1-MT-ORIG-BATCH-NBR PIC S9(5) COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[29:33])[0:7] + "|" // TF1-MT-ORIG-BATCH-DATE PIC S9(7) COMP-3
		lineResult += utils.Hex2string(strBlock[33:35]) + "|"            // TF1-MT-CARD-TYPE PIC 99
		lineResult += utils.Hex2string(strBlock[52:55]) + "|"            // TF1-MT-SEC-OPERATOR PIC X(3)

	case "C":
		// RECORD TYPE C = TF1-MD MERCH ADJ DESCRIPTION
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            //TF1-MD-REC-TYPE  PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            //TF1-MD-STAT      PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" //TF1-MD-BATCH-NBR PIC S9(5)  COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" //TF1-MD-SEQ-NBR   PIC S999   COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            //TF1-MD-REC-NBR   PIC 9
		lineResult += utils.Hex2string(strBlock[8:43]) + "|"           //TF1-MD-DESCRIPTION PIC X(35)

	case "R":
		// RECORD TYPE R = TF1-CRT DOMESTIC COPY REQUEST
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"               //TF1-CRT-REC-TYPE        PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"               //TF1-CRT-STATUS          PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|"    //TF1-CRT-BATCH-NBR       PIC S9(5)  COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|"    //TF1-CRT-SEQ-NBR         PIC S999   COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"               //TF1-CRT-REC-NBR         PIC 9
		lineResult += utils.Hex2string_comp3(strBlock[8:14])[0:11] + "|"  //TF1-CRT-REF-NBR         PIC S9(11) COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[14:16])[0:3] + "|"  //TF1-CRT-ORGN            PIC S999   COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[16:18])[0:3] + "|"  //TF1-CRT-TYPE            PIC S999   COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[18:27])[1:17] + "|" //TF1-CRT-ACCT            PIC S9(16) COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[27:31])[0:7] + "|"  //TF1-CRT-EFF-DATE        PIC S9(7)  COMP-3
		lineResult += utils.Hex2string(strBlock[31:32]) + "|"             //TF1-CRT-DOC-TYPE        PIC 9
		lineResult += utils.Hex2string_comp3(strBlock[32:37])[0:9] + "|"  //TF1-CRT-AMNT            PIC S9(7)V99 COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[37:42])[0:9] + "|"  //TF1-CRT-ORIG-AMNT       PIC S9(7)V99 COMP-3
		lineResult += utils.Hex2string(strBlock[42:45]) + "|"             //TF1-CRT-ORIG-CURR-CODE  PIC XXX
		lineResult += utils.Hex2string(strBlock[45:48]) + "|"             //TF1-CRT-SEC-OPERATOR    PIC XXX

	case "S":
		// RECORD TYPE S = TF1-CRD DOMESTIC COPY REQ MERCH DESC
		lineResult += utils.Hex2string(strBlock[0:1]) + "|"            // TF1-CRD-REC-TYPE  PIC X
		lineResult += utils.Hex2string(strBlock[1:2]) + "|"            // TF1-CRD-STATUS    PIC X
		lineResult += utils.Hex2string_comp3(strBlock[2:5])[0:5] + "|" // TF1-CRD-BATCH-NBR PIC S9(5) COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[5:7])[0:3] + "|" // TF1-CRD-SEQ-NBR   PIC S999  COMP-3
		lineResult += utils.Hex2string(strBlock[7:8]) + "|"            // TF1-CRD-REC-NBR   PIC 9
		lineResult += utils.Hex2string(strBlock[8:48]) + "|"           // TF1-CRD-MERCH-DESC PIC X(40)

	}

	lineResult += "\r\n"
	return lineResult
	// })
}
func defaultIfMatch(val, match, def string) string {
	if val == match {
		return def
	}
	return val
}
