package cia

import (
	"time"
	"unicode/utf8"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cpcrd2text(inputFilename string) bool {
	start := time.Now()

	outputFilename := inputFilename // diasumsikan output sama dengan input?
	recordLength := viper.GetInt("file.CPCRD.length")
	batchSize := viper.GetInt("server.batchSize")

	content, err := utils.ReadAllFile(inputFilename)
	if err != nil {
		utils.Error("Failed to read file", err)
		return false
	}

	totalRecords := utf8.RuneCountInString(content) / recordLength
	// utils.Info("Total record CPCRD ", totalRecords)
	var (
		data    string
		output  string
		counter int
	)
	for line := 0; line <= totalRecords; line++ {

		startIdx := recordLength * line
		endIdx := recordLength * (line + 1)
		strBlock := content[startIdx:endIdx]

		output = decodeCPCRD(strBlock)
		if len(output) > 0 {
			data += output
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

// Decode and mapping CPS102 from EBCDIC to ASCII
func decodeCPCRD(strBlock string) string {
	return utils.SafeDecode("decodeCPCRD", func() string {
		var lineResult string
		if (utils.Hex2string_comp3(strBlock[4:13])[1:17] != "0000000000000000") && (utils.Hex2string_comp3(strBlock[4:13])[1:17] != "9999999999999999") {

			lineResult += utils.Hex2string_comp3(strBlock[0:2])[0:3] + "|"    //CM-ORG-NMBR     		PIC S999        COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[2:4])[0:3] + "|"    //CM-TYPE         		PIC S999        COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[4:13])[1:17] + "|"  //CM-CARD-NMBR    		PIC S9(16)      COMP-3
			lineResult += utils.Hex2string(strBlock[13:28]) + "|"             //CM-SHORT-NAME       	PIC X(15)
			lineResult += utils.Hex2string(strBlock[28:44]) + "|"             //CM-ALPHA-KEY        	PIC X(16)
			lineResult += utils.Hex2string_comp3(strBlock[44:46])[0:3] + "|"  //CM-CUSTOMER-ORG 	  	PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[46:55])[1:17] + "|" //CM-CUSTOMER-NMBR    	PIC S9(16)      COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[55:57])[0:3] + "|"  //CM-ALT-CUSTOMER-ORG	PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[57:66])[1:17] + "|" //CM-ALT-CUSTOMER-NMBR	PIC S9(16)      COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[66:68])[0:3] + "|"  //CM-DUP-STMT-ORG		PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[68:77])[1:17] + "|" //CM-DUP-STMT-NBR 		PIC S9(16)      COMP-3
			lineResult += utils.Hex2string(strBlock[77:79]) + "|"             //CM-CYCLE            	PIC 99
			//CM-XFR-ORG-NMBR 		PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[79:81])[0:3] + "|"
			//CM-XFR-TYPE-NMBR		PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[81:83])[0:3] + "|"
			//CM-XFR-ACCT-NMBR		PIC S9(16)      COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[83:92])[1:17] + "|"
			//CM-OFFICER          	PIC XXX
			lineResult += utils.Hex2string(strBlock[92:95]) + "|"
			//CM-CURR-COLLECTOR   	PIC XXX
			lineResult += utils.Hex2string(strBlock[95:98]) + "|"
			//CM-PERM-COLLECTOR   	PIC XXX
			lineResult += utils.Hex2string(strBlock[98:101]) + "|"
			//CM-AGENT-BANK-NMBR  	PIC S9(5)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[101:104])[0:5] + "|"
			//CM-CORP-ACCT-ORG		PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[104:106])[0:3] + "|"
			//CM-CORP-ACCT-NMBR		PIC S9(16)      COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[106:115])[1:17] + "|"
			//CM-DOMICILE-BRANCH  	PIC S9(5)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[115:118])[0:5] + "|"
			//CM-COLLATERAL-CODE  	PIC XX
			lineResult += utils.Hex2string(strBlock[118:120]) + "|"
			//CM-USER-CODE        	PIC XX
			lineResult += utils.Hex2string(strBlock[120:122]) + "|"
			//CM-USER-CODE-2      	PIC XX
			lineResult += utils.Hex2string(strBlock[122:124]) + "|"
			//CM-USER-CODE-3      	PIC XX
			lineResult += utils.Hex2string(strBlock[124:126]) + "|"
			//CM-INS-CODE         	PIC X
			lineResult += utils.Hex2string(strBlock[126:127]) + "|"
			//CM-OWNERSHIP-FLAG   	PIC X
			lineResult += utils.Hex2string(strBlock[127:128]) + "|"
			//CM-BLOCK-CODE       	PIC X
			lineResult += utils.Hex2string(strBlock[128:129]) + "|"
			//CM-ALT-BLOCK-CODE   	PIC X
			lineResult += utils.Hex2string(strBlock[129:130]) + "|"
			//CM-RPT-AUTH-FLAG    	PIC 9
			lineResult += utils.Hex2string(strBlock[130:131]) + "|"
			//CM-STATUS           	PIC X
			lineResult += utils.Hex2string(strBlock[131:132]) + "|"
			//CM-STATEMENT-FLAG   	PIC 9
			lineResult += utils.Hex2string(strBlock[132:133]) + "|"
			//CM-STMT-FREQ        	PIC 99
			lineResult += utils.Hex2string(strBlock[133:135]) + "|"
			//CM-WAIVE-LATE-CHG   	PIC 9
			lineResult += utils.Hex2string(strBlock[135:136]) + "|"
			//CM-WAIVE-LATE-NOTICE	PIC 9
			lineResult += utils.Hex2string(strBlock[136:137]) + "|"
			//CM-WAIVE-ANNUAL-FEE 	PIC 9
			lineResult += utils.Hex2string(strBlock[137:138]) + "|"
			// //CM-WAIVE-OVER-LIMIT 	PIC 9
			lineResult += utils.Hex2string(strBlock[138:139]) + "|"
			//CM-WAIVE-SVC-CHRG   	PIC 9
			lineResult += utils.Hex2string(strBlock[139:140]) + "|"
			//CM-COLL-CARD-RQTD   	PIC 9
			lineResult += utils.Hex2string(strBlock[140:141]) + "|"
			//CM-STAT-CHNG-FLAG   	PIC 9
			lineResult += utils.Hex2string(strBlock[141:142]) + "|"
			//CM-REQUEST-DISPLAY  	PIC 9
			lineResult += utils.Hex2string(strBlock[142:143]) + "|"
			//CM-REGION-CODES     	PIC X(9)
			lineResult += utils.Hex2string(strBlock[143:152]) + "|"
			//CM-EXCEPTION-RESPONSE-CODE	PIC X(2)
			lineResult += utils.Hex2string(strBlock[152:154]) + "|"
			//CM-CHGOFF-STATUS-FLAG	PIC 9
			lineResult += utils.Hex2string(strBlock[154:155]) + "|"
			//CM-WRITE-OFF-DAYS   	PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[155:157])[0:3] + "|"
			//CM-CR-BAL-REFUND-DAYS	PIC S9(3)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[157:159])[0:3] + "|"
			//CM-AFFINITY-FLAG    	PIC X
			lineResult += utils.Hex2string(strBlock[159:160]) + "|"
			//CM-1098-FLAG        	PIC 9
			lineResult += utils.Hex2string(strBlock[160:161]) + "|"
			//CM-ANN-FEE-DISCLOSURE-SW	PIC X
			lineResult += utils.Hex2string(strBlock[161:162]) + "|"
			//CM-FIRST-USG-FLAG   	PIC X
			lineResult += utils.Hex2string(strBlock[162:163]) + "|"
			//CM-CASH-ADV-FLAG    	PIC X
			lineResult += utils.Hex2string(strBlock[163:164]) + "|"
			//CM-DTE-OPENED       	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[164:168])[0:7] + "|"
			//CM-DTE-LST-STAT-CHNG	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[168:172])[0:7] + "|"
			//CM-DTE-CLOSED       	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[172:176])[0:7] + "|"
			//CM-CARD-FEE-DTE     	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[176:180])[0:7] + "|"
			//CM-DTE-LST-PURCHASE 	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[180:184])[0:7] + "|"
			//CM-DTE-LST-ADVANCE  	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[184:188])[0:7] + "|"
			//CM-DTE-LST-DELQ     	PIC S9(7)      	COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[188:192])[0:7] + "|"
			//CM-DTE-PRIOR-DELQ   	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[192:196])[0:7] + "|"
			//CM-DTE-LST-ACCR     	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[196:200])[0:7] + "|"
			//CM-DTE-ACCR-THRU    	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[200:204])[0:7] + "|"
			//CM-DTE-LST-ACTIVE   	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[204:208])[0:7] + "|"
			//CM-DTE-LST-CASH-PYMT	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[208:212])[0:7] + "|"
			//CM-DTE-LST-RTL-PYMT 	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[212:216])[0:7] + "|"
			//CM-CRLIMIT-DTE      	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[216:220])[0:7] + "|"
			//CM-LST-CRLIMIT-DTE  	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[220:224])[0:7] + "|"
			//CM-ACH-PYMT-EXP-DTE 	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[224:228])[0:7] + "|"
			//CM-DTE-LST-STMT     	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[228:232])[0:7] + "|"
			//CM-DTE-NXT-STMT     	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[232:236])[0:7] + "|"
			//CM-DTE-PYMT-DUE     	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[236:240])[0:7] + "|"
			//CM-CARD-EXPIR-DTE   	PIC S9(4)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[240:243])[1:5] + "|"
			//CM-CARD-CREATE-DTE  	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[243:247])[0:7] + "|"
			//CM-LST-EXPIR-DTE    	PIC S9(4)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[247:250])[1:5] + "|"
			// CM-DTE-HI-BALANCE   	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[250:254])[0:7] + "|"
			//CM-DTE-LST-PYMT     	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[254:258])[0:7] + "|"
			//CM-DTE-CR-BALANCE   	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[258:262])[0:7] + "|"
			//CM-DTE-WARNING-BULLETIN	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[262:266])[0:7] + "|"
			// //CM-DTE-LST-RTN-CHECK		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[266:270])[0:7] + "|"
			//CM-DTE-LST-DEBIT    		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[270:274])[0:7] + "|"
			//CM-DTE-TBL-HI-BALANCE		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[274:278])[0:7] + "|"
			//CM-DTE-LST-MAINT    		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[278:282])[0:7] + "|"
			//CM-DTE-LST-RATE-CHG 		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[282:286])[0:7] + "|"
			//CM-DTE-INTO-COLLECTION	PIC S9(7)       COMP-3
			lineResult += utils.Hex2string_comp3(strBlock[286:290])[0:7] + "|"
			//CM-ALT-CUST-EXPIR-DTE		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[290:294])[0:7] + "|"
			//CM-DUP-STMT-EXPIR-DTE		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[294:298])[0:7] + "|"
			//CM-PYMT-TABLE-DTE   		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[298:302])[0:7] + "|"
			//CM-USER-DTE-1       		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[302:306])[0:7] + "|"
			//CM-USER-DTE-2       		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[306:310])[0:7] + "|"
			//CM-DTE-EXCEPTION-PURGE	PIC S9(4)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[310:313])[1:5] + "|"
			//CM-DTE-XFER-EFFECTIVE		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[313:317])[0:7] + "|"
			//CM-LATE-FEE-DATE    		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[317:321])[0:7] + "|"
			//CM-1ST-LATE-NOTICE-DATE	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[321:325])[0:7] + "|"
			//CM-2ND-LATE-NOTICE-DATE	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[325:329])[0:7] + "|"
			//CM-DTE-CHGOFF-STAT-CHANGE	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[329:333])[0:7] + "|"
			//CM-DTE-CR-BAL-RANGE 		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[333:337])[0:7] + "|"
			//CM-DTE-BLOCK-CODE   		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[337:341])[0:7] + "|"
			//CM-DTE-ALT-BLOCK-CODE		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[341:345])[0:7] + "|"
			//CM-DTE-FILLER-6     		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[345:349])[0:7] + "|"
			//CM-DTE-FILLER-5     		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[349:353])[0:7] + "|"
			//CM-DTE-FILLER-4     		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[353:357])[0:7] + "|"
			//CM-DTE-FILLER-3     		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[357:361])[0:7] + "|"
			//CM-DTE-FILLER-2     		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[361:365])[0:7] + "|"
			//CM-DTE-FILLER-1     		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[365:369])[0:7] + "|"
			//CM-CRLIMIT         		PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[369:374])[0:9] + "|"
			//CM-LST-CRLIMIT      		PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[374:379])[0:9] + "|"
			//CM-HI-BALANCE       		PIC S9(9)       COMP-3.
			if utils.Hex2string_comp3(strBlock[379:384])[9:10] == "d" {
				lineResult += "-"
			} else {
				lineResult += " "
			}
			lineResult += utils.Hex2string_comp3(strBlock[379:384])[0:9] + "|"
			//CM-PYMT-TABLE-HI-BAL		PIC S9(9)V99    COMP-3.
			if utils.Hex2string_comp3(strBlock[384:390])[11:12] == "d" {
				lineResult += "-"
			} else {
				lineResult += " "
			}
			lineResult += utils.Hex2string_comp3(strBlock[384:390])[0:11] + "|"
			//CM-NMBR-RTN-CHECKS  		PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[390:392])[0:3] + "|"
			//CM-MONTHS-BALANCE   		PIC S999        COMP-3.
			if utils.Hex2string_comp3(strBlock[392:394])[3:4] == "d" {
				lineResult += "-"
			} else {
				lineResult += " "
			}
			lineResult += utils.Hex2string_comp3(strBlock[392:394])[0:3] + "|"
			//CM-MONTHS-PURCHASE  		PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[394:396])[0:3] + "|"
			//CM-HISTORY          		PIC X(48).		REDEFINES
			lineResult += utils.Hex2string(strBlock[396:444]) + "|"
			//CM-COLL-HISTORY-FLAG		PIC X.
			lineResult += utils.Hex2string(strBlock[444:445]) + "|"
			//CM-OL-FLAG          		PIC 9.
			lineResult += utils.Hex2string(strBlock[445:446]) + "|"
			//CM-OLC-REASON       		PIC X.
			lineResult += utils.Hex2string(strBlock[446:447]) + "|"
			//CM-OL-AMT-DUE       		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[447:453])[0:11] + "|"
			//CM-OLC-RESTRUCTURE-BAL	PIC S9(9)V99    COMP-3.
			if utils.Hex2string_comp3(strBlock[453:459])[11:12] == "d" {
				lineResult += "-"
			} else {
				lineResult += " "
			}
			lineResult += utils.Hex2string_comp3(strBlock[453:459])[0:11] + "|"
			//CM-CHGOFF-RSN-CD-1		PIC X.
			lineResult += utils.Hex2string(strBlock[459:460]) + "|"
			//CM-CHGOFF-RSN-CD-2		PIC X.
			lineResult += utils.Hex2string(strBlock[460:461]) + "|"
			//CM-EMBOSSER-NAME-1		PIC X(26).
			lineResult += utils.Hex2string(strBlock[461:487]) + "|"
			//CM-EMB-1-CARD-DATA		PIC X(12).
			lineResult += utils.Hex2string(strBlock[487:499]) + "|"
			//CM-EMBOSSER-NAME-2		PIC X(26).
			lineResult += utils.Hex2string(strBlock[499:525]) + "|"
			//CM-EMB-2-CARD-DATA		PIC X(12).
			lineResult += utils.Hex2string(strBlock[525:537]) + "|"
			//CM-EMBOSSER-NAME-3		PIC X(26).
			lineResult += utils.Hex2string(strBlock[537:563]) + "|"
			//CM-EMB-3-CARD-DATA		PIC X(12).
			lineResult += utils.Hex2string(strBlock[563:575]) + "|"
			//CM-EMBOSSER-NAME-4		PIC X(26).
			lineResult += utils.Hex2string(strBlock[575:601]) + "|"
			//CM-EMB-4-CARD-DATA		PIC X(12).		REDEFINES
			lineResult += utils.Hex2string(strBlock[601:613]) + "|"
			//CM-CHECKING-ACCT    		PIC S9(16)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[613:622])[1:17] + "|"
			//CM-SAVINGS-ACCT     		PIC S9(16)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[622:631])[1:17] + "|"
			//CM-USER-ACCT        		PIC S9(16)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[631:640])[1:17] + "|"
			//CM-PIN-OFFSET-FLAG  		PIC X(01).
			lineResult += utils.Hex2string(strBlock[640:641]) + "|"
			//FILLER              		PIC X(03).
			lineResult += utils.Hex2string(strBlock[641:644]) + "|"
			//CM-DTE-LST-REQ      		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[644:648])[0:7] + "|"
			//CM-CARD-USAGE-FLAG  		PIC 9.
			lineResult += utils.Hex2string(strBlock[648:649]) + "|"
			//CM-PRE-REISSUE-ACTION		PIC 9.
			lineResult += utils.Hex2string(strBlock[649:650]) + "|"
			//CM-REISSUE-ACTION   		PIC 9.
			lineResult += utils.Hex2string(strBlock[650:651]) + "|"
			//CM-TOTAL-NMBR-CARDS 		PIC 99.
			lineResult += utils.Hex2string(strBlock[651:653]) + "|"
			//CM-PYMT-SKIP-FLAG			PIC 9.
			lineResult += utils.Hex2string(strBlock[653:654]) + "|"
			//CM-PYMT-FLAG    			PIC 9.
			lineResult += utils.Hex2string(strBlock[654:655]) + "|"
			//CM-ACH-FLAG     			PIC 9.
			lineResult += utils.Hex2string(strBlock[655:656]) + "|"
			//CM-FIXED-PYMT-AMNT		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[656:662])[0:11] + "|"
			//CM-ACH-RT-NMBR  			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[662:667])[0:9] + "|"
			//CM-ACH-DB-NMBR  			PIC S9(16)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[667:676])[1:17] + "|"
			//CM-EP-NBR-PIN-RETRIES		PIC 9(2).
			lineResult += utils.Hex2string(strBlock[676:678]) + "|"
			//CM-EP-PIN-RETRIES-CURR-CODE	PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[678:680])[0:3] + "|"
			//CM-EP-PIN-RETRIES-DATE	PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[680:684])[0:7] + "|"
			//FILLER          			PIC X.
			lineResult += utils.Hex2string(strBlock[684:685]) + "|"
			//CM-BEG-CYCLE-DUE			PIC 9.
			lineResult += utils.Hex2string(strBlock[685:686]) + "|"
			//CM-CYCLE-DUE    			PIC 9.
			lineResult += utils.Hex2string(strBlock[686:687]) + "|"
			//CM-AMNT-PREPAID			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[687:693])[0:11] + "|"
			//CM-TOTAL-AMNT-DUE			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[693:699])[0:11] + "|"
			//CM-REAGE-REQUEST			PIC X.
			lineResult += utils.Hex2string(strBlock[699:700]) + "|"
			//CM-DTE-LST-REAGE			PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[700:704])[0:7] + "|"
			//CM-CURR-DUE				PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[704:710])[0:11] + "|"
			//CM-PAST-DUE				PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[710:716])[0:11] + "|"
			//CM-30DAYS-DELQ			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[716:722])[0:11] + "|"
			//CM-60DAYS-DELQ			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[722:728])[0:11] + "|"
			//CM-90DAYS-DELQ			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[728:743])[0:11] + "|"
			//CM-120DAYS-DELQ			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[734:740])[0:11] + "|"
			//CM-150DAYS-DELQ			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[740:746])[0:11] + "|"
			//CM-180DAYS-DELQ			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[746:752])[0:11] + "|"
			//CM-210DAYS-DELQ			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[752:758])[0:11] + "|"
			//CM-DELQ-COUNTER1			PIC 99.
			lineResult += utils.Hex2string(strBlock[758:760]) + "|"
			//CM-DELQ-COUNTER2			PIC 99.
			lineResult += utils.Hex2string(strBlock[760:762]) + "|"
			//CM-DELQ-COUNTER3			PIC 99.
			lineResult += utils.Hex2string(strBlock[762:764]) + "|"
			//CM-DELQ-COUNTER4			PIC 99.
			lineResult += utils.Hex2string(strBlock[764:766]) + "|"
			//CM-DELQ-COUNTER5			PIC 99.
			lineResult += utils.Hex2string(strBlock[766:768]) + "|"
			//CM-DELQ-COUNTER6			PIC 99.
			lineResult += utils.Hex2string(strBlock[768:770]) + "|"
			//CM-DELQ-COUNTER7			PIC 99.
			lineResult += utils.Hex2string(strBlock[770:772]) + "|"
			//CM-DELQ-COUNTER8			PIC 99.
			lineResult += utils.Hex2string(strBlock[772:774]) + "|"
			//CM-WAIVE-PREPAY 			PIC 9.
			lineResult += utils.Hex2string(strBlock[774:775]) + "|"
			//CM-PREPAY-CYCLES-LEFT		PIC 99.
			lineResult += utils.Hex2string(strBlock[775:777]) + "|"
			//CM-LAST-PYMT-SW 			PIC X.
			lineResult += utils.Hex2string(strBlock[777:778]) + "|"
			//CM-LAST-PYMT-AGED-COUNT	PIC S9          COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[778:779])[0:1] + "|"
			//CM-LAST-PYMT-CHANGE1		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[779:785])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE2		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[785:791])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE3		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[791:797])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE4		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[797:803])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE5		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[803:809])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE6		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[809:815])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE7		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[815:821])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE8		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[821:827])[0:11] + "|"
			//CM-LAST-PYMT-CHANGE9		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[827:833])[0:11] + "|"
			//CM-LAST-PYMT-PREPAY-CHANGE	PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[833:839])[0:11] + "|"
			//CM-RTL-BEG-BALANCE  			PIC S9(9)V99    COMP-3.
			if utils.Hex2string_comp3(strBlock[839:845])[11:12] == "d" {
				lineResult += "-"
			} else {
				lineResult += " "
			}
			lineResult += utils.Hex2string_comp3(strBlock[839:845])[0:11] + "|"
			//CM-RTL-AMNT-DB      			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[845:851])[0:11] + "|"
			//CM-RTL-AMNT-CR      			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[851:857])[0:11] + "|"
			//CM-RTL-BALANCE      			PIC S9(9)V99    COMP-3.
			if utils.Hex2string_comp3(strBlock[857:873])[11:12] == "d" {
				lineResult += "-"
			} else {
				lineResult += " "
			}
			lineResult += utils.Hex2string_comp3(strBlock[857:863])[0:11] + "|"
			//CM-RTL-IBNP         			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[863:869])[0:11] + "|"
			//CM-RTL-SVC-BNP      			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[869:873])[0:7] + "|"
			//CM-RTL-MISC-FEES    			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[873:877])[0:7] + "|"
			//CM-RTL-INSUR-BNP    			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[877:881])[0:7] + "|"
			//CM-RTL-MEMBER-BNP   			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[881:885])[0:7] + "|"
			//CM-RTL-MISC-CTD     			PIC S9(7)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[885:890])[0:9] + "|"
			///CM-RTL-DISPUTED-BAL 			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[890:896], "d") + "|"
			//CM-RTL-CURR-MON-BAL 			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[896:902], "d") + "|"
			//CM-RTL-ACCRUED-INTR 			PIC S9(9)V9(4)  COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[902:909])[0:13] + "|"
			//CM-RTL-NMBR-DB      			PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[909:911])[0:3] + "|"
			//CM-RTL-NMBR-CR      			PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[911:913])[0:3] + "|"
			//CM-RTL-PYMT-CTD     			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[913:919])[0:11] + "|"
			//CM-RTL-YTD-INTR-BILLED		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[919:925])[0:11] + "|"
			//CM-RTL-IBNP-LST-STMT			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[925:931])[0:11] + "|"
			//CM-RTL-YTD-INTR-PAID			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[931:937])[0:11] + "|"
			//CM-RTL-LST-YTD-INTR-PAID		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[937:943])[0:11] + "|"
			//CM-RTL-AMNT-PYMT-REV			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[943:949])[0:11] + "|"
			//CM-RTL-STD-INTR     			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[949:955])[0:11] + "|"
			//CM-RTL-PROVIS-BAL           	PIC S9(9)V99 COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[955:961], "d") + "|"
			//CM-RTL-PROVIS-INTR          	PIC S9(9)V9(4) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[961:968])[0:13] + "|"
			//CM-RTL-FILLER               	PIC X.
			lineResult += utils.Hex2string(strBlock[968:969]) + "|"
			//CM-CASH-BEG-BALANCE 			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[969:975], "d") + "|"
			//CM-CASH-AMNT-DB     			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[975:981])[0:11] + "|"
			//CM-CASH-AMNT-CR     			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[981:987])[0:11] + "|"
			//CM-CASH-BALANCE     			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[987:993], "d") + "|"
			//CM-CASH-IBNP        			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[993:999])[0:11] + "|"
			//CM-CASH-SVC-BNP     			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[999:1003])[0:7] + "|"
			//CM-CASH-DISPUTED-BAL			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1003:1009], "d") + "|"
			//CM-CASH-CURR-MON-BAL			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1009:1015], "d") + "|"
			//CM-CASH-ACCRUED-INTR			PIC S9(9)V9(4)  COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1015:1022])[0:13] + "|"
			//CM-CASH-NMBR-DB     			PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1022:1024])[0:3] + "|"
			//CM-CASH-NMBR-CR     			PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1024:1026])[0:3] + "|"
			//CM-CASH-PYMT-CTD    			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1026:1032])[0:11] + "|"
			//CM-CASH-YTD-INTR-BILLED		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1032:1038])[0:11] + "|"
			//CM-CASH-IBNP-LST-STMT			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1038:1044])[0:11] + "|"
			//CM-CASH-YTD-INTR-PAID			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1044:1050])[0:11] + "|"
			//CM-CASH-LST-YTD-INTR-PAID		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1050:1056])[0:11] + "|"
			//CM-CASH-AMNT-PYMT-REV			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1056:1062])[0:11] + "|"
			//CM-CASH-STD-INTR    			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1062:1068])[0:11] + "|"
			//CM-RTL-NEG-PROVIS-INTR      	PIC S9(9)V9(4) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1068:1075])[0:13] + "|"
			//CM-RTL-NEG-ANTICI-INTR      	PIC S9(9)V9(4) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1075:1084])[0:13] + "|"
			//CM-CASH-FILLER      			PIC X(2).
			lineResult += utils.Hex2string(strBlock[1082:1084]) + "|"
			//CM-CURR-BALANCE     			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1084:1090], "d") + "|"
			//CM-AMNT-LST-DEBIT   			PIC S9(7)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1090:1095])[0:9] + "|"
			//CM-AMNT-LST-PURCH   			PIC S9(7)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1095:1100])[0:9] + "|"
			//CM-AMNT-LST-ADV   			PIC S9(7)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1100:1105])[0:9] + "|"
			//CM-AMNT-OUTST-AUTH  			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1105:1111])[0:11] + "|"
			//CM-NMBR-OUTST-AUTH  			PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1111:1113])[0:3] + "|"
			//CM-AVAIL-CREDIT     			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1113:1119], "d") + "|"
			//CM-CSH-ADV-LIM      			PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1119:1123])[0:7] + "|"
			//CM-CSH-ADV-AVAIL    			PIC S9(7)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1123:1128], "d") + "|"
			//CM-GRACE-DAYS       			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1128:1130])[0:3] + "|"
			//CM-LST-YTD-INTR-BILLED		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1130:1136])[0:11] + "|"
			//CM-CURR-YTD-SVC-CHRG			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1136:1142])[0:11] + "|"
			//CM-AGGR-CASH-BALANCE			PIC S9(11)V99   COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1142:1149], "d") + "|"
			//CM-AGGR-RTL-BALANCE 			PIC S9(11)V99   COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1149:1156], "d") + "|"
			//CM-NMBR-DISPUTED-ITEMS		PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1156:1159])[0:5] + "|"
			//CM-AMNT-DISPUTED-ITEMS		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1159:1165])[0:11] + "|"
			//CM-RTL-AMNT-CHARGE-OFF		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1165:1171])[0:11] + "|"
			//CM-CASH-AMNT-CHARGE-OFF		PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1171:1177])[0:11] + "|"
			//CM-LST-PYMT-AMNT    			PIC S9(7)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1177:1182])[0:9] + "|"
			//CM-AGGR-CASH-DAYS   			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1182:1184])[0:3] + "|"
			//CM-AGGR-RTL-DAYS    			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1184:1186])[0:3] + "|"
			//CM-AMNT-AUTH-TDY    			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1186:1192])[0:11] + "|"
			//CM-NMBR-AUTH-TDY    			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1192:1194])[0:3] + "|"
			//CM-AMNT-DECL-TDY    			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1194:1200])[0:11] + "|"
			//CM-NMBR-DECL-TDY    			PIC S999        COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1200:1202])[0:3] + "|"
			//CM-ANNUAL-FEE       			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1202:1206])[0:7] + "|"
			//CM-BB-BEG-BALANCE				PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1206:1210])[0:7] + "|"
			//CM-BB-USED-CTD  				PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1210:1214])[0:7] + "|"
			//CM-CONV-BALANCE     			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1214:1220], "d") + "|"
			//CM-2ND-LATE-NOTICE-COUNTER	PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1220:1222])[0:3] + "|"
			//CM-AGGR-YTD-DAYS    			PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1222:1224])[0:3] + "|"
			//CM-AGGR-YTD-BALANCE 			PIC S9(11)V99   COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1224:1231], "d") + "|"
			//CM-AGGR-LST-YTD-DAYS			PIC S9(3)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1231:1233])[0:3] + "|"
			//CM-AGGR-LST-YTD-BALANCE		PIC S9(11)V99   COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1233:1240], "d") + "|"
			//CM-RTL-NEG-ANTICI-BAL       	PIC S9(9)V99 COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1240:1246], "d") + "|"
			//CM-RTL-NEG-PROVIS-BAL       	PIC S9(9)V99 COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1246:1252], "d") + "|"
			//CM-CI-OPTION        			PIC 9.
			lineResult += utils.Hex2string(strBlock[1252:1253]) + "|"
			//CM-CI-FLAG          			PIC 9.
			lineResult += utils.Hex2string(strBlock[1253:1254]) + "|"
			//CM-CI-CHANGE-FLAG   			PIC 9.
			lineResult += utils.Hex2string(strBlock[1254:1255]) + "|"
			//CM-CI-LIMIT-1       			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1255:1260])[0:9] + "|"
			//CM-CI-LIMIT-2       			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1260:1265])[0:9] + "|"
			//CM-CI-RATE-1        			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1265:1269])[0:7] + "|"
			//CM-CI-ADJ-1         			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1269:1273])[0:7] + "|"
			//CM-CI-RATE-2        			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1273:1277])[0:7] + "|"
			//CM-CI-ADJ-2         			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1277:1281])[0:7] + "|"
			//CM-CI-RATE-3        			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1281:1285])[0:7] + "|"
			//CM-CI-ADJ-3         			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1285:1289])[0:7] + "|"
			//CM-RI-OPTION        			PIC 9.
			lineResult += utils.Hex2string(strBlock[1289:1290]) + "|"
			//CM-RI-FLAG          			PIC 9.
			lineResult += utils.Hex2string(strBlock[1290:1291]) + "|"
			//CM-RI-CHANGE-FLAG   			PIC 9.
			lineResult += utils.Hex2string(strBlock[1291:1292]) + "|"
			//CM-RI-LIMIT-1       			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1292:1297])[0:9] + "|"
			//CM-RI-LIMIT-2       			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1297:1302])[0:9] + "|"
			//CM-RI-RATE-1        			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1302:1306])[0:7] + "|"
			//CM-RI-ADJ-1         			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1306:1310])[0:7] + "|"
			//CM-RI-RATE-2        			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1310:1314])[0:7] + "|"
			//CM-RI-ADJ-2         			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1314:1318])[0:7] + "|"
			//CM-RI-RATE-3        			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1318:1322])[0:7] + "|"
			//CM-RI-ADJ-3         			PIC SV9(7)      COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1322:1326])[0:7] + "|"
			//CM-INTR-SW          			PIC 9.
			lineResult += utils.Hex2string(strBlock[1326:1327]) + "|"
			//CM-INTR-PER-DIEM    			PIC S9(5)V9(4)  COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1327:1332])[0:9] + "|"
			//CM-POT-SELECT-CODE  			PIC 99.
			lineResult += utils.Hex2string(strBlock[1332:1334]) + "|"
			//CM-POT-EXP-DATE     			PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1334:1337])[0:5] + "|"
			//CM-PEND-POT-SELECT-CODE		PIC 99.
			lineResult += utils.Hex2string(strBlock[1337:1339]) + "|"
			//CM-PEND-POT-EXP-DATE			PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1339:1342])[0:5] + "|"
			//CM-PRIOR-POT-SELECT-CODE		PIC 99.
			lineResult += utils.Hex2string(strBlock[1342:1344]) + "|"
			//CM-PRIOR-POT-EXP-DATE			PIC S9(5)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1344:1347])[0:5] + "|"
			//CM-CASH-ADV-PER-FEE-CTD		PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1347:1351])[0:7] + "|"
			//CM-INTR-FILLER      			PIC X.
			lineResult += utils.Hex2string(strBlock[1351:1352]) + "|"
			//CM-IE-ACCRUED       			PIC S9(7)V9(4) COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1352:1358])[0:11] + "|"
			//CM-IE-AGGR-BAL      			PIC S9(9)V99 COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1358:1364], "d") + "|"
			//CM-IE-AGGR-DAYS     			PIC S999     COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1364:1366])[0:3] + "|"
			//CM-IE-YTD           			PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1366:1372])[0:11] + "|"
			//CM-IE-PRIOR-YTD     			PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1372:1378])[0:11] + "|"
			//CM-IE-TAX-YTD       			PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1378:1384])[0:11] + "|"
			//CM-IE-PRIOR-TAX-YTD 			PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1384:1390])[0:11] + "|"
			//CM-TAX-FISC-YTD     			PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1390:1396])[0:11] + "|"
			//CM-TAX-FISC-PRIOR-YTD			PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1396:1402])[0:11] + "|"
			//CM-PYMT-PENDING    			PIC X.
			lineResult += utils.Hex2string(strBlock[1402:1403]) + "|"
			//CM-BDG-MULTIPLE    			PIC S99V9    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1403:1405])[0:3] + "|"
			//CM-INS-NO-POLICIES 			PIC 99.
			lineResult += utils.Hex2string(strBlock[1405:1407]) + "|"
			//CM-DTE-LST-CYCLE       		PIC S9(7)    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1407:1411])[0:7] + "|"
			//CM-COLL-BLOCK-CODE     		PIC X.
			lineResult += utils.Hex2string(strBlock[1411:1412]) + "|"
			//CM-INSTL-LIMIT  				PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1412:1417])[0:9] + "|"
			//CM-INSTL-BAL    				PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1417:1423], "d") + "|"
			//CM-AMNT-OUTST-INSTL			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1423:1429])[0:11] + "|"
			//CM-AVAIL-INSTL  				PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1429:1435], "d") + "|"
			//FILLER          				PIC X(16).
			// lineResult += utils.Hex2string(strBlock[1435:1451]) + "|"
			lineResult += "                " + "|"
			//CM-INSTL-PROVIS-BAL			PIC S9(7)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1451:1456], "d") + "|"
			//CM-INSTL-PROVIS-INTR			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1456:1460])[0:7] + "|"
			//CM-INSTL-STMT-BAL				PIC S9(7)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1451:1465], "d") + "|"
			//CM-INSTL-STMT-INTR			PIC S9(5)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1465:1469])[0:7] + "|"
			//CM-CTD-EXC-LOUNGE				PIC 9(1).
			lineResult += utils.Hex2string(strBlock[1469:1470]) + "|"
			//CM-CHIP-1ST-USAGE				PIC X(01).
			if utils.Hex2string(strBlock[1470:1471]) != "Y" {
				lineResult += " " + "|"
			} else {
				lineResult += "Y" + "|"
			}
			//CM-BLOCKED-REASON				PIC X(02).
			lineResult += utils.Hex2string(strBlock[1471:1473]) + "|"
			//CM-WAIVE-JOINING-FEE			PIC 9.
			lineResult += utils.Hex2string(strBlock[1473:1474]) + "|"
			//CM-CRLIMIT-PERM     			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1474:1479])[0:9] + "|"
			//CM-CRLIMIT-TEMP     			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1479:1484])[0:9] + "|"
			//CM-CRLIMIT-TEMP-EFF-DTE 		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1484:1488])[0:7] + "|"
			//CM-CRLIMIT-TEMP-EXP-DTE		PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1488:1492])[0:7] + "|"
			//CM-CASH-LIMIT       			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1492:1497])[0:9] + "|"
			//CM-AVAIL-CASH       			PIC S9(9)V99    COMP-3.
			lineResult += utils.ParseComp3SignedMode(strBlock[1497:1503], "d") + "|"
			//CM-CASH-ADV-OS-AUTH			PIC S9(9)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1503:1508])[0:9] + "|"
			//CM-DTE-LST-CASH-AUTH			PIC S9(7)       COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1508:1512])[0:7] + "|"
			//CM-XFR-IND          			PIC 9.
			lineResult += utils.Hex2string(strBlock[1512:1513]) + "|"
			//CM-POSTING-FLAG              	PIC XX.
			lineResult += utils.Hex2string(strBlock[1513:1515]) + "|"
			///CM-POSTING-ACCT-ORGN    		PIC S999     COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1515:1517])[0:3] + "|"
			//CM-POSTING-ACCT-TYPE    		PIC S999     COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1517:1519])[0:3] + "|"
			//CM-POSTING-ACCT-NMBR    		PIC S9(16)   COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1519:1528])[1:17] + "|"
			//CM-NP-CTD-USED-AMNT        	PIC S9(9)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1528:1534])[0:11] + "|"
			//CM-NP-CTD-USED-CASH         	PIC S9(9)V99   COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1534:1540])[0:11] + "|"
			//FILLER-JCB          			PIC X(02).
			lineResult += utils.Hex2string(strBlock[1540:1542]) + "|"
			//CM-CASH-CR-ADJUST-BNP			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1540:1548])[0:11] + "|"
			//CM-RTL-CR-ADJUST-BNP			PIC S9(9)V99    COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1548:1554])[0:11] + "|"
			//CM-OL-CASH-PYMT     			PIC S9(09)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1554:1560])[0:11] + "|"
			//CM-OL-RTL-PYMT      			PIC S9(09)V99 COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1560:1566])[0:11] + "|"
			//CM-LST-DTE-PYMT-DUE   		PIC S9(07)  COMP-3.
			lineResult += utils.Hex2string_comp3(strBlock[1566:1570])[0:7] + "|"
			//CM-PREV-CC-REASON   			PIC X(1).
			lineResult += utils.Hex2string(strBlock[1570:1571]) + "|"
			//FILLER              			PIC X(08).
			lineResult += utils.Hex2string(strBlock[1571:1579]) + "|"
			//FILLER              			PIC X(01).
			lineResult += utils.Hex2string(strBlock[1579:1580]) + "|"
			//CM-OFFLINE-BLOCK   			PIC X(01).
			lineResult += utils.Hex2string(strBlock[1580:1581]) + "|"
			//CM-AUTH-TOL-GRP     			PIC X           VALUE SPACES.
			lineResult += utils.Hex2string(strBlock[1581:1582]) + "|"
			//FILLER              			PIC X(18).
			// lineResult += utils.Hex2string(strBlock[1582:1600]) + "|\r\n"
			lineResult += "                  " + "\r\n"
		}

		return lineResult
	})

}
