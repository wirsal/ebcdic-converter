package cia

import (
	"time"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func Cpmer2text(inputFilename string) bool {
	start := time.Now()

	// Ambil konfigurasi
	outputFilename := inputFilename // diasumsikan output sama dengan input
	recordLength := viper.GetInt("file.CPMER.length")
	batchSize := viper.GetInt("server.batchSize")

	// Baca file input
	content, err := utils.ReadAllFile(inputFilename)
	if err != nil {
		utils.Error("Failed to read file: %v", err)
		return false
	}

	var (
		line      int
		counter   int
		numRecord int
		data      string
	)

	fileLength := len([]byte(content))
	if fileLength > 9000 {
		nmbrParse := ((fileLength - 9000) / recordLength) + 1

		for numRecord < nmbrParse {
			startIdx := 4500 + recordLength*line
			endIdx := recordLength * (line + 1)

			if startIdx > fileLength {
				break
			}
			if endIdx > fileLength {
				endIdx = fileLength
			}

			strBlock := content[startIdx:endIdx]
			output := decodeCPMER(strBlock)

			line++
			counter++
			numRecord++

			if len(output) > 0 {
				data += output
			}

			if numRecord%batchSize == 0 {
				if !utils.WriteAndCheck(outputFilename, data) {
					return false
				}
				data = ""
			}
		}

		// Tulis sisa data jika ada
		if len(data) > 0 {
			if !utils.WriteAndCheck(outputFilename, data) {
				return false
			}
		}
	}
	utils.Info("✅ %s Finished in %s", inputFilename, time.Since(start))
	return true
}

func decodeCPMER(strBlock string) string {
	return utils.SafeDecode("decodeCPMER", func() string {
		var lineResult string
		if utils.Hex2string_comp3(strBlock[0:2])[:3] != "999" {
			// Format hasil parsing
			lineResult += utils.Hex2string_comp3(strBlock[0:2])[:3] + "|"      // MMD-ORGN
			lineResult += utils.Hex2string_comp3(strBlock[2:7])[:9] + "|"      // MMD-ACCT
			lineResult += utils.Hex2string(strBlock[7:9]) + "|"                // MMD-REC-NBR
			lineResult += utils.Hex2string(strBlock[9:11]) + "|"               // MMD-MERCH-TYPE
			lineResult += utils.Hex2string(strBlock[11:36]) + "|"              // MMD-ID-NAME
			lineResult += utils.Hex2string(strBlock[36:49]) + "|"              // MMD-ID-CITY
			lineResult += utils.Hex2string(strBlock[49:51]) + "|"              // MMD-ID-STATE
			lineResult += utils.Hex2string(strBlock[51:81]) + "|"              // MMD-ADDRESS-1
			lineResult += utils.Hex2string(strBlock[81:111]) + "|"             // MMD-ADDRESS-2
			lineResult += utils.Hex2string(strBlock[111:141]) + "|"            // MMD-ADDRESS-3
			lineResult += utils.Hex2string(strBlock[141:171]) + "|"            // MMD-ADDRESS-4
			lineResult += utils.Hex2string(strBlock[171:201]) + "|"            // MMD-ADDRESS-5
			lineResult += utils.Hex2string(strBlock[201:210]) + "|"            // MMD-ZIP-CODE
			lineResult += utils.Hex2string(strBlock[210:228]) + "|"            // MMD-PHONE-NMBR
			lineResult += utils.Hex2string_comp3(strBlock[228:233])[:9] + "|"  // MMD-ROUTE-TRANSIT
			lineResult += utils.Hex2string_comp3(strBlock[233:241])[:15] + "|" // MMD-DB-ACCT-NMBR
			lineResult += utils.Hex2string(strBlock[241:266]) + "|"            // MMD-CONTACT
			lineResult += utils.Hex2string(strBlock[266:269]) + "|"            // MMD-OFFICER-ID
			lineResult += utils.Hex2string_comp3(strBlock[269:272])[:5] + "|"  // MMD-BRANCH
			lineResult += utils.Hex2string_comp3(strBlock[272:275])[:5] + "|"  // MMD-AGENT-BANK
			lineResult += utils.Hex2string_comp3(strBlock[275:280])[:9] + "|"  // MMD-CHAIN-STORE
			lineResult += utils.Hex2string(strBlock[280:281]) + "|"            // MMD-CARD-STATUS-1
			lineResult += utils.Hex2string(strBlock[281:282]) + "|"            // MMD-CARD-CARD-TYPE-1
			lineResult += utils.Hex2string(strBlock[282:283]) + "|"            // MMD-CARD-STATUS-2
			lineResult += utils.Hex2string(strBlock[283:284]) + "|"            // MMD-CARD-CARD-TYPE-2
			lineResult += utils.Hex2string(strBlock[284:285]) + "|"            // MMD-CARD-STATUS-3
			lineResult += utils.Hex2string(strBlock[285:286]) + "|"            // MMD-CARD-CARD-TYPE-3
			lineResult += utils.Hex2string(strBlock[286:287]) + "|"            // MMD-CARD-STATUS-4
			lineResult += utils.Hex2string(strBlock[287:288]) + "|"            // MMD-CARD-CARD-TYPE-4
			lineResult += utils.Hex2string(strBlock[288:289]) + "|"            // MMD-CARD-STATUS-5
			lineResult += utils.Hex2string(strBlock[289:290]) + "|"            // MMD-CARD-CARD-TYPE-5
			lineResult += utils.Hex2string(strBlock[290:291]) + "|"            // MMD-CARD-STATUS-6
			lineResult += utils.Hex2string(strBlock[291:292]) + "|"            // MMD-CARD-CARD-TYPE-6
			lineResult += utils.Hex2string(strBlock[292:293]) + "|"            // MMD-CARD-STATUS-7
			lineResult += utils.Hex2string(strBlock[293:294]) + "|"            // MMD-CARD-CARD-TYPE-7
			lineResult += utils.Hex2string(strBlock[294:295]) + "|"            // MMD-CARD-STATUS-8
			lineResult += utils.Hex2string(strBlock[295:296]) + "|"            // MMD-CARD-CARD-TYPE-8
			lineResult += utils.Hex2string(strBlock[296:297]) + "|"            // MMD-CARD-STATUS-9
			lineResult += utils.Hex2string(strBlock[297:298]) + "|"            // MMD-CARD-CARD-TYPE-9
			lineResult += utils.Hex2string(strBlock[298:299]) + "|"            // MMD-CARD-STATUS-10
			lineResult += utils.Hex2string(strBlock[299:300]) + "|"            // MMD-CARD-CARD-TYPE-10
			lineResult += utils.Hex2string(strBlock[300:301]) + "|"            // MMD-CARD-STATUS-11
			lineResult += utils.Hex2string(strBlock[301:302]) + "|"            // MMD-CARD-CARD-TYPE-11
			lineResult += utils.Hex2string(strBlock[302:303]) + "|"            // MMD-CARD-STATUS-12
			lineResult += utils.Hex2string(strBlock[303:304]) + "|"            // MMD-CARD-CARD-TYPE-12
			lineResult += utils.Hex2string(strBlock[304:305]) + "|"            // MMD-CARD-STATUS-13
			lineResult += utils.Hex2string(strBlock[305:306]) + "|"            // MMD-CARD-CARD-TYPE-13
			lineResult += utils.Hex2string(strBlock[306:307]) + "|"            // MMD-CARD-STATUS-14
			lineResult += utils.Hex2string(strBlock[307:308]) + "|"            // MMD-CARD-CARD-TYPE-14
			lineResult += utils.Hex2string(strBlock[308:309]) + "|"            // MMD-CARD-STATUS-15
			lineResult += utils.Hex2string(strBlock[309:310]) + "|"            // MMD-CARD-CARD-TYPE-15

			//*

			lineResult += utils.Hex2string_comp3(strBlock[310:313])[:5] + "|" // MMD-PCT-OF-DISC     PIC S999V99
			lineResult += utils.Hex2string(strBlock[313:314]) + "|"           // MMD-HOLD-STMT-FLAG  PIC 9
			lineResult += utils.Hex2string(strBlock[314:315]) + "|"           // MMD-STMT-MSG-SUPPRESS PIC 9
			lineResult += utils.Hex2string(strBlock[315:316]) + "|"           // MMD-MERCH-ADJ-FLAG  PIC X
			lineResult += utils.Hex2string(strBlock[316:317]) + "|"           // MMD-DISC-METHOD     PIC 9
			lineResult += utils.Hex2string(strBlock[317:318]) + "|"           // MMD-WHICH-MATRIX    PIC 9
			lineResult += utils.Hex2string_comp3(strBlock[318:321])[:5] + "|" // MMD-PROFIT-MARGIN   PIC SV9(5)
			lineResult += utils.Hex2string(strBlock[321:323]) + "|"           // MMD-DISC-FREQ       PIC 99
			lineResult += utils.Hex2string(strBlock[323:324]) + "|"           // MMD-REJ-UNAUTH-OVER-LIMIT PIC 9
			lineResult += utils.Hex2string(strBlock[324:325]) + "|"           // MMD-MEDIA-TYPE      PIC 9
			lineResult += utils.Hex2string(strBlock[325:326]) + "|"           // MMD-TAPE-BULLETIN   PIC 9
			lineResult += utils.Hex2string(strBlock[326:327]) + "|"           // MMD-ACH-MERCH-DP    PIC 9
			lineResult += utils.Hex2string(strBlock[327:328]) + "|"           // MMD-ACH-MERCH-CHGBK PIC 9
			lineResult += utils.Hex2string(strBlock[328:330]) + "|"           // MMD-USER-CODE-1     PIC XX
			lineResult += utils.Hex2string(strBlock[330:332]) + "|"           // MMD-USER-CODE-2     PIC XX
			lineResult += utils.Hex2string(strBlock[332:334]) + "|"           // MMD-USER-CODE-3     PIC XX
			lineResult += utils.Hex2string(strBlock[334:335]) + "|"           // MMD-INTCHG-REJECT   PIC X
			//*

			lineResult += utils.Hex2string_comp3(strBlock[335:340])[:9] + "|" // MMD-VISA-MEMBER-ID  PIC S9(9)
			lineResult += utils.Hex2string_comp3(strBlock[340:344])[:7] + "|" // MMD-DTE-OPENED      PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[344:348])[:7] + "|" // MMD-DTE-LST-STMT    PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[348:352])[:7] + "|" // MMD-DTE-LST-VISIT   PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[352:356])[:7] + "|" // MMD-DTE-NXT-VISIT   PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[356:360])[:7] + "|" // MMD-DTE-NXT-REVIEW  PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[360:364])[:7] + "|" // MMD-DTE-LST-RTE-ADJ PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[364:368])[:7] + "|" // MMD-DTE-LST-MAINT PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[368:372])[:7] + "|" // MMD-DTE-USER-1      PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[372:376])[:7] + "|" // MMD-DTE-USER-2      PIC S9(7)
			//*

			lineResult += utils.Hex2string_comp3(strBlock[376:378])[:3] + "|" // MMD-NBR-IMPRINTER1  PIC S999
			lineResult += utils.Hex2string_comp3(strBlock[378:380])[:3] + "|" // MMD-NBR-IMPRINTER2  PIC S999
			lineResult += utils.Hex2string_comp3(strBlock[380:382])[:3] + "|" // MMD-NBR-IMPRINTER3  PIC S999
			lineResult += utils.Hex2string_comp3(strBlock[382:384])[:3] + "|" // MMD-NBR-POS-DEV1    PIC S999
			lineResult += utils.Hex2string_comp3(strBlock[384:386])[:3] + "|" // MMD-NBR-POS-DEV2    PIC S999
			lineResult += utils.Hex2string_comp3(strBlock[386:388])[:3] + "|" // MMD-NBR-POS-DEV3    PIC S999
			lineResult += utils.Hex2string(strBlock[388:389]) + "|"           // MMD-ACH-MERCH-DISC-FEE  PIC 9
			lineResult += utils.Hex2string_comp3(strBlock[389:394])[:9] + "|" // MMD-CHAIN-MER-NBR   PIC S9(9)
			lineResult += utils.Hex2string(strBlock[394:395]) + "|"           // MMD-CHAIN-MER-LEVEL PIC X
			lineResult += utils.Hex2string(strBlock[395:396]) + "|"           // MMD-CHAIN-STMT-IND  PIC 9
			lineResult += utils.Hex2string(strBlock[396:397]) + "|"           // MMD-CHAIN-REPRT-IND PIC 9
			lineResult += utils.Hex2string(strBlock[397:398]) + "|"           // MMD-CHAIN-SETT-IND  PIC 9
			lineResult += utils.Hex2string(strBlock[398:399]) + "|"           // MMD-CHAIN-DISC-IND  PIC 9
			lineResult += utils.Hex2string(strBlock[399:400]) + "|"           // MMD-CHAIN-FEES-IND  PIC 9
			lineResult += utils.Hex2string(strBlock[400:401]) + "|"           // MMD-CHAIN-DD-IND    PIC 9

			// MMD-CARD-DEMO-DATA
			//CARD 1

			lineResult += utils.Hex2string(strBlock[401:409]) + "|"           // MMD-CARD-CARDH-1-8-FROM  PIC X8
			lineResult += utils.Hex2string(strBlock[409:410]) + "|"           // MMD-CARD-CARDH-OPER  PIC X
			lineResult += utils.Hex2string(strBlock[410:418]) + "|"           // MMD-CARD-CARDH-1-8-TO X8
			lineResult += utils.Hex2string_comp3(strBlock[418:422])[:7] + "|" // MMD-CARD-PER-TRANS S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[422:424])[:3] + "|" // MMD-CARD-NBR-BULLETINS S999
			lineResult += utils.Hex2string_comp3(strBlock[424:427])[:5] + "|" // MMD-CARD-DISC-RATE SV9(5)
			lineResult += utils.Hex2string_comp3(strBlock[427:430])[:5] + "|" // MMD-CARD-FLOOR-LIMIT S9(5)
			lineResult += utils.Hex2string(strBlock[430:431]) + "|"           // MMD-CARD-DISC-TYPE     PIC 9
			lineResult += utils.Hex2string(strBlock[431:432]) + "|"           // MMD-CARD-INTCHG-FEE     PIC 9
			lineResult += utils.Hex2string(strBlock[432:434]) + "|"           // MMD-CARD-USER-CODE    PIC XX

			//CARD 2
			lineResult += utils.Hex2string(strBlock[434:442]) + "|"           // MMD-CARD-CARDH-1-8-FROM  PIC X8
			lineResult += utils.Hex2string(strBlock[442:443]) + "|"           // MMD-CARD-CARDH-OPER  PIC X
			lineResult += utils.Hex2string(strBlock[443:451]) + "|"           // MMD-CARD-CARDH-1-8-TO X8
			lineResult += utils.Hex2string_comp3(strBlock[451:455])[:7] + "|" // MMD-CARD-PER-TRANS S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[455:457])[:3] + "|" // MMD-CARD-NBR-BULLETINS S999
			lineResult += utils.Hex2string_comp3(strBlock[457:460])[:5] + "|" // MMD-CARD-DISC-RATE SV9(5)
			lineResult += utils.Hex2string_comp3(strBlock[460:463])[:5] + "|" // MMD-CARD-FLOOR-LIMIT S9(5)
			lineResult += utils.Hex2string(strBlock[463:464]) + "|"           // MMD-CARD-DISC-TYPE     PIC 9
			lineResult += utils.Hex2string(strBlock[464:465]) + "|"           // MMD-CARD-INTCHG-FEE     PIC 9
			lineResult += utils.Hex2string(strBlock[465:467]) + "|"           // MMD-CARD-USER-CODE    PIC XX

			//CARD 3
			lineResult += utils.Hex2string(strBlock[467:475]) + "|"           // MMD-CARD-CARDH-1-8-FROM  PIC X8
			lineResult += utils.Hex2string(strBlock[475:476]) + "|"           // MMD-CARD-CARDH-OPER  PIC X
			lineResult += utils.Hex2string(strBlock[476:484]) + "|"           // MMD-CARD-CARDH-1-8-TO X8
			lineResult += utils.Hex2string_comp3(strBlock[484:488])[:7] + "|" // MMD-CARD-PER-TRANS S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[488:490])[:3] + "|" // MMD-CARD-NBR-BULLETINS S999
			lineResult += utils.Hex2string_comp3(strBlock[490:493])[:5] + "|" // MMD-CARD-DISC-RATE SV9(5)
			lineResult += utils.Hex2string_comp3(strBlock[493:496])[:5] + "|" // MMD-CARD-FLOOR-LIMIT S9(5)
			lineResult += utils.Hex2string(strBlock[496:497]) + "|"           // MMD-CARD-DISC-TYPE     PIC 9
			lineResult += utils.Hex2string(strBlock[497:498]) + "|"           // MMD-CARD-INTCHG-FEE     PIC 9
			lineResult += utils.Hex2string(strBlock[498:500]) + "|"           // MMD-CARD-USER-CODE    PIC XX

			//CARD 4
			lineResult += utils.Hex2string(strBlock[500:508]) + "|"           // MMD-CARD-CARDH-1-8-FROM  PIC X8
			lineResult += utils.Hex2string(strBlock[508:509]) + "|"           // MMD-CARD-CARDH-OPER  PIC X
			lineResult += utils.Hex2string(strBlock[509:517]) + "|"           // MMD-CARD-CARDH-1-8-TO X8
			lineResult += utils.Hex2string_comp3(strBlock[517:521])[:7] + "|" // MMD-CARD-PER-TRANS S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[521:523])[:3] + "|" // MMD-CARD-NBR-BULLETINS S999
			lineResult += utils.Hex2string_comp3(strBlock[523:526])[:5] + "|" // MMD-CARD-DISC-RATE SV9(5)
			lineResult += utils.Hex2string_comp3(strBlock[526:529])[:5] + "|" // MMD-CARD-FLOOR-LIMIT S9(5)
			lineResult += utils.Hex2string(strBlock[529:530]) + "|"           // MMD-CARD-DISC-TYPE     PIC 9
			lineResult += utils.Hex2string(strBlock[530:531]) + "|"           // MMD-CARD-INTCHG-FEE     PIC 9
			lineResult += utils.Hex2string(strBlock[531:533]) + "|"           // MMD-CARD-USER-CODE    PIC XX

			//CARD 5
			lineResult += utils.Hex2string(strBlock[533:541]) + "|"           //CARD5-EXPDATE
			lineResult += utils.Hex2string(strBlock[541:542]) + "|"           //CARD5-STATUS
			lineResult += utils.Hex2string(strBlock[542:550]) + "|"           //CARD5-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[550:554])[:7] + "|" //CARD5-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[554:556])[:3] + "|" //CARD5-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[556:559])[:5] + "|" //CARD5-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[559:562])[:5] + "|" //CARD5-AMOUNT
			lineResult += utils.Hex2string(strBlock[562:563]) + "|"           //CARD5-CURRENCY
			lineResult += utils.Hex2string(strBlock[563:564]) + "|"           //CARD5-CYCLE
			lineResult += utils.Hex2string(strBlock[564:566]) + "|"           //CARD5-CARDTYPE

			// CARD 6
			lineResult += utils.Hex2string(strBlock[566:574]) + "|"           //CARD6-EXPDATE
			lineResult += utils.Hex2string(strBlock[574:575]) + "|"           //CARD6-STATUS
			lineResult += utils.Hex2string(strBlock[575:583]) + "|"           //CARD6-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[583:587])[:7] + "|" //CARD6-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[587:589])[:3] + "|" //CARD6-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[589:592])[:5] + "|" //CARD6-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[592:595])[:5] + "|" //CARD6-AMOUNT
			lineResult += utils.Hex2string(strBlock[595:596]) + "|"           //CARD6-CURRENCY
			lineResult += utils.Hex2string(strBlock[596:597]) + "|"           //CARD6-CYCLE
			lineResult += utils.Hex2string(strBlock[597:599]) + "|"           //CARD6-CARDTYPE

			// CARD 7
			lineResult += utils.Hex2string(strBlock[599:607]) + "|"           //CARD7-EXPDATE
			lineResult += utils.Hex2string(strBlock[607:608]) + "|"           //CARD7-STATUS
			lineResult += utils.Hex2string(strBlock[608:616]) + "|"           //CARD7-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[616:620])[:7] + "|" //CARD7-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[620:622])[:3] + "|" //CARD7-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[622:625])[:5] + "|" //CARD7-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[625:628])[:5] + "|" //CARD7-AMOUNT
			lineResult += utils.Hex2string(strBlock[628:629]) + "|"           //CARD7-CURRENCY
			lineResult += utils.Hex2string(strBlock[629:630]) + "|"           //CARD7-CYCLE
			lineResult += utils.Hex2string(strBlock[630:632]) + "|"           //CARD7-CARDTYPE

			// CARD 8
			lineResult += utils.Hex2string(strBlock[632:640]) + "|"           //CARD8-EXPDATE
			lineResult += utils.Hex2string(strBlock[640:641]) + "|"           //CARD8-STATUS
			lineResult += utils.Hex2string(strBlock[641:649]) + "|"           //CARD8-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[649:653])[:7] + "|" //CARD8-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[653:655])[:3] + "|" //CARD8-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[655:658])[:5] + "|" //CARD8-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[658:661])[:5] + "|" //CARD8-AMOUNT
			lineResult += utils.Hex2string(strBlock[661:662]) + "|"           //CARD8-CURRENCY
			lineResult += utils.Hex2string(strBlock[662:663]) + "|"           //CARD8-CYCLE
			lineResult += utils.Hex2string(strBlock[663:665]) + "|"           //CARD8-CARDTYPE

			// CARD 9
			lineResult += utils.Hex2string(strBlock[665:673]) + "|"           //CARD9-EXPDATE
			lineResult += utils.Hex2string(strBlock[673:674]) + "|"           //CARD9-STATUS
			lineResult += utils.Hex2string(strBlock[674:682]) + "|"           //CARD9-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[682:686])[:7] + "|" //CARD9-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[686:688])[:3] + "|" //CARD9-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[688:691])[:5] + "|" //CARD9-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[691:694])[:5] + "|" //CARD9-AMOUNT
			lineResult += utils.Hex2string(strBlock[694:695]) + "|"           //CARD9-CURRENCY
			lineResult += utils.Hex2string(strBlock[695:696]) + "|"           //CARD9-CYCLE
			lineResult += utils.Hex2string(strBlock[696:698]) + "|"           //CARD9-CARDTYPE

			// CARD 10
			lineResult += utils.Hex2string(strBlock[698:706]) + "|"           //CARD10-EXPDATE
			lineResult += utils.Hex2string(strBlock[706:707]) + "|"           //CARD10-STATUS
			lineResult += utils.Hex2string(strBlock[707:715]) + "|"           //CARD10-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[715:719])[:7] + "|" //CARD10-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[719:721])[:3] + "|" //CARD10-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[721:724])[:5] + "|" //CARD10-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[724:727])[:5] + "|" //CARD10-AMOUNT
			lineResult += utils.Hex2string(strBlock[727:728]) + "|"           //CARD10-CURRENCY
			lineResult += utils.Hex2string(strBlock[728:729]) + "|"           //CARD10-CYCLE
			lineResult += utils.Hex2string(strBlock[729:731]) + "|"           //CARD10-CARDTYPE

			// CARD 11
			lineResult += utils.Hex2string(strBlock[731:739]) + "|"           //CARD11-EXPDATE
			lineResult += utils.Hex2string(strBlock[739:740]) + "|"           //CARD11-STATUS
			lineResult += utils.Hex2string(strBlock[740:748]) + "|"           //CARD11-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[748:752])[:7] + "|" //CARD11-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[752:754])[:3] + "|" //CARD11-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[754:757])[:5] + "|" //CARD11-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[757:760])[:5] + "|" //CARD11-AMOUNT
			lineResult += utils.Hex2string(strBlock[760:761]) + "|"           //CARD11-CURRENCY
			lineResult += utils.Hex2string(strBlock[761:762]) + "|"           //CARD11-CYCLE
			lineResult += utils.Hex2string(strBlock[762:764]) + "|"           //CARD11-CARDTYPE

			// CARD 12
			lineResult += utils.Hex2string(strBlock[764:772]) + "|"           //CARD12-EXPDATE
			lineResult += utils.Hex2string(strBlock[772:773]) + "|"           //CARD12-STATUS
			lineResult += utils.Hex2string(strBlock[773:781]) + "|"           //CARD12-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[781:785])[:7] + "|" //CARD12-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[785:787])[:3] + "|" //CARD12-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[787:790])[:5] + "|" //CARD12-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[790:793])[:5] + "|" //CARD12-AMOUNT
			lineResult += utils.Hex2string(strBlock[793:794]) + "|"           //CARD12-CURRENCY
			lineResult += utils.Hex2string(strBlock[794:795]) + "|"           //CARD12-CYCLE
			lineResult += utils.Hex2string(strBlock[795:797]) + "|"           //CARD12-CARDTYPE

			// CARD 13
			lineResult += utils.Hex2string(strBlock[797:805]) + "|"           //CARD13-EXPDATE
			lineResult += utils.Hex2string(strBlock[805:806]) + "|"           //CARD13-STATUS
			lineResult += utils.Hex2string(strBlock[806:814]) + "|"           //CARD13-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[814:818])[:7] + "|" //CARD13-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[818:820])[:3] + "|" //CARD13-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[820:823])[:5] + "|" //CARD13-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[823:826])[:5] + "|" //CARD13-AMOUNT
			lineResult += utils.Hex2string(strBlock[826:827]) + "|"           //CARD13-CURRENCY
			lineResult += utils.Hex2string(strBlock[827:828]) + "|"           //CARD13-CYCLE
			lineResult += utils.Hex2string(strBlock[828:830]) + "|"           //CARD13-CARDTYPE

			// CARD 14
			lineResult += utils.Hex2string(strBlock[830:838]) + "|"           //CARD14-EXPDATE
			lineResult += utils.Hex2string(strBlock[838:839]) + "|"           //CARD14-STATUS
			lineResult += utils.Hex2string(strBlock[839:847]) + "|"           //CARD14-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[847:851])[:7] + "|" //CARD14-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[851:853])[:3] + "|" //CARD14-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[853:856])[:5] + "|" //CARD14-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[856:859])[:5] + "|" //CARD14-AMOUNT
			lineResult += utils.Hex2string(strBlock[859:860]) + "|"           //CARD14-CURRENCY
			lineResult += utils.Hex2string(strBlock[860:861]) + "|"           //CARD14-CYCLE
			lineResult += utils.Hex2string(strBlock[861:863]) + "|"           //CARD14-CARDTYPE

			// CARD 15
			lineResult += utils.Hex2string(strBlock[863:871]) + "|"           //CARD15-EXPDATE
			lineResult += utils.Hex2string(strBlock[871:872]) + "|"           //CARD15-STATUS
			lineResult += utils.Hex2string(strBlock[872:880]) + "|"           //CARD15-LASTUSE
			lineResult += utils.Hex2string_comp3(strBlock[880:884])[:7] + "|" //CARD15-USEDLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[884:886])[:3] + "|" //CARD15-TXLIMIT
			lineResult += utils.Hex2string_comp3(strBlock[886:889])[:5] + "|" //CARD15-TOTALTX
			lineResult += utils.Hex2string_comp3(strBlock[889:892])[:5] + "|" //CARD15-AMOUNT
			lineResult += utils.Hex2string(strBlock[892:893]) + "|"           //CARD15-CURRENCY
			lineResult += utils.Hex2string(strBlock[893:894]) + "|"           //CARD15-CYCLE
			lineResult += utils.Hex2string(strBlock[894:896]) + "|"           //CARD15-CARDTYPE

			// MMD-GENERAL-DATA
			lineResult += utils.Hex2string_comp3(strBlock[896:899])[:5] + "|" // MMD-VISA-CATEGORY S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[899:902])[:5] + "|" // MMD-MC-CATEGORY S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[902:905])[:5] + "|" // MMD-PL-CATEGORY S9(5)

			// VISA DATA - CURRENT WEEK TO DATE
			lineResult += utils.Hex2string_comp3(strBlock[911:916])[:9] + "|" // MMD-CWTD-GROSS-SALES-AMT S9(7)V99
			lineResult += utils.Hex2string_comp3(strBlock[916:920])[:7] + "|" // MMD-CWTD-AVG-TRAN-AMT S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[920:924])[:7] + "|" // MMD-CWTD-NBR-DRAFTS S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[924:928])[:7] + "|" // MMD-CWTD-NBR-CHGBKS S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[928:930])[:3] + "|" // MMD-CWTD-ELP-TOTAL-DAYS S9(3)

			// LAST MONTH TOTALS
			lineResult += utils.Hex2string_comp3(strBlock[930:935])[:9] + "|" // MMD-LMF-GROSS-SALES-AMT S9(7)V99
			lineResult += utils.Hex2string_comp3(strBlock[935:939])[:7] + "|" // MMD-LMF-AVG-TRAN-AMT S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[939:943])[:7] + "|" // MMD-LMF-NBR-DRAFTS S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[943:947])[:7] + "|" // MMD-LMF-NBR-CHGBKS S9(7)

			lineResult += utils.Hex2string_comp3(strBlock[947:951])[:7] + "|" // MMD-MTD-NBR-CHGBKS S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[951:955])[:7] + "|" // MMD-DATE-LAST-INTERROGATED S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[955:959])[:7] + "|" // MMD-FIRST-POST-DATE S9(7)

			// Monitoring and Visa Flags
			lineResult += utils.Hex2string(strBlock[959:960]) + "|" // MMD-MONITORING-INDICATOR PIC 9
			lineResult += utils.Hex2string(strBlock[960:961]) + "|" // MMD-VISA-INTCHG-FLAG PIC X
			lineResult += utils.Hex2string(strBlock[961:963]) + "|" // MMD-VISA-AUTH-INQUIRY-TC PIC X2
			lineResult += utils.Hex2string(strBlock[963:964]) + "|" // MMD-VISA-SPECL-COND-1 PIC X
			lineResult += utils.Hex2string(strBlock[964:965]) + "|" // MMD-VISA-SPECL-COND-2 PIC X
			lineResult += utils.Hex2string(strBlock[965:966]) + "|" // MMD-VISA-MAIL-PHONE-IND PIC X
			lineResult += utils.Hex2string(strBlock[966:969]) + "|" // MMD-MONTH-TO-DATE-FIELDS PIC X3
			lineResult += utils.Hex2string(strBlock[969:970]) + "|" // MMD-POS-CAP PIC X
			lineResult += utils.Hex2string(strBlock[970:972]) + "|" // MMD-POS-MODE PIC X2
			lineResult += utils.Hex2string(strBlock[972:973]) + "|" // MMD-AUTH-SOURCE PIC X
			lineResult += utils.Hex2string(strBlock[973:974]) + "|" //

			// MASTERCARD DATA
			lineResult += utils.Hex2string(strBlock[974:975]) + "|" // MMD-MC-INTCHG-FLAG PIC X
			lineResult += utils.Hex2string(strBlock[975:976]) + "|" // MMD-MC-AUTH-INQUIRY-TC PIC X
			lineResult += utils.Hex2string(strBlock[976:977]) + "|" // MMD-MC-EPSS-INTCHG-FLAG PIC 9

			// PRIVATE LABEL
			lineResult += utils.Hex2string(strBlock[977:993]) + "|"             // MMD-CARD-DESCR PIC X(16)
			lineResult += utils.Hex2string(strBlock[993:998]) + "|"             // MMD-CARD-DESCR-SHORT PIC X5
			lineResult += utils.Hex2string_comp3(strBlock[998:1001])[:5] + "|"  // MMD-CARD-ASSESS S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1001:1005])[:7] + "|" // MMD-CARD-BULL-COST S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[1005:1008])[:5] + "|" // MMD-CARD-ON-US-RATE S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1008:1012])[:7] + "|" // MMD-CARD-ON-US-PER-ITEM S9(2)V9(5)

			// *2
			lineResult += utils.Hex2string(strBlock[1012:1028]) + "|"           // MMD-CARD-DESCR PIC X(16)
			lineResult += utils.Hex2string(strBlock[1028:1033]) + "|"           // MMD-CARD-DESCR-SHORT PIC X5
			lineResult += utils.Hex2string_comp3(strBlock[1033:1036])[:5] + "|" // MMD-CARD-ASSESS S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1036:1040])[:7] + "|" // MMD-CARD-BULL-COST S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[1040:1043])[:5] + "|" // MMD-CARD-ON-US-RATE S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1043:1047])[:7] + "|" // MMD-CARD-ON-US-PER-ITEM S9(2)V9(5)

			// *3
			lineResult += utils.Hex2string(strBlock[1047:1063]) + "|"           // MMD-CARD-DESCR PIC X(16)
			lineResult += utils.Hex2string(strBlock[1063:1068]) + "|"           // MMD-CARD-DESCR-SHORT PIC X5
			lineResult += utils.Hex2string_comp3(strBlock[1068:1071])[:5] + "|" // MMD-CARD-ASSESS S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1071:1075])[:7] + "|" // MMD-CARD-BULL-COST S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[1075:1078])[:5] + "|" // MMD-CARD-ON-US-RATE S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1078:1082])[:7] + "|" // MMD-CARD-ON-US-PER-ITEM S9(2)V9(5)

			// *4
			lineResult += utils.Hex2string(strBlock[1082:1098]) + "|"           // MMD-CARD-DESCR PIC X(16)
			lineResult += utils.Hex2string(strBlock[1098:1103]) + "|"           // MMD-CARD-DESCR-SHORT PIC X5
			lineResult += utils.Hex2string_comp3(strBlock[1103:1106])[:5] + "|" // MMD-CARD-ASSESS S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1106:1110])[:7] + "|" // MMD-CARD-BULL-COST S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[1110:1113])[:5] + "|" // MMD-CARD-ON-US-RATE S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1113:1117])[:7] + "|" // MMD-CARD-ON-US-PER-ITEM S9(2)V9(5)

			// *5
			lineResult += utils.Hex2string(strBlock[1117:1133]) + "|"           // MMD-CARD-DESCR PIC X(16)
			lineResult += utils.Hex2string(strBlock[1133:1138]) + "|"           // MMD-CARD-DESCR-SHORT PIC X5
			lineResult += utils.Hex2string_comp3(strBlock[1138:1141])[:5] + "|" // MMD-CARD-ASSESS S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1141:1145])[:7] + "|" // MMD-CARD-BULL-COST S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[1145:1148])[:5] + "|" // MMD-CARD-ON-US-RATE S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1148:1152])[:7] + "|" // MMD-CARD-ON-US-PER-ITEM S9(2)V9(5)

			// *6
			lineResult += utils.Hex2string(strBlock[1152:1168]) + "|"           // MMD-CARD-DESCR PIC X(16)
			lineResult += utils.Hex2string(strBlock[1168:1173]) + "|"           // MMD-CARD-DESCR-SHORT PIC X5
			lineResult += utils.Hex2string_comp3(strBlock[1173:1176])[:5] + "|" // MMD-CARD-ASSESS S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1176:1180])[:7] + "|" // MMD-CARD-BULL-COST S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[1180:1183])[:5] + "|" // MMD-CARD-ON-US-RATE S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1183:1187])[:7] + "|" // MMD-CARD-ON-US-PER-ITEM S9(2)V9(5)

			// CHARGEBACK AND FEES
			lineResult += utils.Hex2string(strBlock[1207:1208]) + "|"            // MMD-CHG-ADMIN       PIC 9
			lineResult += utils.Hex2string(strBlock[1208:1209]) + "|"            // MMD-MIN-DISC-CHG    PIC 9
			lineResult += utils.Hex2string(strBlock[1209:1210]) + "|"            // MMD-FEE-ON-CHGBK    PIC 9
			lineResult += utils.Hex2string(strBlock[1210:1211]) + "|"            // MMD-MISC3-FEE-CHG   PIC 9
			lineResult += utils.Hex2string(strBlock[1211:1212]) + "|"            // MMD-PER-ITEM-CHG    PIC 9
			lineResult += utils.Hex2string(strBlock[1212:1213]) + "|"            // MMD-STMT-FEE-CHG    PIC 9
			lineResult += utils.Hex2string(strBlock[1213:1214]) + "|"            // MMD-FEE-ON-QUAL-REJ    PIC 9
			lineResult += utils.Hex2string(strBlock[1214:1215]) + "|"            // MMD-CHECK-APPROVAL-FEE   PIC 9
			lineResult += utils.Hex2string_comp3(strBlock[1215:1219])[:7] + "|"  // MMD-ADMIN-FEE       PIC S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1219:1223])[:7] + "|"  // MMD-CHGBK-FEE       PIC S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1223:1227])[:7] + "|"  // MMD-QUAL-REJ-FEE    PIC S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1227:1230])[:5] + "|"  // MMD-MIN-DISC-AMT    PIC S999V99
			lineResult += utils.Hex2string_comp3(strBlock[1230:1234])[:7] + "|"  // MMD-MISC3-FEE       PIC S99V9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1234:1237])[:5] + "|"  // MMD-STMT-FEE        PIC S999V99
			lineResult += utils.Hex2string(strBlock[1237:1239]) + "|"            // MMD-MEMB-FEE-FREQ   PIC 99
			lineResult += utils.Hex2string(strBlock[1239:1240]) + "|"            // MMD-TYPE-OF-STMT    PIC 9
			lineResult += utils.Hex2string(strBlock[1240:1241]) + "|"            // MMD-EXC-CHGBK-MTH-ORG-MNT   PIC 9
			lineResult += utils.Hex2string(strBlock[1241:1242]) + "|"            // MMD-EXC-QUAL-REJ-MTH-ORG-MNT   PIC 9
			lineResult += utils.Hex2string_comp3(strBlock[1242:1244])[:3] + "|"  // MMD-EXC-CHGBK-MTH       PIC S999
			lineResult += utils.Hex2string_comp3(strBlock[1244:1246])[:3] + "|"  // MMD-EXC-QUAL-REJ-MTH        PIC S999
			lineResult += utils.Hex2string_comp3(strBlock[1246:1250])[:7] + "|"  // MMD-NBR-CHGBACKS-CTD        PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[1250:1256])[:11] + "|" // MMD-AMT-CHGBACKS-CTD       PIC S9(9)V99
			lineResult += utils.Hex2string_comp3(strBlock[1256:1260])[:7] + "|"  // MMD-NBR-CHK-APPR-CTD        PIC S9(7)
			lineResult += utils.Hex2string_comp3(strBlock[1260:1265])[:9] + "|"  // MMD-AMT-CHK-APPR-CHGS-CTD        PIC S9(5)V9(4)
			lineResult += utils.Hex2string_comp3(strBlock[1265:1268])[:5] + "|"  // MMD-PROJ-AVG-TKT       PIC S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1268:1273])[:9] + "|"  // MMD-PROJ-MTH-VOLUME PIC S9(9)
			lineResult += utils.Hex2string_comp3(strBlock[1273:1276])[:5] + "|"  // MMD-AVG-TKT-RANGES1       PIC S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1276:1279])[:5] + "|"  // MMD-AVG-TKT-RANGES2       PIC S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1279:1282])[:5] + "|"  // MMD-AVG-TKT-RANGES3       PIC S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1282:1285])[:5] + "|"  // MMD-AVG-TKT-RANGES4       PIC S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1285:1288])[:5] + "|"  // MMD-AVG-TKT-RANGES5       PIC S9(5)
			lineResult += utils.Hex2string_comp3(strBlock[1288:1292])[:7] + "|"  // MMD-PL-TOT-ON-US-RATE   PIC S9(5)V99
			lineResult += utils.Hex2string_comp3(strBlock[1292:1296])[:7] + "|"  // MMD-PL-TOT-ON-US-ITEM   PIC S9(5)V99

			lineResult += activityData(strBlock)
			lineResult += edData(strBlock)

			lineResult += utils.Hex2string(strBlock[3840:3870]) + "|" //MMD-USER-DATA       PIC X(30)
			lineResult += utils.Hex2string(strBlock[3870:3889]) + "|" //MMD-USER-ACCT       PIC X(19)
			lineResult += utils.Hex2string(strBlock[3889:3989]) + "|" //MMD-USER-FILLER     PIC X(100)
			lineResult += utils.Hex2string(strBlock[3989:4029]) + "|" //MMD-EMAIL-ADDRESS   PIC X(40)
			// lineResult += utils.Hex2string(strBlock[4029:4031]) + "|"//MMD-PAY-TERM   PIC 9(2)
			// lineResult += utils.Hex2string(strBlock[4031:4034]) + "|"//MMD-FEE-PROG-IND   PIC X(3)
			lineResult += onUsTable(strBlock)                         //MMD-CARD-ONUS-TABLE.
			lineResult += utils.Hex2string(strBlock[4438:4444]) + "|" //MMD-MC-ASSIGNED-ID  PIC X(06)
			lineResult += utils.Hex2string(strBlock[4444:4454]) + "|" //MMD-VERIFICATION    PIC X(10)
			lineResult += utils.Hex2string(strBlock[4454:4455]) + "|" //MMD-INSTL-ALLOWED   PIC X(01)
			lineResult += utils.Hex2string(strBlock[4455:4464]) + "|" //MMD-INSTL-SPL-FROM  PIC 9(9)
			lineResult += utils.Hex2string(strBlock[4464:4473]) + "|" //MMD-INSTL-SPL-TO    PIC 9(9)
			lineResult += utils.Hex2string(strBlock[4473:4476]) + "|" //MMD-INSTL-PLAN1     PIC 9(3)
			lineResult += utils.Hex2string(strBlock[4476:4479]) + "|" //MMD-INSTL-PLAN2     PIC 9(3)
			lineResult += utils.Hex2string(strBlock[4479:4482]) + "|" //MMD-INSTL-PLAN3     PIC 9(3)
			lineResult += utils.Hex2string(strBlock[4482:4497]) + "|" //MMD-MERCH-MEMO      PIC X(15)
			lineResult += utils.Hex2string(strBlock[4497:4498]) + "|" //MMD-EOC-FLAG        PIC X
			lineResult += utils.Hex2string(strBlock[4498:4499]) + "|" //MMD-MAINT-STATUS    PIC X
			lineResult += utils.Hex2string(strBlock[4499:4500]) + ""  //MMD-STATUS-FLAG     PIC X

			// lineResult += utils.Hex2string_comp3(strBlock[908:911])[:5] + "|" // MMD-JC-CATEGORY S9(5)

			lineResult += "\r\n"
		}

		return lineResult
	})

}

// unUsTable processes the ON-US table from the MMD data block.
func onUsTable(strBlock string) string {
	lineResult := ""

	if len(strBlock) < 4056 {
		return lineResult // Not enough data to process
	}

	for i := 0; i < 8; i++ {
		offset := 4056 + i*7
		lineResult += utils.Hex2string_comp3(strBlock[offset : offset+3])[:5] + "|"   // MMD-CARD-ONUS-RATE        PIC SV9(5)     COMP-3
		lineResult += utils.Hex2string_comp3(strBlock[offset+3 : offset+7])[:7] + "|" // MMD-CARD-ONUS-PER-ITEM    PIC S99V9(5)   COMP-3
	}

	return lineResult
}

// edData processes the ED data from the MMD data block.
func edData(strBlock string) string {
	lineResult := ""

	if len(strBlock) < 3580 {
		return lineResult // Not enough data to process
	}

	for h := 0; h < 4; h++ {
		base := 3580 + h*65

		lineResult += utils.Hex2string(strBlock[base:base+40]) + "|"                 // MMD-ED-NAME         PIC X(40)
		lineResult += utils.Hex2string(strBlock[base+40:base+41]) + "|"              // MMD-ED-ACTION       PIC 9(01)
		lineResult += utils.Hex2string(strBlock[base+41:base+43]) + "|"              // MMD-ED-NBR-REQ      PIC 9(2)
		lineResult += utils.Hex2string(strBlock[base+43:base+45]) + "|"              // MMD-ED-TYPE         PIC 9(2)
		lineResult += utils.Hex2string(strBlock[base+45:base+47]) + "|"              // MMD-ED-NBR-OUT      PIC 9(2)
		lineResult += utils.Hex2string_comp3(strBlock[base+47 : base+56])[:17] + "|" // MMD-ED-DDA          PIC S9(17)
		lineResult += utils.Hex2string_comp3(strBlock[base+56 : base+63])[:13] + "|" // MMD-ED-USER-NBR     PIC S9(13)
		lineResult += utils.Hex2string(strBlock[base+63:base+65]) + "|"              // MMD-ED-USER-FIELD   PIC X(2)
	}

	return lineResult
}

// activityData processes the activity data from the MMD data block.
func activityData(strBlock string) string {
	lineResult := ""

	if len(strBlock) < 1800 {
		return lineResult // Not enough data to process
	}

	for j := 0; j < 4; j++ {
		actvt := j * (74*5 + 156 + 45)
		// *MMD-ACTIVITY *1
		// AMEX X(74)
		// VISA ACTIVITY
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1370 : actvt+1374])[:7] + "|"  // MMD-VI-NBR-OUTGO-SALES   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1374 : actvt+1380])[:11] + "|" // MMD-VI-AMT-OUTGO-SALES   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1380 : actvt+1384])[:7] + "|"  // MMD-VI-NBR-OUTGO-RETURNS PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1384 : actvt+1390])[:11] + "|" // MMD-VI-AMT-OUTGO-RETURNS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1390 : actvt+1394])[:7] + "|"  // MMD-VI-NBR-NONQUAL-SALES PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1394 : actvt+1400])[:11] + "|" // MMD-VI-AMT-NONQUAL-SALES PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1400 : actvt+1404])[:7] + "|"  // MMD-VI-NBR-NONQUAL-RETURNS PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1404 : actvt+1410])[:11] + "|" // MMD-VI-AMT-NONQUAL-RETURNS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1410 : actvt+1414])[:7] + "|"  // MMD-VI-NBR-TOTAL-SALES PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1414 : actvt+1420])[:11] + "|" // MMD-VI-AMT-TOTAL-SALES PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1420 : actvt+1424])[:7] + "|"  // MMD-VI-NBR-TOTAL-RETURNS PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1424 : actvt+1430])[:11] + "|" // MMD-VI-AMT-TOTAL-RETURNS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1430 : actvt+1436])[:11] + "|" // MMD-VI-AMT-TOTAL-PER-TRANS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1436 : actvt+1441])[:9] + "|"  // MMD-VI-AMT-DISC PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1441 : actvt+1444])[:5] + "|"  // MMD-VI-EFFECT-RATE PIC S9(5)

		// MASTERCARD ACTIVITY
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1444 : actvt+1448])[:7] + "|"  // MMD-MC-NBR-OUTGO-SALES   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1448 : actvt+1454])[:11] + "|" // MMD-MC-AMT-OUTGO-SALES   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1454 : actvt+1458])[:7] + "|"  // MMD-MC-NBR-OUTGO-RETURNS PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1458 : actvt+1464])[:11] + "|" // MMD-MC-AMT-OUTGO-RETURNS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1464 : actvt+1468])[:7] + "|"  // MMD-MC-NBR-NONQUAL-SALES PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1468 : actvt+1474])[:11] + "|" // MMD-MC-AMT-NONQUAL-SALES PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1474 : actvt+1478])[:7] + "|"  // MMD-MC-NBR-NONQUAL-RETURNS PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1478 : actvt+1484])[:11] + "|" // MMD-MC-AMT-NONQUAL-RETURNS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1484 : actvt+1488])[:7] + "|"  // MMD-MC-NBR-TOTAL-SALES   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1488 : actvt+1494])[:11] + "|" // MMD-MC-AMT-TOTAL-SALES   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1494 : actvt+1498])[:7] + "|"  // MMD-MC-NBR-TOTAL-RETURNS PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1498 : actvt+1504])[:11] + "|" // MMD-MC-AMT-TOTAL-RETURNS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1504 : actvt+1510])[:11] + "|" // MMD-MC-AMT-TOTAL-PER-TRANS PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1510 : actvt+1515])[:9] + "|"  // MMD-MC-AMT-DISC PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1515 : actvt+1518])[:5] + "|"  // MMD-MC-EFFECT-RATE PIC S9(5)

		// PRIVATE LABEL ACTIVITY
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1518 : actvt+1522])[:7] + "|"  // MMD-PL-NBR-OUTGO-SALES   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1522 : actvt+1528])[:11] + "|" // MMD-PL-AMT-OUTGO-SALES   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1528 : actvt+1532])[:7] + "|"  // MMD-PL-NBR-OUTGO-RETURNS  PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1532 : actvt+1538])[:11] + "|" // MMD-PL-AMT-OUTGO-RETURNS   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1538 : actvt+1542])[:7] + "|"  // MMD-PL-NBR-NONQUAL-SALES  PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1542 : actvt+1548])[:11] + "|" // MMD-PL-AMT-NONQUAL-SALES   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1548 : actvt+1552])[:7] + "|"  // MMD-PL-NBR-NONQUAL-RETURNS  PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1552 : actvt+1558])[:11] + "|" // MMD-PL-AMT-NONQUAL-RETURNS   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1558 : actvt+1562])[:7] + "|"  // MMD-PL-NBR-TOTAL-SALES   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1562 : actvt+1568])[:11] + "|" // MMD-PL-AMT-TOTAL-SALES   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1568 : actvt+1572])[:7] + "|"  // MMD-PL-NBR-TOTAL-RETURNS   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1572 : actvt+1578])[:11] + "|" // MMD-PL-AMT-TOTAL-RETURNS    PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1578 : actvt+1584])[:11] + "|" // MMD-PL-AMT-TOTAL-PER-TRANS   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1584 : actvt+1589])[:9] + "|"  // MMD-PL-AMT-DISC  PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1589 : actvt+1592])[:5] + "|"  // MMD-PL-EFFECT-RATE   PIC S9(5)

		//TOTAL ACTIVITY
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1666 : actvt+1671])[:9] + "|"  // MMD-AMT-FEES   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1671 : actvt+1676])[:9] + "|"  // MMD-AMT-DISC   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1676 : actvt+1681])[:9] + "|"  // MMD-AMT-ORG-EXPENSE  PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1681 : actvt+1684])[:5] + "|"  // MMD-EFFECT-RATE   PIC SV9(5)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1684 : actvt+1686])[:3] + "|"  // MMD-NBR-OF-DEP-DAYS  PIC S999
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1686 : actvt+1690])[:7] + "|"  // MMD-NBR-POS1-AUTH   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1690 : actvt+1694])[:7] + "|"  // MMD-NBR-POS2-AUTH   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1694 : actvt+1698])[:7] + "|"  // MMD-NBR-POS3-AUTH   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1698 : actvt+1702])[:7] + "|"  // MMD-NBR-VOICE-AUTH  PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1702 : actvt+1706])[:7] + "|"  // MMD-TOT-NBR-DEP   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1706 : actvt+1710])[:7] + "|"  // MMD-TOT-NBR-DRAFTS  PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1710 : actvt+1714])[:7] + "|"  // MMD-NBR-POS1-DRAFTS    PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1714 : actvt+1718])[:7] + "|"  // MMD-NBR-POS2-DRAFTS  PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1718 : actvt+1722])[:7] + "|"  // MMD-NBR-POS3-DRAFTS  PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1722 : actvt+1726])[:7] + "|"  // MMD-NBR-TAPE-DRAFTS   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1726 : actvt+1730])[:7] + "|"  // MMD-NBR-KEYED-DEP   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1730 : actvt+1734])[:7] + "|"  // MMD-NBR-KEYED-DRAFTS   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1734 : actvt+1738])[:7] + "|"  // MMD-NBR-QUAL-REJ   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1738 : actvt+1744])[:11] + "|" // MMD-AMT-QUAL-REJ   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1744 : actvt+1748])[:7] + "|"  // MMD-NBR-CHGBACKS   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1748 : actvt+1754])[:11] + "|" // MMD-AMT-CHGBACKS   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1754 : actvt+1758])[:7] + "|"  // MMD-NBR-CHK-APPR   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1758 : actvt+1763])[:9] + "|"  // MMD-AMT-CHK-APPR-CHGS   PIC S9(5)V9(4)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1763 : actvt+1768])[:9] + "|"  // MMD-AMT-MEMB-FEE   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1768 : actvt+1773])[:9] + "|"  // MMD-AMT-IMP1-FEE   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1773 : actvt+1778])[:9] + "|"  // MMD-AMT-IMP2-FEE   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1778 : actvt+1783])[:9] + "|"  // MMD-AMT-IMP3-FEE   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1783 : actvt+1788])[:9] + "|"  // MMD-AMT-POS1-FEE   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1788 : actvt+1793])[:9] + "|"  // MMD-AMT-POS2-FEE   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1793 : actvt+1798])[:9] + "|"  // MMD-AMT-POS3-FEE    PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1798 : actvt+1802])[:7] + "|"  // MMD-NBR-DISC-ADJ   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1802 : actvt+1807])[:9] + "|"  // MMD-AMT-DISC-ADJ   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1807 : actvt+1811])[:7] + "|"  // MMD-NBR-FEE-ADJ   PIC S9(7)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1811 : actvt+1822])[:9] + "|"  // MMD-AMT-FEE-ADJ   PIC S9(7)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1816 : actvt+1822])[:11] + "|" // MMD-AMT-PER-TRANS   PIC S9(9)V99
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1822 : actvt+1826])[:7] + "|"  // MMD-SLSA-NBR   PIC S9(7)1
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1826 : actvt+1831])[:9] + "|"  // MMD-SLSA-AMT    PIC S9(9)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1831 : actvt+1835])[:7] + "|"  // MMD-SLSA-NBR   PIC S9(7)2
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1835 : actvt+1840])[:9] + "|"  // MMD-SLSA-AMT    PIC S9(9)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1840 : actvt+1844])[:7] + "|"  // MMD-SLSA-NBR   PIC S9(7)3
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1844 : actvt+1849])[:9] + "|"  // MMD-SLSA-AMT    PIC S9(9)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1849 : actvt+1853])[:7] + "|"  // MMD-SLSA-NBR   PIC S9(7)4
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1853 : actvt+1858])[:9] + "|"  // MMD-SLSA-AMT    PIC S9(9)
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1858 : actvt+1862])[:7] + "|"  // MMD-SLSA-NBR   PIC S9(7)5
		lineResult += utils.Hex2string_comp3(strBlock[actvt+1862 : actvt+1867])[:9] + "|"  // MMD-SLSA-AMT    PIC S9(9)
	}
	return lineResult
}
