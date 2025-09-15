package main

import (
	"sync"
	"time"

	"github.com/spf13/viper"

	"github.com/wirsal/ebcdic-converter/pkg/config"
	"github.com/wirsal/ebcdic-converter/pkg/converter/cedc"
	"github.com/wirsal/ebcdic-converter/pkg/converter/cepp"
	"github.com/wirsal/ebcdic-converter/pkg/converter/cia"
)

var (
	waitSecond   int
	waitDuration time.Duration
)

func main() {
	config.InitConfig()
	config.PrintWelcome()

	waitSecond = viper.GetInt("server.waitDuration")
	waitDuration = time.Duration(waitSecond) * time.Second

	var wg sync.WaitGroup
	processes := []func(){
		// CIA
		func() { runProcessor("file.CPCRD.fileCPCRD", cia.Cpcrd2text) },
		func() { runProcessor("file.CPCUS.fileCPCUS1", cia.Cpcus2text) },
		func() { runProcessor("file.CPCUS.fileCPCUS2", cia.Cpcus2text) },
		func() { runProcessor("file.CPCRA.fileCPCRA", cia.Cpcra2text) },
		func() { runProcessor("file.CPMER.fileCPMER", cia.Cpmer2text) },
		func() { runProcessor("file.CPS130.fileCPOLST", cia.Cpols2text) },
		func() { runProcessor("file.CPS130.fileCPOLPT", cia.Cpols2text) },

		func() { runProcessor("file.CPS120.fileCXCHA", cia.Cps1202text) },
		func() { runProcessor("file.CPS120.fileCXCHU", cia.Cps1202text) },
		func() { runProcessor("file.CPAHF.fileCPAHF", cia.Cpahf2text) },
		func() { runProcessor("file.CPS123.fileCPLMT", cia.Cps123input2text) },
		func() { runProcessor("file.CPSMTF.fileCPSMTF", cia.Cpsmtf2text) },

		func() { runProcessor("file.CPS110.fileMIGS", cia.Cps110input2txt) },
		func() { runProcessor("file.CPS110.fileUI33", cia.Cps110input2txt) },
		func() { runProcessor("file.CPS110.fileADAX", cia.Cps110input2txt) },
		func() { runProcessor("file.CPS110.fileTOCP1", cia.Cps110input2txt) },
		// func() { runProcessor("file.CPS110.fileTOCP2", cia.Cps110input2txt) },

		//CEPP
		func() { runProcessor("file.EPTRANS.fileEPTRANS", cepp.Eptrans2text) },

		//CEDC
		func() { runProcessor("file.OADCTF.fileOADCTF", cedc.Oadctf2text) },
		func() { runProcessor("file.OADCLG.fileOADCLGB", cedc.Oadclog2txt) },

		//combine results
		func() {
			runCombineResult("file.CPCUS.fileCPCUS1", "file.CPCUS.fileCPCUS2", "file.CPCUS.fileCPCUS", cia.CombineCpcus)
		},
	}

	wg.Add(len(processes))

	for _, process := range processes {
		go func(p func()) {
			defer wg.Done()
			p()
		}(process)
	}

	wg.Wait()
}
