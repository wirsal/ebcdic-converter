package main

import (
	"time"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func runCombineResult(file1, file2, outputFile string, processor func(string, string, string) bool) {
	for {
		fileName1 := viper.GetString(file1)
		fileName2 := viper.GetString(file2)
		fileOutput := viper.GetString(outputFile)

		utils.WaitForFileInDir(utils.GetCurrentResultDir(), fileName1, waitDuration)
		utils.WaitForFileInDir(utils.GetCurrentResultDir(), fileName2, waitDuration)

		if processor(fileName1, fileName2, fileOutput) {
			utils.DeleteResultFile(utils.GetCurrentResultDir(), fileName1)
			utils.DeleteResultFile(utils.GetCurrentResultDir(), fileName2)
			utils.WaitUntilNextDay(fileOutput)
		}
		time.Sleep(waitDuration)

	}
}
