package main

import (
	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func runProcessor(configKey string, processor func(string) bool) {
	for {

		filePath := viper.GetString(configKey)
		utils.WaitForFileInDir(utils.GetCurrentDataDir(), filePath, waitDuration)
		utils.DeleteResultFile(utils.GetCurrentResultDir(), filePath)

		if processor(filePath) {
			err := utils.MoveFile(filePath)
			if err != nil {
				utils.Error("Gagal memindahkan file", err)
			}
			utils.Info("%s", filePath+" File berhasil diproses dan dipindahkan.")
		}
		utils.WaitUntilNextDay(filePath)
	}
}
