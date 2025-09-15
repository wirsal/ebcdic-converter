package utils

import (
	"path/filepath"
	"time"

	"github.com/spf13/viper"
)

func GetCurrentDataDir() string {
	return filepath.Join(viper.GetString("file.baseData"), getYesterdayYYMMDD())
}

func GetCurrentBackupDir() string {
	return filepath.Join(viper.GetString("file.baseBackup"), getYesterdayYYMMDD())
}

func GetCurrentResultDir() string {
	return filepath.Join(viper.GetString("file.baseResult"), getYesterdayYYMMDD())
}

func GetCurrentLoggingDir() string {
	return filepath.Join(viper.GetString("file.baseLog"), getYesterdayYYMMDD())
}

func getYesterdayYYMMDD() string {
	yesterday := time.Now().AddDate(0, 0, -1)
	return yesterday.Format("060102") // YYMMDD
}
