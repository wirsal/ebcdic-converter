package config

import (
	"fmt"
	"strings"
	"time"

	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

func PrintWelcome() {
	cfg := map[string]interface{}{
		"App Name       ": viper.GetString("server.appName"),
		"Version        ": viper.GetString("server.version"),
		"Record Limit   ": fmt.Sprintf("%d baris per batch", viper.GetInt("server.batchSize")),
		"Wait Duration  ": fmt.Sprintf("%d detik jika file belum tersedia", viper.GetInt("server.waitDuration")),
		"Start Time     ": time.Now().Format("2006-01-02 15:04:05"),
	}

	fmt.Println(strings.Repeat("=", 91))
	for k, v := range cfg {
		fmt.Printf("===  %-14s : %v\n", k, v)
	}
	fmt.Println(strings.Repeat("=", 91))

	utils.Info("====================== Application started =======================")
}
