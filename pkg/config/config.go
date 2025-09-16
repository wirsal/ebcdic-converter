package config

import (
	"os"
	"strings"

	"github.com/fsnotify/fsnotify"
	"github.com/spf13/viper"
	"github.com/wirsal/ebcdic-converter/pkg/utils"
)

// InitConfig inisialisasi konfigurasi aplikasi menggunakan viper.
// Config dicari di folder ./config dan root project dengan nama ".dev.config.yaml"
func InitConfig() {
	viper.AddConfigPath("./configs")
	viper.AddConfigPath(".")
	viper.SetConfigType("yaml")

	envr := strings.ToLower(os.Getenv("ENVIRONMENT"))

	switch envr {
	case "production", "prod":
		viper.SetConfigName(".prod.config")
	case "testing", "test", "development", "dev":
		viper.SetConfigName(".dev.config")
	default:
		viper.SetConfigName(".config")
	}

	// Baca config
	if err := viper.ReadInConfig(); err != nil {
		utils.Error("⚠️ Failed to read config: %v", err)
		if envr == "production" {
			os.Exit(1)
		}
	}

	// Watch perubahan file config
	viper.WatchConfig()
	viper.OnConfigChange(func(e fsnotify.Event) {
		utils.Debug("♻️ Config file has changed: %s", e.Name)
	})
}
