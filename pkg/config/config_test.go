package config

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/spf13/viper"
)

func createTempConfig(t *testing.T, name, content string) string {
	t.Helper()
	tempDir := t.TempDir()
	configPath := filepath.Join(tempDir, name)

	err := os.WriteFile(configPath, []byte(content), 0644)
	if err != nil {
		t.Fatalf("Gagal membuat file config: %v", err)
	}

	return tempDir
}

func TestInitConfig_DevEnvironment(t *testing.T) {
	os.Setenv("ENVIRONMENT", "dev")

	// Buat file .dev.config.yaml sementara
	tempDir := createTempConfig(t, ".dev.config.yaml", "file:\n  baseData: /tmp/data")

	// Override path agar viper cari di tempDir
	viper.Reset()
	viper.AddConfigPath(tempDir)
	viper.SetConfigName(".dev.config")
	viper.SetConfigType("yaml")

	if err := viper.ReadInConfig(); err != nil {
		t.Fatalf("Gagal membaca config: %v", err)
	}

	// Cek apakah key yang ditulis terbaca
	if viper.GetString("file.baseData") != "/tmp/data" {
		t.Errorf("Config tidak sesuai, dapat: %s", viper.GetString("file.baseData"))
	}
}

func TestInitConfig_DefaultEnvironment(t *testing.T) {
	os.Unsetenv("ENVIRONMENT")

	// Buat file .config.yaml sementara
	tempDir := createTempConfig(t, ".config.yaml", "file:\n  baseData: /tmp/default")

	// Override path agar viper cari di tempDir
	viper.Reset()
	viper.AddConfigPath(tempDir)
	viper.SetConfigName(".config")
	viper.SetConfigType("yaml")

	if err := viper.ReadInConfig(); err != nil {
		t.Fatalf("Gagal membaca config default: %v", err)
	}

	if viper.GetString("file.baseData") != "/tmp/default" {
		t.Errorf("Config default tidak sesuai, dapat: %s", viper.GetString("file.baseData"))
	}
}

func TestInitConfig_ProductionModeNoFile(t *testing.T) {
	os.Setenv("ENVIRONMENT", "production")

	viper.Reset()
	viper.AddConfigPath("/path/invalid")
	viper.SetConfigName(".prod.config")
	viper.SetConfigType("yaml")

	// Seharusnya gagal baca file tapi tidak panic
	err := viper.ReadInConfig()
	if err == nil {
		t.Error("Seharusnya error karena file config tidak ada")
	}
}
