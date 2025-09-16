package utils

import (
	"path/filepath"
	"testing"
	"time"

	"github.com/spf13/viper"
)

func TestGetCurrentDirs(t *testing.T) {
	// Set konfigurasi viper untuk test
	viper.Set("file.baseData", "/tmp/data")
	viper.Set("file.baseBackup", "/tmp/backup")
	viper.Set("file.baseResult", "/tmp/result")
	viper.Set("file.baseLog", "/tmp/log")

	// Dapatkan tanggal kemarin
	yesterday := time.Now().AddDate(0, 0, -1).Format("060102")

	tests := []struct {
		name     string
		fn       func() string
		basePath string
	}{
		{"DataDir", GetCurrentDataDir, "/tmp/data"},
		{"BackupDir", GetCurrentBackupDir, "/tmp/backup"},
		{"ResultDir", GetCurrentResultDir, "/tmp/result"},
		{"LoggingDir", GetCurrentLoggingDir, "/tmp/log"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.fn()
			want := filepath.Join(tt.basePath, yesterday)

			if got != want {
				t.Errorf("%s() = %v; want %v", tt.name, got, want)
			}
		})
	}
}
