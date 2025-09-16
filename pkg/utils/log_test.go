package utils

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/spf13/viper"
)

func setupLogTestEnv(t *testing.T) string {
	tmpDir := t.TempDir()

	// Set lokasi log folder dan nama file log
	viper.Set("file.baseLog", tmpDir)
	viper.Set("file.Log.Logging", "app.log")

	// Pastikan folder ada
	os.MkdirAll(GetCurrentLoggingDir(), os.ModePerm)

	return filepath.Join(GetCurrentLoggingDir(), "app.log")
}

func TestLoggingFunctions(t *testing.T) {
	logFile := setupLogTestEnv(t)

	// Panggil semua fungsi logging
	Info("Ini pesan info")
	Debug("Ini pesan debug")
	Warn("Ini pesan peringatan")
	Error("Ini pesan error", nil)

	// Pastikan file log ada
	if _, err := os.Stat(logFile); os.IsNotExist(err) {
		t.Fatalf("File log tidak dibuat: %v", err)
	}

	// Baca isi file log
	data, err := os.ReadFile(logFile)
	if err != nil {
		t.Fatalf("Gagal membaca file log: %v", err)
	}
	content := string(data)

	// Cek apakah semua level log ada di file
	wantLevels := []string{"INFO", "DEBUG", "WARN", "ERROR"}
	for _, level := range wantLevels {
		if !strings.Contains(content, level) {
			t.Errorf("Log untuk level %s tidak ditemukan di file log", level)
		}
	}

	t.Logf("Log berhasil ditulis ke %s", logFile)
}

func TestWriteLog_ErrorHandling(t *testing.T) {
	// Set folder yang tidak valid agar penulisan gagal
	viper.Set("file.baseLog", "/root/forbidden")
	viper.Set("file.Log.Logging", "app.log")

	// Coba tulis log ke folder yang tidak ada izin
	ok, err := writeLog("Test pesan gagal\n")
	if err == nil || ok {
		t.Errorf("Seharusnya gagal menulis log, tapi berhasil")
	}
}

func TestLogFileFormat(t *testing.T) {
	logFile := setupLogTestEnv(t)

	// Tulis log dengan timestamp
	Info("Pesan dengan timestamp")

	data, err := os.ReadFile(logFile)
	if err != nil {
		t.Fatalf("Gagal membaca file log: %v", err)
	}
	content := string(data)

	// Cek apakah timestamp (format YYYY/MM/DD) ada di file
	today := time.Now().Format("2006/01/02")
	if !strings.Contains(content, today) {
		t.Errorf("Timestamp %s tidak ditemukan di log", today)
	}
}
