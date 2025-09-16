package config

import (
	"bytes"
	"os"
	"strings"
	"testing"

	"github.com/spf13/viper"
)

func TestPrintWelcome(t *testing.T) {
	// Set config value di viper
	viper.Set("server.appName", "TestApp")
	viper.Set("server.version", "1.0.0")
	viper.Set("server.batchSize", 100)
	viper.Set("server.waitDuration", 10)

	// Simpan stdout asli dan redirect ke buffer
	old := os.Stdout
	r, w, _ := os.Pipe()
	os.Stdout = w

	// Jalankan fungsi PrintWelcome()
	PrintWelcome()

	// Tutup writer dan kembalikan stdout
	w.Close()
	os.Stdout = old

	// Baca hasil output
	var buf bytes.Buffer
	_, _ = buf.ReadFrom(r)
	output := buf.String()

	// Cek apakah output mengandung semua value yang di-set di viper
	expectedValues := []string{
		"TestApp",
		"1.0.0",
		"100 baris per batch",
		"10 detik jika file belum tersedia",
	}

	for _, val := range expectedValues {
		if !strings.Contains(output, val) {
			t.Errorf("Output tidak mengandung nilai: %s", val)
		}
	}
}
