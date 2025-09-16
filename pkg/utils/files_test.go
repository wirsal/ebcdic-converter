package utils

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/spf13/viper"
)

func setupTestEnv(t *testing.T) string {
	tmpDir := t.TempDir()

	// Set base folder untuk testing
	viper.Set("file.baseData", filepath.Join(tmpDir, "data"))
	viper.Set("file.baseBackup", filepath.Join(tmpDir, "backup"))
	viper.Set("file.baseResult", filepath.Join(tmpDir, "result"))
	viper.Set("file.baseLog", filepath.Join(tmpDir, "log"))

	// Pastikan folder data sudah ada
	yesterdayDir := filepath.Join(viper.GetString("file.baseData"), getYesterdayYYMMDD())
	os.MkdirAll(yesterdayDir, os.ModePerm)

	return tmpDir
}

func TestWriteAndReadFile(t *testing.T) {
	tmpDir := setupTestEnv(t)

	fileName := "testfile.txt"
	content := "Hello, World!"

	ok, err := WriteFile(fileName, content)
	if err != nil || !ok {
		t.Fatalf("WriteFile gagal: %v", err)
	}

	// Copy file ke folder data supaya ReadAllFile bisa baca
	src := filepath.Join(GetCurrentResultDir(), fileName)
	dst := filepath.Join(GetCurrentDataDir(), fileName)
	os.MkdirAll(GetCurrentDataDir(), os.ModePerm)
	os.Rename(src, dst)

	readContent, err := ReadAllFile(fileName)
	if err != nil {
		t.Fatalf("ReadAllFile gagal: %v", err)
	}
	if readContent != content {
		t.Errorf("Isi file tidak sesuai. got=%s, want=%s", readContent, content)
	}

	// Cek FileExists
	if !FileExists(dst) {
		t.Errorf("FileExists harusnya true")
	}

	// Delete file
	if err := DeleteResultFile(GetCurrentDataDir(), fileName); err != nil {
		t.Errorf("DeleteResultFile gagal: %v", err)
	}

	if FileExists(dst) {
		t.Errorf("File masih ada setelah dihapus")
	}

	t.Logf("Tes Write, Read, Delete berhasil di %s", tmpDir)
}

func TestMoveFile(t *testing.T) {
	setupTestEnv(t)

	fileName := "movefile.txt"
	content := "Data untuk dipindahkan"

	// Tulis file di folder data
	os.MkdirAll(GetCurrentDataDir(), os.ModePerm)
	src := filepath.Join(GetCurrentDataDir(), fileName)
	os.WriteFile(src, []byte(content), 0644)

	// Pindahkan ke folder backup
	if err := MoveFile(fileName); err != nil {
		t.Fatalf("MoveFile gagal: %v", err)
	}

	dest := filepath.Join(GetCurrentBackupDir(), fileName)
	if !FileExists(dest) {
		t.Errorf("File tidak ada di folder backup")
	}
}

func TestMergeFiles(t *testing.T) {
	tmpDir := setupTestEnv(t)

	file1 := filepath.Join(tmpDir, "file1.txt")
	file2 := filepath.Join(tmpDir, "file2.txt")
	out := filepath.Join(tmpDir, "merged.txt")

	os.WriteFile(file1, []byte("Hello"), 0644)
	os.WriteFile(file2, []byte("World"), 0644)

	if err := MergeFiles(file1, file2, out); err != nil {
		t.Fatalf("MergeFiles gagal: %v", err)
	}

	content, _ := os.ReadFile(out)
	expected := "Hello\nWorld"
	if string(content) != expected {
		t.Errorf("Isi file merged salah. got=%s, want=%s", string(content), expected)
	}
}

func TestWaitForFileInDir(t *testing.T) {
	tmpDir := setupTestEnv(t)

	fileName := "waitfile.txt"
	dir := filepath.Join(tmpDir, "waitdir")
	os.MkdirAll(dir, os.ModePerm)

	go func() {
		time.Sleep(500 * time.Millisecond)
		os.WriteFile(filepath.Join(dir, fileName), []byte("test"), 0644)
	}()

	WaitForFileInDir(dir, fileName, 100*time.Millisecond)
	if !FileExists(filepath.Join(dir, fileName)) {
		t.Errorf("File tidak ditemukan setelah WaitForFileInDir")
	}
}
