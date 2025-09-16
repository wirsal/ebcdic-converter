package utils

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"
)

// WriteFile writes the given data to a file under [baseOutput]/[YYMMDD]/[fileName].
// It creates the directory if it doesn't exist.
// The file is opened in append mode, and data is flushed to disk using Sync().
func WriteFile(fileName, data string) (bool, error) {
	if err := os.MkdirAll(GetCurrentResultDir(), os.ModePerm); err != nil {
		return false, fmt.Errorf("failed to create directory: %w", err)
	}

	path := filepath.Join(GetCurrentResultDir(), fileName)
	file, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return false, fmt.Errorf("failed to open file: %w", err)
	}
	defer file.Close()

	if _, err := file.WriteString(data); err != nil {
		return false, fmt.Errorf("failed to write data: %w", err)
	}

	if err := file.Sync(); err != nil {
		return false, fmt.Errorf("failed to sync file: %w", err)
	}

	return true, nil
}

// ReadAllFile reads a file from the path [basePath]/[YYMMDD]/[fileName]
// and returns its raw binary content as a string.
// Note: No EBCDIC to ASCII or HEX conversion is performed here.
func ReadAllFile(fileName string) (string, error) {
	path := filepath.Join(GetCurrentDataDir(), fileName)

	file, err := os.OpenFile(path, os.O_RDONLY, 0)
	if err != nil {
		return "", err
	}

	fi, err := file.Stat()
	if err != nil {
		return "", err
	}

	text := make([]byte, fi.Size())
	bufr := bufio.NewReader(file)
	_, err = bufr.Read(text)
	if err != nil {
		return "", err
	}
	return string(text), nil
}

func DeleteResultFile(directory, fileName string) error {

	resultFile := filepath.Join(directory, fileName)
	err := os.Remove(resultFile)
	if err != nil {
		return fmt.Errorf("failed to delete file %s: %w", resultFile, err)
	}
	return nil
}

func MoveFile(fileName string) error {

	sourcePath := filepath.Join(GetCurrentDataDir(), fileName)
	destPath := filepath.Join(GetCurrentBackupDir(), fileName)

	// Cek dan buat folder backup jika belum ada
	err := os.MkdirAll(GetCurrentBackupDir(), os.ModePerm)
	if err != nil {
		return fmt.Errorf("failed to create destination folder: %w", err)
	}

	// Pindahkan file
	err = os.Rename(sourcePath, destPath)
	if err != nil {
		return fmt.Errorf("failed to move file: %w", err)
	}

	return nil
}

func FileExists(filename string) bool {
	_, err := os.Stat(filename)
	return err == nil
}

// waitForFileInDir checks for the existence of a file in the specified directory.
// It continuously loops until the file is found, pausing between checks based on
// the global `waitDuration` value.
//
// Parameters:
//   - dir: the directory path to look in
//   - filename: the name of the file to check for
func WaitForFileInDir(dir, filename string, waitDuration time.Duration) {
	filePath := filepath.Join(dir, filename)
	for !FileExists(filePath) {
		Info("File %s does not exist yet. Waiting for %v", filePath, waitDuration)

		time.Sleep(waitDuration)
	}
	Info("File found: %s", filePath)
}

// FileExists checks whether a file or directory exists at the given path.
// Returns `true` if the file exists and is accessible, `false` otherwise.
func WaitUntilNextDay(filename string) {
	// Hitung waktu hingga tengah malam
	now := time.Now()
	next := now.AddDate(0, 0, 1).Truncate(24 * time.Hour)
	duration := next.Sub(now)

	// Info("Finished processing %s. Waiting until the next day (%v)", filename, duration)
	time.Sleep(duration)
}

// MergeFiles menggabungkan isi dari dua file (file1 dan file2) ke file output.
func MergeFiles(file1, file2, output string) error {
	out, err := os.Create(output)
	if err != nil {
		return fmt.Errorf("failed to create output file: %w", err)
	}
	defer out.Close()

	// Copy file1
	if err := copyFileContent(file1, out); err != nil {
		return fmt.Errorf("failed to copy contents of file1: %w", err)
	}

	// Tambahkan newline sebagai pemisah
	_, _ = out.WriteString("\n")

	// Copy file2
	if err := copyFileContent(file2, out); err != nil {
		return fmt.Errorf("failed to copy contents of file2: %w", err)
	}

	return nil
}

// copyFileContent menyalin isi file ke file tujuan (writer).
func copyFileContent(filename string, out *os.File) error {
	in, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer in.Close()

	_, err = io.Copy(out, in)
	return err
}
