package utils

func WriteAndCheck(outputFilename, data string) bool {
	if ok, err := WriteFile(outputFilename, data); !ok {
		Error("Write error: %v", err)
		return false
	}
	return true
}
