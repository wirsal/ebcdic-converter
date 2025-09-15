# EBCDIC Converter

**EBCDIC Converter** is a Go-based application for converting **legacy mainframe data (EBCDIC & COMP-3)** into modern formats such as **ASCII/UTF-8** or **JSON**.  
This project is designed to support **banking system modernization** where mainframe systems with COBOL-based data formats are still in use.

---

## ✨ Key Features
- 🔄 **EBCDIC & COMP-3 Conversion** → Supports both fixed-length and variable-length records  
- ⚡ **Batch Processing** → Efficient processing for large files with configurable batch size  
- 🧩 **Modular Design** → Independent modules (CIA, CEDC, CEPP, CA) for better maintainability  
- 📜 **Configurable via YAML** → Easy to configure file paths, record lengths, and batch sizes  
- 🐳 **Docker Ready** → Run anywhere using Docker containers  
- 🧪 **Unit Testing & CI/CD** → Clean and testable code with automation support  

---

## 📂 Project Structure
```text
ebcdic-converter/
├── cmd/
│   └── converter/       # Application entry point (main.go, processor.go, combine.go)
├── pkg/
│   ├── config/          # Loads config.yaml
│   ├── converter/       # Conversion modules (cia, cedc, cepp, ca)
│   └── utils/           # Helper functions: file I/O, logging, error handling
├── configs/
│   └── config.yaml      # Configuration file
├── test/                # Unit tests
├── go.mod
└── README.md
