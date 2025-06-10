CXX := /opt/homebrew/opt/llvm/bin/clang++

CXXFLAGS := -std=c++14 -Wall -Wextra -w -nostdinc++ -isystem /opt/homebrew/opt/llvm/include/c++/v1

SRC_DIR := src
BUILD_DIR := build

# Пути к LLVM
LLVM_PREFIX := /opt/homebrew/opt/llvm
LLVM_CONFIG := $(LLVM_PREFIX)/bin/llvm-config

# Получение флагов через llvm-config
LLVM_CXXFLAGS := $(shell $(LLVM_CONFIG) --cxxflags | sed 's/-stdlib=libc++//g')
LLVM_LDFLAGS := $(shell $(LLVM_CONFIG) --ldflags --libs --system-libs)

# Цели
all: $(BUILD_DIR)/compiler

$(BUILD_DIR)/compiler: $(SRC_DIR)/main.cpp | $(BUILD_DIR)
	$(CXX) $(CXXFLAGS) $< -o $@ $(LLVM_CXXFLAGS) $(LLVM_LDFLAGS)

$(BUILD_DIR):
	mkdir -p $@

clean:
	rm -rf $(BUILD_DIR)

run:
	./build/compiler

rebuild: clean all

.PHONY: all clean rebuild