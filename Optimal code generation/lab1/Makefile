PLUGIN_NAME = gimple_dump
PLUGIN_DIR = src/plugin
TEST_DIR = src/test

GCC = gcc-14
GXX = g++-14
PLUGIN_INCLUDE = $(shell $(GCC) -print-file-name=plugin)/include
GMP_INCLUDE = /opt/homebrew/include

CFLAGS = -I$(PLUGIN_INCLUDE) -I$(GMP_INCLUDE) -shared -fPIC -fno-rtti -Wno-reorder
LDFLAGS = -undefined dynamic_lookup

$(PLUGIN_NAME).so:
	$(GXX) $(CFLAGS) $(LDFLAGS) $(PLUGIN_DIR)/$(PLUGIN_NAME)_plugin.cpp -o $(PLUGIN_DIR)/$@

test: $(PLUGIN_NAME).so
	$(GCC) -fplugin=./$(PLUGIN_DIR)/$(PLUGIN_NAME).so -O0 $(TEST_DIR)/test.c -o $(TEST_DIR)/test
	@echo "\nRunning the test program:"
	./$(TEST_DIR)/test

clean:
	rm -f $(PLUGIN_DIR)/*.so $(TEST_DIR)/test

.PHONY: test clean