target_game = redblue_english
# target_game = yellow_english

# --------------------------------------------------

CA65 = ca65
LD65 = ld65

SRC_DIR = src
INCLUDE_DIR = $(SRC_DIR)/include
BUILD_DIR = build

CA65_FLAGS = --include-dir $(INCLUDE_DIR)/

dirs = \
	kernel \

src_subdirs := $(patsubst %,$(SRC_DIR)/%,$(dirs))

obj_subdirs := $(patsubst %,$(BUILD_DIR)/%,$(dirs))

.PHONY: all clean

kernel_objs = $(patsubst $(SRC_DIR)/%.s,$(BUILD_DIR)/%.o,$(shell ls $(SRC_DIR)/kernel/*.s))

vpath %.o $(BUILD_DIR) $(obj_subdirs)
vpath %.bin $(BUILD_DIR)
vpath %.inc $(SRC_DIR) $(INCLUDE_DIR)
vpath %.cfg $(SRC_DIR)
vpath %.s $(SRC_DIR) $(src_subdirs)

all: cinderacekit_kernel.bin

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.s
	mkdir -p $(@D)
	$(CA65) $(CA65_FLAGS) -o $@ $<

cinderacekit_kernel.bin: cinderacekit_rb.cfg $(kernel_objs)
	$(LD65) -o $(BUILD_DIR)/$@ --dbgfile $(BUILD_DIR)/$(@:.bin=.dbg) --config $^
	
clean:
	-rm -r build 2> /dev/null || true

