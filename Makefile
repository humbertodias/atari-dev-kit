MKFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MKFILE_DIR  := "$(subst Makefile,,$(MKFILE_PATH))"

build:
	docker build -t hldtux/atari-dev-kit-cc65 .

run:
	docker run -t -v $(MKFILE_DIR)/games:/root/games -w /root/games -i hldtux/atari-dev-kit-cc65 sh