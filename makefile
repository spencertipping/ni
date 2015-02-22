SRC_FILES = core fn io iotypes ops optypes data main

ni: ni-boot
	./ni-boot --self > ni
	chmod +x ni

ni-boot: $(wildcard src/*.pl)
	cat $(foreach s,$(SRC_FILES),src/$(s).pl) > ni-boot
	chmod +x ni-boot
