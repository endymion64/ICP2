default:
	@echo
	@echo "**********************************************************"
	@echo
	@echo "(g)make -C tpico install"
	@echo "     to build and install Pico with a text only interface"
	@echo "        based on the readline libraries"
	@echo
	@echo "(g)make -C MetaPico install"
	@echo "     to install the MetaPico files in /usr/local/share/"
	@echo
	@echo "**********************************************************"
	@echo

distri tgz:
	$(MAKE) -C Pico clean
	$(MAKE) -C tpico clean
	$(MAKE) -C wxPico clean
	$(MAKE) -C PicoShell clean
	cd .. ; tar cfz tpico-1.0pl19.tar.gz tpico-1.0pl19
