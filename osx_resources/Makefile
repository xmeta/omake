# Assumes OMake has been built and installed into ./install

# This ought to work, but it doesn't.  :-(
#osxpkg: Info.plist Description.plist installer_files/ReadMe.txt installer_files/License.txt
	#PackageMaker -v -ds -build \
	#	-f ./osx_install \
	#	-i Info.plist \
	#	-d Description.plist \
	#	-r installer_files \
	#	-p ./OMake.pkg
	#tar cvzf OMake.pkg.tgz OMake.pkg

.PHONY: osxpkg osxpkg-clean

osxpkg:
	@echo ""
	@echo "======== Sorry, you have to use the PackageMaker GUI. ========"
	@echo "========     Be sure to update the version info!      ========"
	open ./OMake.pmproj

osxpkg-clean:
	rm -rf ./install OMake.pkg OMake-*.dmg

V := $(shell head -n 1 ../version.txt | tr -d ' \t\n\r')
dmg:
	hdiutil create -srcfolder ./OMake.pkg OMake-$(V).dmg
