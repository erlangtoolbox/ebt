PROJECT=ebt
VERSION=`./version.sh`
REVISION=`git --no-pager log --max-count=1 --format=format:%H`

spec:
	cat opensuse.spec-templ | \
		sed "s/{{VERSION}}/$(VERSION)/" | \
		sed "s/{{RELEASE}}/$(BUILD_NUMBER)/" | \
		sed "s/{{REVISION}}/$(REVISION)/" \
		> strikead-$(PROJECT).spec
