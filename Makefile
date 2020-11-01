CC=go build
ARCHS=amd64 386
DIST=./dist

all: clean update test windows linux darwin

release: all zip

update:
	@go get -u; \
	go mod tidy -v

windows:
	@for GOARCH in ${ARCHS}; do \
		mkdir -p $(DIST)/overflow-windows-$$GOARCH; \
		GOOS=windows GOARCH=$$GOARCH GO111MODULE=on CGO_ENABLED=0 $(CC) -trimpath -o $(DIST)/overflow-windows-$$GOARCH/overflow.exe; \
	done

linux:
	@for GOARCH in ${ARCHS}; do \
		mkdir -p $(DIST)/overflow-linux-$$GOARCH; \
		GOOS=linux GOARCH=$$GOARCH GO111MODULE=on CGO_ENABLED=0 $(CC) -trimpath -o $(DIST)/overflow-linux-$$GOARCH/overflow; \
	done

darwin:
	@for GOARCH in ${ARCHS}; do \
		mkdir -p $(DIST)/overflow-darwin-$$GOARCH; \
		GOOS=darwin GOARCH=$$GOARCH GO111MODULE=on CGO_ENABLED=0 $(CC) -trimpath -o $(DIST)/overflow-darwin-$$GOARCH/overflow; \
	done

zip:
	@zip -r overflow-all.zip $(DIST); \
	for i in $(DIST)/*; do \
		zip -r "$$i.zip" "$$i"; \
	done; \
	mv overflow-all.zip $(DIST)/overflow-all.zip

test:
	@go test ./...

clean:
	@rm -rf $(DIST)/*; \
	go clean ./...
