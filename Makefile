CC=go build
ARCHS=amd64 386
DIST=./dist

all: clean update test windows linux darwin

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

test:
	@go test ./...

clean:
	@rm -rf $(DIST)/*; \
	go clean ./...
