default: clean test

# Set LIBRARY_PATH to the path containing target and armaillo, or

.PHONY: test
test:
	g++ test.cpp -larmadillo -ltarget -o test
#	g++ test.cpp -larmadillo -ltarget -L$(HOME)/local/lib64 -o test
	@echo "Running test program"; echo ""	
	@./test

.PHONY: functional
functional:
	rm functional
	g++ functional.cpp -o functional && ./functional

.PHONY: clean
clean:
	@rm -f test
