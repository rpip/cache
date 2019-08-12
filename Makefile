ERL ?= $(shell which erl)
ERLC ?= $(shell which erlc)
ERLCFLAGS=-o
SRCDIR=.
BEAMDIR=./ebin
TEST123 = -s cache_test test1 -s cache_test test2 -s cache_test test2
TAR ?= $(shell which tar)
TARFILE = cache_task-yao.tar

all: clean
	@echo "==> compiling modules"
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
clean:
	@echo "==> cleaning build directory"
	@ rm -rf $(BEAMDIR) ;
	@ rm -rf erl_crash.dump cache.dets $(TARFILE) ._*
shell: all
	$(ERL) -pa $(BEAMDIR)
tar: all
	@echo "==> archiving project.. $(TARFILE)"
	@$(TAR) -pcf $(TARFILE) --exclude .git --exclude .gitignore --exclude $(TARFILE) --exclude ebin .
test-123: all
	@echo "==> running tests: 1, 2, 3"
	@ $(ERL) -pa $(BEAMDIR) -noshell $(TEST123) -s init stop
test-4: all
	@echo "==> running test 4"
	@ $(ERL) -pa $(BEAMDIR) -noshell -s cache_test test4 -s init stop
test: all test-123 test-4
	@echo "==> running tests"
	@ $(ERL) -pa $(BEAMDIR) -noshell  -s cache_test run_all -s init stop
test-123-ext: all
	@echo "==> running test 123 .. 10 times"
	@for i in {1..10}; do echo "==> $i\n\n"; make test-123; done > results.txt
test-cache-receiver: all
	@echo "==> running cache receiever tests"
	@ $(ERL) -pa $(BEAMDIR) -noshell -s cache_receiver test -s init stop
