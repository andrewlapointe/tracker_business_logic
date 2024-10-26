REBAR = rebar3

# Default target when running 'make'
all: clean compile test cover eqc xref dialyzer release

# Run all checks (test, xref, dialyzer)
check: test xref dialyzer cover

# Run compile, checks, then open shell
shell: clean compile check shell

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) as test do eunit

# code coverage report. not currently working, but will not error
cover:
	$(REBAR) cover

# also not working currently
eqc:
	$(REBAR) eqc || echo "QuickCheck not available, skipping."

# Check for cross-references in the code
xref:
	-$(REBAR) xref

dialyzer:
	-$(REBAR) dialyzer

release: 
	$(REBAR) release

shell:
	$(REBAR) shell

# Phony targets prevent conflicts with files named after these targets
.PHONY: all compile clean test eqc cover xref dialyzer release shell check
