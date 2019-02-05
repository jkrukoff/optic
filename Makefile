MAKEFLAGS += --warn-undefined-variables
.DEFAULT_GOAL := all

PYTHON := python3
REBAR ?= ./rebar3

.PHONY: all
## Build everything.
all: test-unit doc
	${MAKE} compile

.PHONY: clean
## Delete intermediate files.
clean:
	${REBAR} clean -a

.PHONY: compile
## Compile all profiles.
compile:
	${REBAR} compile
	${REBAR} as test compile

.PHONY: doc
## Build documentation.
doc: require-pic2plot $(addsuffix .png,$(basename $(wildcard doc/*.pic)))
	${REBAR} edoc
	${REBAR} as markdown edoc

doc/%.png: doc/%.pic
	pic2plot --font-size=0.010 --bitmap-size=4096x4096 --line-width=0.00097656 -Tpng $< > $@

.SILENT: help
.PHONY: help
## This help screen.
help:
	# Extracts help from the Makefile itself, printing help for any rule
	# which matches the defined regular expression and that has a double
	# hash (##) comment on the line above.
	printf "Available Targets:\n\n"
	awk '/^[a-zA-Z\-\_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-18s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' ${MAKEFILE_LIST}

rebar.lock: rebar.config
	${REBAR} update
	${REBAR} unlock
	${REBAR} upgrade

REQUIREMENTS = $(addprefix require-,pic2plot yamllint virtualenv)

.PHONY: ${REQUIREMENTS}
${REQUIREMENTS}: what=$(patsubst require-%,%,$@)
${REQUIREMENTS}: require-%:
	@which $(what) > /dev/null || \
		(printf "%s%s%s\n" "$$(tput setaf 3)" '"$(what)" is required, please install.' "$$(tput sgr0)"; exit 1)

rebar3:
	curl -o rebar3 https://s3.amazonaws.com/rebar3/rebar3
	chmod 755 rebar3

.PHONY: test
## Run all test suites.
test: test-unit test-integration

.PHONY: test-integration
## Run the integration test suite.
test-integration:
	envsubst < config/test.config.template > config/test.config
	if ! ${REBAR} as test do ct --config=config/test.config; then \
		grep -lr 'CT Error Notification' _build/test/logs/ | xargs -n 1 html2text; \
	fi

.PHONY: test-unit
## Run the unit test suite.
test-unit: require-yamllint
	${REBAR} as prod dialyzer
	${REBAR} as test do dialyzer, eunit, proper, geas #, lint
	yamllint -s .

.virtualenv: requirements.txt
	${MAKE} require-virtualenv
	virtualenv --clear --download --always-copy -p "$$(which ${PYTHON})" .virtualenv
	.virtualenv/bin/pip install --force-reinstall --upgrade pip setuptools wheel
	.virtualenv/bin/pip install -r requirements.txt
	@printf "\n\n%s%s%s\n" "$$(tput setaf 3)" "To activate virtualenv run: source .virtualenv/bin/activate" "$$(tput sgr0)"
