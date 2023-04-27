MAIN_TEST_TARGET = ytaudio:test:ytaudio-test

.PHONY:
check: check-build check-test check-hlint

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-test:
	stack --verbosity error test --fast --ta='-f silent' $(MAIN_TEST_TARGET)


.PHONY:
check-hlint: check-hlint-other check-hlint-program

.PHONY:
check-hlint-program:
	hlint -j4 -h program/.hlint.yaml program

.PHONY:
check-hlint-other:
	hlint -j4 src test

.PHONY:
testd:
	@ghcid -c 'HSPEC_FORMAT=failed-examples stack ghci --test --main-is $(MAIN_TEST_TARGET) --ghci-options=-fobject-code' -T main

# run like this: `m testfw-ext MATCH=InputParser SEED=401874497`
# both variables are optional
.PHONY:
testd-ext:
	@ghcid --command "stack ghci --test --main-is $(MAIN_TEST_TARGET) --ghci-options=-fobject-code" --test ":main $${MATCH:+--match \"$${MATCH}\"} $${SEED:+--seed $${SEED}}"

.PHONY:
testfw:
	@stack test --fast --file-watch $(MAIN_TEST_TARGET) $${MATCH:+--ta="--match \"/$${MATCH}/\""}

.PHONY:
buildd:
	@ghcid -c 'stack ghci'

.PHONY:
buildfw:
	@stack build --fast --file-watch

# GNU ln supports the `-r` option to create a relative symlink
.PHONY:
install-precommit-hook:
	@gln -srvf .git-pre-commit .git/hooks/pre-commit
