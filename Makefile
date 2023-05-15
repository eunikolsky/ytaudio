MAIN_TEST_TARGET = ytaudio:test:ytaudio-test

.DEFAULT_GOAL := check

### --8<-- test and build actions --8<-- ###

# `echo package.yaml stack.yaml` prints one line, and `entr` doesn't understand that;
# `print (-l)` doesn't work in `/bin/sh`,
# so `ls -1` it is;
#
# the ignored `.entr` file is for a more convenient way to restart `ghcid`:
# simply `touch`ing any of the files doesn't work for `entr`, but writing
# something to a file works: `:.w! .entr` in vim
ENTR_WATCHED_FILES = ls -1 package.yaml stack.yaml .entr

# something in `ghcid --test` hides the cursor, and being wrapped in `entr` doesn't
# restore it, so this command is to restore it
RESTORE_CURSOR = tput cnorm

.PHONY:
testd:
	@$(ENTR_WATCHED_FILES) | entr -rs "ghcid -c 'HSPEC_FORMAT=failed-examples stack ghci --test --main-is $(MAIN_TEST_TARGET) --ghci-options=-fobject-code' -T main"; $(RESTORE_CURSOR)

# run like this: `m testfw-ext MATCH=InputParser SEED=401874497`
# both variables are optional
.PHONY:
testd-ext:
	@$(ENTR_WATCHED_FILES) | entr -rs "ghcid --command \"stack ghci --test --main-is $(MAIN_TEST_TARGET) --ghci-options=-fobject-code\" --test \":main $${MATCH:+--match \"$${MATCH}\"} $${SEED:+--seed $${SEED}}\""; $(RESTORE_CURSOR)

.PHONY:
testfw:
	@stack test --fast --file-watch --ghc-options='-freverse-errors' $(MAIN_TEST_TARGET) $${MATCH:+--ta="--match \"/$${MATCH}/\""}

# `ghcid` doesn't track changes in package description files (`package.yaml`, `stack.yaml`),
# so we need to restart it manually: this is done externally by `entr`
.PHONY:
buildd:
	@$(ENTR_WATCHED_FILES) | entr -rs "ghcid -c 'stack ghci'"

.PHONY:
buildfw:
	@stack build --fast --file-watch --ghc-options='-freverse-errors'

.PHONY:
format:
	@fourmolu -q -i $$(git ls-files '*.hs')

### --8<-- git hook check actions --8<-- ###

.PHONY:
check: check-format check-build check-test check-hlint

.PHONY:
check-format:
	fourmolu -q -m check $$(git ls-files '*.hs')

.PHONY:
check-build:
	stack --verbosity error build --fast

.PHONY:
check-test:
	stack --verbosity error test --fast --ta='-f silent' $(MAIN_TEST_TARGET)

.PHONY:
check-hlint: check-hlint-other check-hlint-program

HLINT_ARGS = -j --no-summary

.PHONY:
check-hlint-program:
	hlint $(HLINT_ARGS) -h program/.hlint.yaml program

.PHONY:
check-hlint-other:
	hlint $(HLINT_ARGS) src test

# GNU ln supports the `-r` option to create a relative symlink
.PHONY:
install-precommit-hook:
	@gln -srvf .git-pre-commit .git/hooks/pre-commit
