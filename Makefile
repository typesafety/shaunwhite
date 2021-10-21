.PHONY: help lint build check run haddock-all haddock-deps hoogle hoogle-generate

help:
	@echo TODO: Implement help target

# Run hlint on the project. Assumes that hlint has been installed and can be
# run with `stack exec hlint`.
lint:
	@stack exec -- hlint --no-exit-code lib \
		|| echo Ensure that hlint is installed for the project with \
			\"stack build hlint\".

# Build the project.
build:
	stack build --test --bench --no-run-tests --no-run-benchmarks

# Only run type checking (uses a hacky method to accomplish this). Will output
# a "()" to stdout.
check:
	@stack ghci --ghci-options="-e ()"

# Run the bot locally. Attempts to use a token file "token" in the same
# directory, and a configuration file from
# "$XDG_CONFIG_HOME/.config/shaunwhite/config.json".
run:
	stack run -- --token ./token

# Build all documentation.
haddock-all:
	stack build --haddock

# Build documentation only for dependencies.
haddock-deps:
	stack build --haddock-deps

# Run local hoogle server for searchable documentation.
hoogle:
	stack hoogle -- server --local --port=8080

# Generate the Hoogle index.
hoogle-generate:
	stack hoogle -- generate --local
