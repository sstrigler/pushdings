PROJECT = pushdings
PROJECT_DESCRIPTION = Pushing Daisies
PROJECT_VERSION = 0.0.1

DEPS = cowboy eper gproc hackney lager jsx

BUILD_DEPS = elvis_mk

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 784e41b

DEP_PLUGINS = elvis_mk

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

PLT_APPS = mnesia
