PROJECT = pushdings
PROJECT_DESCRIPTION = Pusheldy Dingseldey
PROJECT_VERSION = 1.1.0

DEPS = cowboy eper gen_smtp gproc hackney jsx lager uuid

BUILD_DEPS = elvis_mk

TEST_DEPS = katt

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 784e41b
dep_katt = git https://github.com/for-GET/katt.git 1.5.2

DEP_PLUGINS = elvis_mk

COVER=1

SHELL_OPTS = -pa ebin \
	-pa deps/*/ebin \
	-boot start_sasl \
	-config dev.config \
	-mnesia dir db \
	-mnesia debug verbose \
	-s rb \
	-s $(PROJECT) \
	-sname $(PROJECT)

DIALYZER_DIRS = ebin

PLT_APPS = crypto inets mnesia

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'

tarball: rel
	cd _rel && tar cfvz ../$(PROJECT)-$(PROJECT_VERSION).tar.gz $(PROJECT)
