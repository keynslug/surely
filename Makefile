PROJECT = surely
DEPS = lager cowboy jiffy recon sync genlib

dep_recon = git https://github.com/ferd/recon master
dep_genlib = git https://github.com/platbox/genlib integration

ERLC_OPTS = \
	-Werror +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +'{parse_transform,lager_transform}'

SHELL_OPTS = -config dev -s surely -s sync

include erlang.mk
