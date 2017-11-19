
FILES := $(shell find src/main/scala/stainless src/main/scala/actors -name '*.scala' -type f)

ARGS :=
TIMEOUT := 5

STAINLESS := stainless
STAINLESS_OPTIONS := --timeout=$(TIMEOUT) --check-models

ifeq ($(WATCH),1)
STAINLESS_OPTIONS := --watch $(STAINLESS_OPTIONS)
endif

ifneq ($(NOCACHE),1)
STAINLESS_OPTIONS := --vc-cache $(STAINLESS_OPTIONS)
endif

ifeq ($(TERMINATION),1)
STAINLESS_OPTIONS := --termination --debug=forcing,termination $(STAINLESS_OPTIONS)
else
STAINLESS_OPTIONS := --verification --debug=forcing,verification $(STAINLESS_OPTIONS)
endif

STAINLESS_OPTIONS := $(STAINLESS_OPTIONS) $(ARGS)

%:
	$(STAINLESS) $(STAINLESS_OPTIONS) $(FILES) src/main/scala/examples/$@.scala
