
FILES := $(shell find src/main/scala/stainless src/main/scala/actors -name '*.scala' -type f)

ARGS :=
TIMEOUT := 5

STAINLESS := stainless
STAINLESS_OPTIONS := --timeout=$(TIMEOUT) --check-models

ifeq ($(WATCH),1)
STAINLESS_OPTIONS := --watch $(STAINLESS_OPTIONS)
endif

ifneq ($(CACHE),1)
STAINLESS_OPTIONS := --vc-cache=false $(STAINLESS_OPTIONS)
endif

ifeq ($(PARTIAL),1)
STAINLESS_OPTIONS := --partial-eval $(STAINLESS_OPTIONS)
endif

ifeq ($(TERMINATION),1)
STAINLESS_OPTIONS := --termination --debug=termination $(STAINLESS_OPTIONS)
else
STAINLESS_OPTIONS := --verification --debug=verification $(STAINLESS_OPTIONS)
endif

STAINLESS_OPTIONS := $(STAINLESS_OPTIONS) $(ARGS)

%:
	$(STAINLESS) $(STAINLESS_OPTIONS) $(FILES) src/main/scala/examples/$@.scala

