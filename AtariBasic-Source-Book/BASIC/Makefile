
.PHONY : clean check md5check

INCDIR    = ../include

SRCBASE=ataribas

INC = $(INCDIR)/atari.inc $(INCDIR)/gtia.inc \
	  $(INCDIR)/pokey.inc $(INCDIR)/pia.inc $(INCDIR)/antic.inc

CA65FLAGS = -l $(SRCBASE).lst
# CA65FLAGS =

ROMS := $(SRCBASE)-revA.rom $(SRCBASE)-revB.rom $(SRCBASE)-revC.rom $(SRCBASE)-revD.rom
SHAS := $(SRCBASE)-revA.sha1 $(SRCBASE)-revB.sha1 $(SRCBASE)-revC.sha1 $(SRCBASE)-revD.sha1
SUMS := 70cdf57469a208528f78a14275837352e90e20f1 7ad88dd99ff4a6ee66f6d162074db6f8bef7a9b6 3693c9cb9bf3b41bae1150f7a8264992468fc8c0 f2b9e1bd49834baa2924b38d6a1b6eda15dac010

OS := $(shell uname)
ifeq ($(OS),Darwin)
MD5CHECK  := md5sum -c   # BSD/macOS md5(1)
else
MD5CHECK  := md5sum -cv  # GNU md5sum (Linux, etc.)
endif

RED   := \033[31m
RESET := \033[0m
define RUN_AND_FAIL
  @$(1) || { echo " 🛑 $(RED)$(1)$(RESET)"; exit 1; }
  @echo "$(1)"
endef

.SECONDARY: $(ROMS)

roms : $(ROMS)

revA : $(SRCBASE)-revA.rom
revB : $(SRCBASE)-revB.rom
revC : $(SRCBASE)-revC.rom
revD : $(SRCBASE)-revD.rom
revX : $(SRCBASE)-revX.rom

check: $(SHAS)
	@echo "Validation..."
	@i=1; \
	for f in $(SHAS); do \
		expected=$$(echo $(SUMS) | cut -d ' ' -f$$i); \
		actual=$$(cat $$f); \
		rm $$f; \
		if [ "$$actual" != "$$expected" ]; then \
			echo "⚠️ Checksum mismatch for $$f:"; \
			echo "    expected: $$expected"; \
			echo "    actual  : $$actual"; \
		else \
			echo "✅ Checksum OK for $${f%.sha1}.rom"; \
		fi; \
		((i++)); \
	done

md5check : $(ROMS)
	$(MD5CHECK) MD5SUM

%-revA.o : %.asm
	$(call RUN_AND_FAIL, ca65 -DBASIC_REVISION=1 $(CA65FLAGS) $^ -o $@)
	@test -e $(SRCBASE).lst && mv $(SRCBASE).lst $(SRCBASE)-revA.lst

%-revB.o : %.asm
	$(call RUN_AND_FAIL, ca65 -DBASIC_REVISION=2 $(CA65FLAGS) $^ -o $@)
	@test -e $(SRCBASE).lst && mv $(SRCBASE).lst $(SRCBASE)-revB.lst

%-revC.o : %.asm
	$(call RUN_AND_FAIL, ca65 -DBASIC_REVISION=3 $(CA65FLAGS) $^ -o $@)
	@test -e $(SRCBASE).lst && mv $(SRCBASE).lst $(SRCBASE)-revC.lst

%-revD.o : %.asm
	$(call RUN_AND_FAIL, ca65 -DBASIC_REVISION=4 $(CA65FLAGS) $^ -o $@)
	@test -e $(SRCBASE).lst && mv $(SRCBASE).lst $(SRCBASE)-revC.lst

%-revX.o : %.asm
	$(call RUN_AND_FAIL, ca65 -DBASIC_REVISION=10 $(CA65FLAGS) $^ -o $@)
	@test -e $(SRCBASE).lst && mv $(SRCBASE).lst $(SRCBASE)-revX.lst

%.rom : %.o
	$(call RUN_AND_FAIL, ld65 -C $(SRCBASE).lnk $< -o $@)

%.sha1 : %.rom
	@sha1sum $<  | cut -d ' ' -f1 > $@

%.dump : %.rom
	od -tx1 -Ax -v $^ > $@

$(ROMS) : ataribas.lnk $(OBJ) $(INC)

clean :
	$(RM) *.o *~ *.lst *.sha1 *.rom *.dump
