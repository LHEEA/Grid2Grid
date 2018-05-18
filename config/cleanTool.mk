### Clean Rule -------------------------------------------------------------

.PHONY : cleanSub
cleanSub:
	$(foreach subMakeObj, $(SUB_CREATEOBJ_LIST), 		\
		$(MAKE_SUB_CLEAN) $(subMakeObj)/makefile;)

.PHONY : cleanSubAll
cleanSubAll:
	$(foreach subMakeObj, $(SUB_CREATEOBJ_LIST), 		\
		$(MAKE_SUB_CLEANALL) $(subMakeObj)/makefile;)

.PHONY : clean
clean: \
	cleanSub \
	cleanObj

.PHONY : cleanall
cleanall: \
	cleanSubAll \
	cleanObj \
	cleanlib
