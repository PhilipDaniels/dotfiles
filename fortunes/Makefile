# Use this Makefile to regenerate the fortunes. Steps:
#    1. Edit the fortune text file (the one that doesn't end with .dat).
#    2. Run 'make'. This will run the 'all' target.
#    3. Use the 'overlong' target to find long lines.


# Look for all files in the current directory except those ending with .dat
# or called README or Makefile.
INPUTS += $(shell ls -1 | egrep -v '\.dat|README|Makefile') 
POSSIBLE += $(shell ls -1 | egrep -v '\.dat|README|Makefile' | sed -e 's/$$/.dat/g')

all: ${POSSIBLE}

%.dat : %
	@strfile $< $@

# Blow away all the .dat files so they can be recreated.
clean:
	rm *.dat

overlong:
	grep --color -n '.\{80\}' ${INPUTS}

