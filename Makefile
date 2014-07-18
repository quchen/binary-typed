NUM_CORES=$(shell grep -c ^processor /proc/cpuinfo)

test :
	./dist/build/tasty/tasty -j$(NUM_CORES) --quickcheck-tests 1000

bench :
	./dist/build/criterion/criterion -ocriterion-out.html

bench2 :
	./dist/build/criterion-overview/criterion-overview -ocriterion-overview-out.html