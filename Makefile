test :
	./dist/build/tasty/tasty

bench :
	./dist/build/criterion/criterion -ocriterion-out.html

bench2 :
	./dist/build/criterion-overview/criterion-overview -ocriterion-overview-out.html