ELMCMP = elm
REMOTE_RUNTIME = http://www.stephenbalaban.com/wp-content/uploads/2014/11/elm-runtime.js

gravity: Gravity.elm images/stars.png
	$(ELMCMP) Gravity.elm
	mkdir -p build/images
	cp images/stars.png build/images/.
	ln -s images/stars.png build/stars.png

distribution: Gravity.elm images/stars.png
	$(ELMCMP) --set-runtime=$(REMOTE_RUNTIME) Gravity.elm
	mkdir -p build/images
	cp images/stars.png build/images/.
	ln -s images/stars.png build/stars.png

clean:
	rm -rf build
	rm -rf cache
