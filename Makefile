
build:
	spago bundle-app
	# cd src && cp -f index.js ../index.js

serve:
	python -m SimpleHTTPServer 8000

clean:
	rm -rf index.js

three.min.js:
	curl -L -o three.min.js https://raw.githubusercontent.com/mrdoob/three.js/dev/build/three.min.js

MTLLoader.js:
	curl -L -o MTLLoader.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/loaders/MTLLoader.js

OBJLoader.js:
	curl -L -o OBJLoader.js https://raw.githubusercontent.com/mrdoob/three.js/dev/examples/js/loaders/OBJLoader.js
