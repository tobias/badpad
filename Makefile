
target/public/cljs-out/dev-main.js: src/badpad/main.cljs
	clojure -A:fig:min

.PHONY: clean deploy dev

clean:
	rm -rf target

deploy: target/public/cljs-out/dev-main.js
	scp target/public/cljs-out/dev-main.js tcrawley.org:tcrawley.org/badpad/cljs-out
	scp -r resources/public/* tcrawley.org:tcrawley.org/badpad

dev:
	clojure -A:fig:build
