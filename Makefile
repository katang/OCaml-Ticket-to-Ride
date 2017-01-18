main:
	corebuild -pkgs async,lablgtk2 main.byte && ./main.byte

test:
	corebuild -pkgs oUnit test_main.byte && ./test_main.byte

clean:
	corebuild -clean

zip:
	zip ttr	.zip *.ml{,i,y,l}
