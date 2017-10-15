KAWAC = kawa --main -C
SOURCE = waka.scm
MAINCLASS = waka
JAR = waka.jar
RM = rm -rf
UNZIP = unzip -q

KAWAJAR ?= /usr/share/kawa/lib/kawa.jar
JLINEJAR ?= /usr/share/java/jline3.jar
JARS = $(KAWAJAR) $(JLINEJAR)

.PHONY: unjar clean

all: waka.jar

%.class: %.scm
	$(KAWAC) $<

manifest.txt:
	@printf 'Manifest-Version: 1.0\nMain-Class: $(MAINCLASS)\n\n' > manifest.txt

unjar: $(JARS)
	$(UNZIP) $(KAWAJAR) -x 'META-INF/*' -d tmp/
	$(UNZIP) $(JLINEJAR) -x 'META-INF/*' -d tmp/

waka.jar: manifest.txt $(MAINCLASS).class unjar
	jar -cmf manifest.txt $(JAR) *.class -C tmp gnu -C tmp kawa -C tmp org

clean:
	$(RM) *.class $(JAR) tmp/ manifest.txt
