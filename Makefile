KAWAC = kawa --main -C
SOURCE = waka.scm
MAINCLASS = waka.class
JAR = waka.jar
RM = rm -rf
UNZIP = unzip -q

KAWAJAR ?= kawa.jar
JLINEJAR ?= jline3.jar
JARS = $(KAWAJAR) $(JLINEJAR)

.PHONY: unjar clean

all: waka.jar

%.class: %.scm
	$(KAWAC) $<

manifest.txt:
	@echo "Manifest-Version: 1.0" > manifest.txt
	@echo "Main-Class: waka" >> manifest.txt
	@echo "" >> manifest.txt

unjar: $(KAWAJAR) $(JLINEJAR)
	$(UNZIP) $(KAWAJAR) -x 'META-INF/*' -d tmp/
	$(UNZIP) $(JLINEJAR) -x 'META-INF/*' -d tmp/

waka.jar: manifest.txt $(MAINCLASS) unjar
	jar -cmf manifest.txt $(JAR) *.class -C tmp gnu -C tmp kawa -C tmp org

clean:
	$(RM) *.class $(JAR) tmp/ manifest.txt
