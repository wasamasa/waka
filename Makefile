KAWAC = kawa --main -C
SOURCE = waka.scm
MAINCLASS = waka
JAR = waka.jar
MINJAR = waka_min.jar
RM = rm -rf
UNZIP = unzip -q
PROGUARD = proguard @minify.pro

KAWAJAR ?= /usr/share/kawa/lib/kawa.jar
JLINEJAR ?= /usr/share/java/jline3.jar
JARS = $(KAWAJAR) $(JLINEJAR)
MINIFYDEPS = tmp/jna-4.2.2.jar tmp/jansi-1.16.jar
MINIFYDEPS += tmp/juniversalchardet-1.0.3.jar tmp/sshd-core-1.4.0.jar

.PHONY: unjar minify clean

all: $(JAR) $(MINJAR)

%.class: %.scm
	$(KAWAC) $<

manifest.txt:
	@printf 'Manifest-Version: 1.0\nMain-Class: $(MAINCLASS)\n\n' > manifest.txt

unjar: $(JARS)
	$(UNZIP) $(KAWAJAR) -x 'META-INF/*' -d tmp/
	$(UNZIP) $(JLINEJAR) -x 'META-INF/*' -d tmp/

$(JAR): manifest.txt $(MAINCLASS).class unjar
	jar -cmf manifest.txt $(JAR) *.class -C tmp gnu -C tmp kawa -C tmp org

$(MINJAR): $(JAR) $(MINIFYDEPS)
	$(PROGUARD)

tmp/jna-4.2.2.jar:
	curl -o $@ http://central.maven.org/maven2/net/java/dev/jna/jna/4.2.2/jna-4.2.2.jar
tmp/jansi-1.16.jar:
	curl -o $@ http://central.maven.org/maven2/org/fusesource/jansi/jansi/1.16/jansi-1.16.jar
tmp/juniversalchardet-1.0.3.jar:
	curl -o $@ http://central.maven.org/maven2/com/googlecode/juniversalchardet/juniversalchardet/1.0.3/juniversalchardet-1.0.3.jar
tmp/sshd-core-1.4.0.jar:
	curl -o $@ http://central.maven.org/maven2/org/apache/sshd/sshd-core/1.4.0/sshd-core-1.4.0.jar

clean:
	$(RM) *.class $(JAR) $(MINJAR) tmp/ manifest.txt
