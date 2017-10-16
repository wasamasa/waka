-injars         waka.jar
-outjars        waka_min.jar
-libraryjars    <java.home>/lib/rt.jar
-libraryjars    tmp/jna-4.2.2.jar
-libraryjars    tmp/jansi-1.16.jar
-libraryjars    tmp/juniversalchardet-1.0.3.jar
-libraryjars    tmp/sshd-core-1.4.0.jar
-keepattributes *Annotation*
-keepattributes Signature
-keepattributes InnerClasses,EnclosingMethod
-dontwarn       java.lang.invoke.MethodHandle

-keep public class waka {
      public static void main(java.lang.String[]);
}

-keep class waka { waka $instance; }

-keep class gnu.mapping.Procedure
-keep class gnu.mapping.CallContext

-keep,includedescriptorclasses class gnu.kawa.io.JLineInPort$KawaParsedLine {
      gnu.expr.Compilation parse(gnu.expr.Language,gnu.text.Lexer);
}

-keepclassmembers class sun.misc.Signal {
      <init>(java.lang.String);
}

-keepclassmembers class sun.misc.SignalHandler {
      sun.misc.SignalHandler SIG_IGN;
}

-keepclassmembers class org.jline.terminal.Terminal$SignalHandler {
      org.jline.terminal.Terminal$SignalHandler SIG_DFL;
      org.jline.terminal.Terminal$SignalHandler SIG_IGN;
}
