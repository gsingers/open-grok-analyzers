/* The following code was generated by JFlex 1.6.1 */

/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").  
 * You may not use this file except in compliance with the License.
 *
 * See LICENSE.txt included in this distribution for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at LICENSE.txt.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */

/*
 * Cross reference a C# file
 * @author Christoph Hofmann - ChristophHofmann AT gmx dot de
 */

package com.grantingersoll.opengrok.analysis.csharp;
import com.grantingersoll.opengrok.analysis.JFlexXref;
import java.io.IOException;
import java.io.Writer;
import java.io.Reader;
import com.grantingersoll.opengrok.web.Util;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>/Users/grantingersoll/projects/OpenGrok/src/com.grantingersoll.opengrok/analysis/csharp/CSharpXref.lex</tt>
 */
public class CSharpXref extends JFlexXref {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;
  public static final int STRING = 2;
  public static final int COMMENT = 4;
  public static final int SCOMMENT = 6;
  public static final int QSTRING = 8;
  public static final int VSTRING = 10;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  5, 5
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\1\1\3\1\3\1\2\1\2\22\0\1\1\1\6\1\32"+
    "\1\37\2\6\1\36\1\33\2\37\1\34\1\25\1\6\1\10\1\11"+
    "\1\20\1\21\11\5\1\45\1\6\1\30\1\6\1\31\1\6\1\46"+
    "\2\23\1\13\1\27\1\24\1\44\1\4\1\40\3\4\1\26\3\4"+
    "\1\43\2\4\1\16\1\42\1\26\2\4\1\22\2\4\1\37\1\35"+
    "\2\37\1\7\1\37\2\23\1\14\1\27\1\24\1\44\1\4\1\41"+
    "\3\4\1\26\3\4\1\43\2\4\1\17\1\12\1\26\2\4\1\22"+
    "\2\4\3\37\1\6\6\0\1\3\371\0\1\15\u1ea8\0\1\3\1\3"+
    "\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\udfe6\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\6\0\1\1\1\2\2\3\1\4\1\5\2\4\1\5"+
    "\1\6\1\7\1\10\1\11\1\12\3\4\1\6\1\13"+
    "\4\4\1\2\2\14\1\15\1\4\1\13\1\16\1\17"+
    "\2\0\1\5\1\20\1\21\12\0\1\22\1\23\2\0"+
    "\1\24\1\0\1\25\1\26\2\5\1\0\1\5\3\0"+
    "\1\27\1\0\1\27\2\0\1\2\2\0\1\5\4\0"+
    "\1\27\1\30\3\0\1\31\2\0\1\32\4\0\1\33";

  private static int [] zzUnpackAction() {
    int [] result = new int[94];
    int offset = 0;
    offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAction(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /** 
   * Translates a state to a row index in the transition table
   */
  private static final int [] ZZ_ROWMAP = zzUnpackRowMap();

  private static final String ZZ_ROWMAP_PACKED_0 =
    "\0\0\0\47\0\116\0\165\0\234\0\303\0\352\0\u0111"+
    "\0\u0111\0\352\0\u0138\0\u015f\0\352\0\u0186\0\u01ad\0\u01d4"+
    "\0\352\0\352\0\352\0\352\0\u01fb\0\u0222\0\u0249\0\352"+
    "\0\u0270\0\u0297\0\u02be\0\u02e5\0\u030c\0\u0333\0\u0333\0\352"+
    "\0\u035a\0\u0381\0\u03a8\0\352\0\u0138\0\u03cf\0\u03f6\0\u041d"+
    "\0\352\0\352\0\u0444\0\u046b\0\u0492\0\u01fb\0\u04b9\0\u04e0"+
    "\0\u0507\0\u0222\0\u052e\0\u0555\0\352\0\352\0\u057c\0\u05a3"+
    "\0\352\0\u05ca\0\352\0\352\0\u05f1\0\u0618\0\u063f\0\u0666"+
    "\0\u068d\0\u06b4\0\u06db\0\u01fb\0\u0702\0\u0702\0\u0729\0\u0750"+
    "\0\352\0\u0777\0\u079e\0\u07c5\0\u07ec\0\u0813\0\u083a\0\u0861"+
    "\0\352\0\u0888\0\u08af\0\u08d6\0\u08fd\0\352\0\u0924\0\u094b"+
    "\0\u08af\0\u0972\0\u0999\0\u09c0\0\u09e7\0\u09e7";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[94];
    int offset = 0;
    offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackRowMap(String packed, int offset, int [] result) {
    int i = 0;  /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int high = packed.charAt(i++) << 16;
      result[j++] = high | packed.charAt(i++);
    }
    return j;
  }

  /** 
   * The transition table of the DFA
   */
  private static final int [] ZZ_TRANS = zzUnpackTrans();

  private static final String ZZ_TRANS_PACKED_0 =
    "\1\7\1\10\1\11\1\12\1\13\1\14\1\15\1\13"+
    "\2\15\3\13\1\7\2\13\1\16\1\17\3\13\1\15"+
    "\2\13\1\20\1\21\1\22\1\23\2\15\1\24\1\15"+
    "\5\13\2\15\1\7\1\10\1\11\1\12\1\25\1\26"+
    "\1\15\3\26\3\25\1\7\2\25\1\27\1\26\3\25"+
    "\1\15\2\25\1\30\1\21\1\31\2\15\1\32\1\24"+
    "\1\15\2\33\2\25\1\34\2\15\1\7\1\10\1\11"+
    "\1\12\1\25\1\26\1\15\3\26\3\25\1\7\2\25"+
    "\1\27\1\26\3\25\1\15\2\25\1\30\1\21\2\15"+
    "\1\35\1\15\1\24\1\15\2\33\2\25\1\34\2\15"+
    "\1\7\1\36\1\37\1\40\1\25\1\26\1\15\3\26"+
    "\3\25\1\7\2\25\1\27\1\26\3\25\1\15\2\25"+
    "\1\30\1\21\4\15\1\24\1\15\2\33\2\25\1\34"+
    "\2\15\1\7\1\10\1\11\1\12\1\25\1\26\1\15"+
    "\3\26\3\25\1\7\2\25\1\27\1\26\3\25\1\15"+
    "\2\25\1\30\1\21\1\15\1\41\1\15\1\42\1\24"+
    "\1\15\2\33\2\25\1\34\2\15\1\7\1\10\1\11"+
    "\1\12\1\25\1\26\1\15\3\26\3\25\1\7\2\25"+
    "\1\27\1\26\3\25\1\15\2\25\1\30\1\21\1\43"+
    "\2\15\1\44\1\24\1\15\2\33\2\25\1\34\2\15"+
    "\50\0\1\10\1\11\1\12\47\0\2\45\1\0\1\45"+
    "\2\0\3\45\1\0\2\45\1\0\4\45\1\0\2\45"+
    "\10\0\5\45\7\0\1\14\3\0\1\46\7\0\1\14"+
    "\2\0\1\47\1\0\2\50\14\0\1\50\22\0\1\51"+
    "\13\0\1\52\17\0\1\14\3\0\1\46\7\0\1\14"+
    "\1\53\1\0\1\47\1\0\2\50\14\0\1\50\6\0"+
    "\1\54\5\0\3\54\1\0\2\54\1\55\1\0\3\54"+
    "\1\0\2\54\10\0\5\54\6\0\2\56\1\0\2\56"+
    "\1\57\3\56\1\0\2\56\1\60\4\56\1\0\2\56"+
    "\10\0\5\56\1\0\1\61\4\0\2\62\1\0\6\62"+
    "\1\0\2\62\1\0\4\62\1\0\2\62\10\0\5\62"+
    "\1\0\1\61\4\0\1\63\5\0\3\63\1\0\2\63"+
    "\2\0\3\63\1\0\2\63\10\0\5\63\3\0\2\64"+
    "\76\0\1\65\2\0\1\66\15\0\2\56\1\0\2\56"+
    "\1\57\1\67\2\56\1\0\2\56\1\60\4\56\1\0"+
    "\2\56\10\0\2\56\1\67\2\56\1\0\1\61\4\0"+
    "\2\56\1\0\2\56\1\57\1\70\2\56\1\0\2\56"+
    "\1\60\4\56\1\0\2\56\10\0\2\56\1\70\2\56"+
    "\1\0\1\61\20\0\1\71\27\0\1\36\1\37\1\40"+
    "\44\0\2\72\77\0\1\73\1\0\1\66\12\0\2\64"+
    "\27\0\1\74\21\0\1\75\13\0\1\75\32\0\1\76"+
    "\2\0\1\77\10\0\1\76\3\0\1\77\47\0\2\50"+
    "\14\0\1\50\7\0\1\100\5\0\2\100\4\0\1\100"+
    "\1\0\2\100\2\0\1\100\14\0\1\100\6\0\2\54"+
    "\1\0\2\54\1\101\3\54\1\0\2\54\1\102\4\54"+
    "\1\0\2\54\10\0\5\54\6\0\1\103\5\0\3\103"+
    "\1\0\2\103\2\0\3\103\1\0\2\103\10\0\5\103"+
    "\6\0\2\56\1\0\2\56\1\57\1\104\1\105\1\106"+
    "\1\0\1\56\1\104\1\60\4\56\1\0\2\56\10\0"+
    "\1\56\1\104\3\56\1\0\1\61\4\0\1\107\5\0"+
    "\3\107\1\0\2\107\2\0\3\107\1\0\2\107\10\0"+
    "\5\107\6\0\2\110\1\0\6\110\1\0\2\110\1\0"+
    "\4\110\1\0\2\110\10\0\5\110\6\0\2\63\1\0"+
    "\6\63\1\0\2\63\1\60\4\63\1\0\2\63\10\0"+
    "\5\63\3\0\2\64\27\0\1\111\20\0\2\56\1\0"+
    "\2\56\1\57\1\112\2\56\1\0\2\56\1\60\4\56"+
    "\1\0\2\56\10\0\2\56\1\112\2\56\1\0\1\61"+
    "\4\0\2\56\1\0\2\56\1\57\3\56\1\0\2\56"+
    "\1\60\4\56\1\0\2\56\10\0\3\56\1\113\1\56"+
    "\1\0\1\61\1\0\2\72\30\0\1\111\20\0\1\75"+
    "\13\0\1\75\2\0\1\47\1\0\2\50\14\0\1\50"+
    "\7\0\1\76\13\0\1\76\4\0\2\50\14\0\1\50"+
    "\7\0\1\76\13\0\1\76\32\0\1\100\5\0\2\100"+
    "\4\0\1\100\1\0\1\100\1\114\1\0\1\50\1\100"+
    "\14\0\1\100\6\0\2\54\1\0\2\54\1\101\1\115"+
    "\1\116\1\117\1\0\1\54\1\115\1\102\4\54\1\0"+
    "\2\54\10\0\1\54\1\115\3\54\6\0\1\120\5\0"+
    "\3\120\1\0\2\120\2\0\3\120\1\0\2\120\10\0"+
    "\5\120\6\0\2\103\1\0\6\103\1\0\2\103\1\102"+
    "\4\103\1\0\2\103\10\0\5\103\6\0\2\56\1\0"+
    "\2\56\1\57\3\56\1\121\2\104\1\60\4\56\1\0"+
    "\2\56\10\0\5\56\1\0\1\61\4\0\2\122\1\0"+
    "\3\107\3\122\1\0\2\122\1\0\4\122\1\0\2\122"+
    "\10\0\5\122\6\0\2\110\1\0\2\110\1\123\3\110"+
    "\1\0\2\110\1\0\4\110\1\0\2\110\10\0\5\110"+
    "\6\0\2\56\1\0\2\56\1\57\3\56\1\0\2\56"+
    "\1\60\4\56\1\0\2\56\10\0\3\56\1\124\1\56"+
    "\1\0\1\61\4\0\2\56\1\0\2\56\1\57\3\56"+
    "\1\0\2\56\1\60\4\56\1\0\2\56\10\0\5\56"+
    "\1\125\1\61\5\0\1\100\2\0\1\77\2\0\2\100"+
    "\4\0\1\100\1\0\1\100\1\114\1\77\1\50\1\100"+
    "\14\0\1\100\6\0\2\54\1\0\2\54\1\101\3\54"+
    "\1\0\2\54\1\102\4\54\1\0\2\54\1\0\1\126"+
    "\6\0\5\54\6\0\2\54\1\0\2\54\1\101\3\54"+
    "\1\127\2\115\1\102\4\54\1\0\2\54\10\0\5\54"+
    "\6\0\2\54\1\0\2\54\1\101\3\54\1\127\2\115"+
    "\1\102\4\54\1\0\2\54\1\0\1\126\6\0\5\54"+
    "\6\0\2\130\1\0\3\120\3\130\1\0\2\130\1\0"+
    "\4\130\1\0\2\130\10\0\5\130\6\0\2\122\1\0"+
    "\3\107\3\122\1\0\2\122\1\60\4\122\1\0\2\122"+
    "\10\0\5\122\6\0\2\131\1\0\6\131\1\0\2\131"+
    "\1\0\4\131\1\0\2\131\10\0\5\131\6\0\2\56"+
    "\1\0\2\56\1\57\3\56\1\132\2\113\1\60\4\56"+
    "\1\0\2\56\10\0\5\56\1\125\1\61\20\0\1\133"+
    "\57\0\1\126\21\0\2\130\1\0\3\120\3\130\1\0"+
    "\2\130\1\102\4\130\1\0\2\130\1\0\1\126\6\0"+
    "\5\130\47\0\1\125\21\0\1\134\32\0\11\135\1\0"+
    "\12\135\4\0\3\135\1\0\7\135\4\0\2\136\4\135"+
    "\3\136\1\0\7\136\1\135\2\136\4\0\3\135\1\0"+
    "\5\136\2\135";

  private static int [] zzUnpackTrans() {
    int [] result = new int[2574];
    int offset = 0;
    offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackTrans(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      value--;
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }


  /* error codes */
  private static final int ZZ_UNKNOWN_ERROR = 0;
  private static final int ZZ_NO_MATCH = 1;
  private static final int ZZ_PUSHBACK_2BIG = 2;

  /* error messages for the codes above */
  private static final String ZZ_ERROR_MSG[] = {
    "Unknown internal scanner error",
    "Error: could not match input",
    "Error: pushback value was too large"
  };

  /**
   * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
   */
  private static final int [] ZZ_ATTRIBUTE = zzUnpackAttribute();

  private static final String ZZ_ATTRIBUTE_PACKED_0 =
    "\6\0\1\11\2\1\1\11\2\1\1\11\3\1\4\11"+
    "\3\1\1\11\7\1\1\11\3\1\1\11\1\1\2\0"+
    "\1\1\2\11\12\0\2\11\2\0\1\11\1\0\2\11"+
    "\2\1\1\0\1\1\3\0\1\1\1\0\1\1\2\0"+
    "\1\11\2\0\1\1\4\0\1\11\1\1\3\0\1\11"+
    "\2\0\1\1\4\0\1\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[94];
    int offset = 0;
    offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
    return result;
  }

  private static int zzUnpackAttribute(String packed, int offset, int [] result) {
    int i = 0;       /* index in packed string  */
    int j = offset;  /* index in unpacked array */
    int l = packed.length();
    while (i < l) {
      int count = packed.charAt(i++);
      int value = packed.charAt(i++);
      do result[j++] = value; while (--count > 0);
    }
    return j;
  }

  /** the input device */
  private java.io.Reader zzReader;

  /** the current state of the DFA */
  private int zzState;

  /** the current lexical state */
  private int zzLexicalState = YYINITIAL;

  /** this buffer contains the current text to be matched and is
      the source of the yytext() string */
  private char zzBuffer[] = new char[ZZ_BUFFERSIZE];

  /** the textposition at the last accepting state */
  private int zzMarkedPos;

  /** the current text position in the buffer */
  private int zzCurrentPos;

  /** startRead marks the beginning of the yytext() string in the buffer */
  private int zzStartRead;

  /** endRead marks the last character in the buffer, that has been read
      from input */
  private int zzEndRead;

  /** number of newlines encountered up to the start of the matched text */
  private int yyline;

  /** the number of characters up to the start of the matched text */
  private int yychar;

  /**
   * the number of characters from the last newline up to the start of the 
   * matched text
   */
  private int yycolumn;

  /** 
   * zzAtBOL == true <=> the scanner is currently at the beginning of a line
   */
  private boolean zzAtBOL = true;

  /** zzAtEOF == true <=> the scanner is at the EOF */
  private boolean zzAtEOF;

  /** denotes if the user-EOF-code has already been executed */
  private boolean zzEOFDone;
  
  /** 
   * The number of occupied positions in zzBuffer beyond zzEndRead.
   * When a lead/high surrogate has been read from the input stream
   * into the final zzBuffer position, this will have a value of 1;
   * otherwise, it will have a value of 0.
   */
  private int zzFinalHighSurrogate = 0;

  /* user code: */
  /* Must match WhiteSpace regex */
  private final static String WHITE_SPACE = "[ \t\f\r]+";

  // TODO move this into an include file when bug #16053 is fixed
  @Override
  protected int getLineNumber() { return yyline; }
  @Override
  protected void setLineNumber(int x) { yyline = x; }


  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  public CSharpXref(java.io.Reader in) {
    this.zzReader = in;
  }


  /** 
   * Unpacks the compressed character translation table.
   *
   * @param packed   the packed character translation table
   * @return         the unpacked character translation table
   */
  private static char [] zzUnpackCMap(String packed) {
    char [] map = new char[0x110000];
    int i = 0;  /* index in packed string  */
    int j = 0;  /* index in unpacked array */
    while (i < 194) {
      int  count = packed.charAt(i++);
      char value = packed.charAt(i++);
      do map[j++] = value; while (--count > 0);
    }
    return map;
  }


  /**
   * Refills the input buffer.
   *
   * @return      <code>false</code>, iff there was new input.
   * 
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  private boolean zzRefill() throws java.io.IOException {

    /* first: make room (if you can) */
    if (zzStartRead > 0) {
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
      System.arraycopy(zzBuffer, zzStartRead,
                       zzBuffer, 0,
                       zzEndRead-zzStartRead);

      /* translate stored positions */
      zzEndRead-= zzStartRead;
      zzCurrentPos-= zzStartRead;
      zzMarkedPos-= zzStartRead;
      zzStartRead = 0;
    }

    /* is the buffer big enough? */
    if (zzCurrentPos >= zzBuffer.length - zzFinalHighSurrogate) {
      /* if not: blow it up */
      char newBuffer[] = new char[zzBuffer.length*2];
      System.arraycopy(zzBuffer, 0, newBuffer, 0, zzBuffer.length);
      zzBuffer = newBuffer;
      zzEndRead += zzFinalHighSurrogate;
      zzFinalHighSurrogate = 0;
    }

    /* fill the buffer with new input */
    int requested = zzBuffer.length - zzEndRead;
    int numRead = zzReader.read(zzBuffer, zzEndRead, requested);

    /* not supposed to occur according to specification of java.io.Reader */
    if (numRead == 0) {
      throw new java.io.IOException("Reader returned 0 characters. See JFlex examples for workaround.");
    }
    if (numRead > 0) {
      zzEndRead += numRead;
      /* If numRead == requested, we might have requested to few chars to
         encode a full Unicode character. We assume that a Reader would
         otherwise never return half characters. */
      if (numRead == requested) {
        if (Character.isHighSurrogate(zzBuffer[zzEndRead - 1])) {
          --zzEndRead;
          zzFinalHighSurrogate = 1;
        }
      }
      /* potentially more input available */
      return false;
    }

    /* numRead < 0 ==> end of stream */
    return true;
  }

    
  /**
   * Closes the input stream.
   */
  public final void yyclose() throws java.io.IOException {
    zzAtEOF = true;            /* indicate end of file */
    zzEndRead = zzStartRead;  /* invalidate buffer    */

    if (zzReader != null)
      zzReader.close();
  }


  /**
   * Resets the scanner to read from a new input stream.
   * Does not close the old reader.
   *
   * All internal variables are reset, the old input stream 
   * <b>cannot</b> be reused (internal buffer is discarded and lost).
   * Lexical state is set to <tt>ZZ_INITIAL</tt>.
   *
   * Internal scan buffer is resized down to its initial length, if it has grown.
   *
   * @param reader   the new input stream 
   */
  public final void yyreset(java.io.Reader reader) {
    zzReader = reader;
    zzAtBOL  = true;
    zzAtEOF  = false;
    zzEOFDone = false;
    zzEndRead = zzStartRead = 0;
    zzCurrentPos = zzMarkedPos = 0;
    zzFinalHighSurrogate = 0;
    yyline = yychar = yycolumn = 0;
    zzLexicalState = YYINITIAL;
    if (zzBuffer.length > ZZ_BUFFERSIZE)
      zzBuffer = new char[ZZ_BUFFERSIZE];
  }


  /**
   * Returns the current lexical state.
   */
  public final int yystate() {
    return zzLexicalState;
  }


  /**
   * Enters a new lexical state
   *
   * @param newState the new lexical state
   */
  public final void yybegin(int newState) {
    zzLexicalState = newState;
  }


  /**
   * Returns the text matched by the current regular expression.
   */
  public final String yytext() {
    return new String( zzBuffer, zzStartRead, zzMarkedPos-zzStartRead );
  }


  /**
   * Returns the character at position <tt>pos</tt> from the 
   * matched text. 
   * 
   * It is equivalent to yytext().charAt(pos), but faster
   *
   * @param pos the position of the character to fetch. 
   *            A value from 0 to yylength()-1.
   *
   * @return the character at position pos
   */
  public final char yycharat(int pos) {
    return zzBuffer[zzStartRead+pos];
  }


  /**
   * Returns the length of the matched text region.
   */
  public final int yylength() {
    return zzMarkedPos-zzStartRead;
  }


  /**
   * Reports an error that occured while scanning.
   *
   * In a wellformed scanner (no or only correct usage of 
   * yypushback(int) and a match-all fallback rule) this method 
   * will only be called with things that "Can't Possibly Happen".
   * If this method is called, something is seriously wrong
   * (e.g. a JFlex bug producing a faulty scanner etc.).
   *
   * Usual syntax/scanner level error handling should be done
   * in error fallback rules.
   *
   * @param   errorCode  the code of the errormessage to display
   */
  private void zzScanError(int errorCode) {
    String message;
    try {
      message = ZZ_ERROR_MSG[errorCode];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
    }

    throw new Error(message);
  } 


  /**
   * Pushes the specified amount of characters back into the input stream.
   *
   * They will be read again by then next call of the scanning method
   *
   * @param number  the number of characters to be read again.
   *                This number must not be greater than yylength()!
   */
  public void yypushback(int number)  {
    if ( number > yylength() )
      zzScanError(ZZ_PUSHBACK_2BIG);

    zzMarkedPos -= number;
  }


  /**
   * Resumes scanning until the next regular expression is matched,
   * the end of input is encountered or an I/O-Error occurs.
   *
   * @return      the next token
   * @exception   java.io.IOException  if any I/O-Error occurs
   */
  public int yylex() throws java.io.IOException {
    int zzInput;
    int zzAction;

    // cached fields:
    int zzCurrentPosL;
    int zzMarkedPosL;
    int zzEndReadL = zzEndRead;
    char [] zzBufferL = zzBuffer;
    char [] zzCMapL = ZZ_CMAP;

    int [] zzTransL = ZZ_TRANS;
    int [] zzRowMapL = ZZ_ROWMAP;
    int [] zzAttrL = ZZ_ATTRIBUTE;

    while (true) {
      zzMarkedPosL = zzMarkedPos;

      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      zzState = ZZ_LEXSTATE[zzLexicalState];

      // set up zzAction for empty match case:
      int zzAttributes = zzAttrL[zzState];
      if ( (zzAttributes & 1) == 1 ) {
        zzAction = zzState;
      }


      zzForAction: {
        while (true) {
    
          if (zzCurrentPosL < zzEndReadL) {
            zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
            zzCurrentPosL += Character.charCount(zzInput);
          }
          else if (zzAtEOF) {
            zzInput = YYEOF;
            break zzForAction;
          }
          else {
            // store back cached positions
            zzCurrentPos  = zzCurrentPosL;
            zzMarkedPos   = zzMarkedPosL;
            boolean eof = zzRefill();
            // get translated positions and possibly new buffer
            zzCurrentPosL  = zzCurrentPos;
            zzMarkedPosL   = zzMarkedPos;
            zzBufferL      = zzBuffer;
            zzEndReadL     = zzEndRead;
            if (eof) {
              zzInput = YYEOF;
              break zzForAction;
            }
            else {
              zzInput = Character.codePointAt(zzBufferL, zzCurrentPosL, zzEndReadL);
              zzCurrentPosL += Character.charCount(zzInput);
            }
          }
          int zzNext = zzTransL[ zzRowMapL[zzState] + zzCMapL[zzInput] ];
          if (zzNext == -1) break zzForAction;
          zzState = zzNext;

          zzAttributes = zzAttrL[zzState];
          if ( (zzAttributes & 1) == 1 ) {
            zzAction = zzState;
            zzMarkedPosL = zzCurrentPosL;
            if ( (zzAttributes & 8) == 8 ) break zzForAction;
          }

        }
      }

      // store back cached position
      zzMarkedPos = zzMarkedPosL;

      if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
        zzAtEOF = true;
        return YYEOF;
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { writeUnicodeChar(yycharat(0));
            }
          case 28: break;
          case 2: 
            { out.write(yytext());
            }
          case 29: break;
          case 3: 
            { startNewLine();
            }
          case 30: break;
          case 4: 
            { out.write(yycharat(0));
            }
          case 31: break;
          case 5: 
            { out.write("<span class=\"n\">"); out.write(yytext()); out.write("</span>");
            }
          case 32: break;
          case 6: 
            { out.write( "&lt;");
            }
          case 33: break;
          case 7: 
            { out.write( "&gt;");
            }
          case 34: break;
          case 8: 
            { yybegin(STRING);out.write("<span class=\"s\">\"");
            }
          case 35: break;
          case 9: 
            { yybegin(QSTRING);out.write("<span class=\"s\">\'");
            }
          case 36: break;
          case 10: 
            { out.write( "&amp;");
            }
          case 37: break;
          case 11: 
            { yybegin(YYINITIAL); out.write("\"</span>");
            }
          case 38: break;
          case 12: 
            { yybegin(YYINITIAL); out.write("</span>");
    startNewLine();
            }
          case 39: break;
          case 13: 
            { yybegin(YYINITIAL); out.write("'</span>");
            }
          case 40: break;
          case 14: 
            { out.write("\\");
            }
          case 41: break;
          case 15: 
            { String id = yytext();
    writeSymbol(id, Consts.kwd, yyline - 1);
            }
          case 42: break;
          case 16: 
            { yybegin(SCOMMENT);out.write("<span class=\"c\">//");
            }
          case 43: break;
          case 17: 
            { yybegin(COMMENT);out.write("<span class=\"c\">/*");
            }
          case 44: break;
          case 18: 
            { out.write("\\\"");
            }
          case 45: break;
          case 19: 
            { out.write("\\\\");
            }
          case 46: break;
          case 20: 
            { yybegin(YYINITIAL); out.write("*/</span>");
            }
          case 47: break;
          case 21: 
            { out.write("\\\'");
            }
          case 48: break;
          case 22: 
            { out.write("\"\"");
            }
          case 49: break;
          case 23: 
            { String path = yytext();
        out.write("<a href=\""+urlPrefix+"path=");
        out.write(path);
        appendProject();
        out.write("\">");
        out.write(path);
        out.write("</a>");
            }
          case 50: break;
          case 24: 
            { out.write(Util.breadcrumbPath(urlPrefix+"path=",yytext(),'/'));
            }
          case 51: break;
          case 25: 
            { out.write("&lt;");
        String path = yytext();
        path = path.substring(1, path.length() - 1);
        out.write("<a href=\""+urlPrefix+"path=");
        out.write(path);
        appendProject();
        out.write("\">");
        out.write(path);
        out.write("</a>");
        out.write("&gt;");
            }
          case 52: break;
          case 26: 
            { writeEMailAddress(yytext());
            }
          case 53: break;
          case 27: 
            { String url = yytext();
         out.write("<a href=\"");
         out.write(url);out.write("\">");
         out.write(url);out.write("</a>");
            }
          case 54: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}
