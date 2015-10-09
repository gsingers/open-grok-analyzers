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
 * Copyright (c) 2005, 2014, Oracle and/or its affiliates. All rights reserved.
 */

package com.grantingersoll.opengrok.analysis.sh;
import com.grantingersoll.opengrok.analysis.JFlexXref;
import java.io.IOException;
import com.grantingersoll.opengrok.web.Util;
import java.util.Stack;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>/Users/grantingersoll/projects/OpenGrok/src/org/opensolaris/opengrok/analysis/sh/ShXref.lex</tt>
 */
public class ShXref extends JFlexXref {

  /** This character denotes the end of file */
  public static final int YYEOF = -1;

  /** initial size of the lookahead buffer */
  private static final int ZZ_BUFFERSIZE = 16384;

  /** lexical states */
  public static final int YYINITIAL = 0;
  public static final int STRING = 2;
  public static final int SCOMMENT = 4;
  public static final int QSTRING = 6;
  public static final int SUBSHELL = 8;
  public static final int BACKQUOTE = 10;
  public static final int BRACEGROUP = 12;
  public static final int HEREDOC = 14;

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  7,  8, 8
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\1\1\3\1\0\1\1\1\2\22\0\1\1\1\13\1\21"+
    "\1\23\1\6\1\13\1\33\1\22\1\27\1\30\3\13\1\15\1\7"+
    "\1\16\1\10\11\5\1\44\1\32\1\24\1\13\1\34\1\13\1\45"+
    "\5\12\1\43\1\4\1\36\7\4\1\40\2\4\1\42\1\37\3\4"+
    "\1\11\2\4\1\20\1\25\2\35\1\14\1\26\5\12\1\43\1\4"+
    "\1\36\7\4\1\40\2\4\1\42\1\37\3\4\1\11\2\4\1\17"+
    "\1\35\1\31\1\13\6\0\1\0\371\0\1\41\u1ea8\0\1\0\1\0"+
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
    "\11\0\1\1\1\2\2\3\1\4\1\5\2\4\1\5"+
    "\4\4\1\6\1\7\1\10\1\11\1\4\1\12\1\13"+
    "\1\14\1\15\3\4\1\16\1\11\3\4\2\17\1\16"+
    "\1\4\1\16\1\4\1\2\2\20\1\21\10\0\1\2"+
    "\2\0\1\21\1\0\2\22\14\0\1\23\1\0\1\24"+
    "\1\0\1\24\1\5\1\21\1\0\1\5\1\25\2\0"+
    "\1\26\1\24\1\0\1\24\1\27\2\30\3\0\1\31"+
    "\1\5\1\32\4\0\1\32\1\33\3\0\1\34\2\0"+
    "\1\35";

  private static int [] zzUnpackAction() {
    int [] result = new int[114];
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
    "\0\0\0\46\0\114\0\162\0\230\0\276\0\344\0\u010a"+
    "\0\u0130\0\u0156\0\u017c\0\u01a2\0\u0156\0\u01c8\0\u01ee\0\u0214"+
    "\0\u023a\0\u0260\0\u0156\0\u0286\0\u02ac\0\u02d2\0\u0156\0\u0156"+
    "\0\u0156\0\u02f8\0\u031e\0\u0156\0\u0156\0\u0156\0\u0156\0\u0344"+
    "\0\u036a\0\u0390\0\u03b6\0\u0156\0\u03dc\0\u0402\0\u0428\0\u044e"+
    "\0\u0156\0\u0474\0\u049a\0\u0156\0\u04c0\0\u04e6\0\u050c\0\u0532"+
    "\0\u01c8\0\u0558\0\u057e\0\u05a4\0\u023a\0\u05ca\0\u05f0\0\u0616"+
    "\0\u02d2\0\u0156\0\u063c\0\u0662\0\u0286\0\u0688\0\u0156\0\u06ae"+
    "\0\u06d4\0\u0344\0\u06fa\0\u0720\0\u036a\0\u0746\0\u076c\0\u0792"+
    "\0\u03b6\0\u07b8\0\u07de\0\u0474\0\u0156\0\u04c0\0\u0558\0\u0804"+
    "\0\u063c\0\u082a\0\u05f0\0\u0850\0\u0662\0\u0156\0\u0876\0\u089c"+
    "\0\u08c2\0\u06fa\0\u08e8\0\u0746\0\u076c\0\u0156\0\u090e\0\u0934"+
    "\0\u095a\0\u0980\0\u0804\0\u0850\0\u09a6\0\u09cc\0\u09f2\0\u0a18"+
    "\0\u0a3e\0\u0a64\0\u09cc\0\u0a8a\0\u0ab0\0\u0ad6\0\u0156\0\u0afc"+
    "\0\u0b22\0\u0b22";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[114];
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
    "\1\12\1\13\1\14\1\15\1\16\1\17\1\20\1\21"+
    "\1\22\2\16\1\23\1\24\1\21\1\25\1\26\1\23"+
    "\1\27\1\30\1\31\1\32\1\33\1\34\1\35\3\23"+
    "\1\36\1\37\1\23\3\16\1\12\2\16\2\23\1\12"+
    "\1\13\1\14\1\15\1\40\1\41\1\42\2\41\2\40"+
    "\1\23\2\41\1\25\2\23\1\43\2\23\1\44\1\45"+
    "\1\34\4\23\1\36\1\37\1\23\1\46\2\40\1\12"+
    "\1\40\1\47\2\23\1\12\1\13\1\50\1\51\1\40"+
    "\1\41\1\23\2\41\2\40\1\23\2\41\1\25\5\23"+
    "\1\44\6\23\1\36\1\37\1\23\1\46\2\40\1\12"+
    "\1\40\1\47\2\23\1\12\1\13\1\14\1\15\1\40"+
    "\1\41\1\23\2\41\2\40\1\23\2\41\1\25\3\23"+
    "\1\52\1\23\1\44\1\53\5\23\1\36\1\37\1\23"+
    "\1\46\2\40\1\12\1\40\1\47\2\23\1\12\1\13"+
    "\1\14\1\15\1\16\1\17\1\20\1\21\1\22\2\16"+
    "\1\23\1\24\1\21\1\25\1\26\1\23\1\27\1\30"+
    "\1\31\1\32\1\33\1\34\1\35\1\54\2\23\1\36"+
    "\1\37\1\23\3\16\1\12\2\16\2\23\1\12\1\13"+
    "\1\14\1\15\1\16\1\17\1\20\1\21\1\22\2\16"+
    "\1\23\1\24\1\21\1\25\1\26\1\23\1\27\1\30"+
    "\1\31\1\32\1\33\1\54\1\35\3\23\1\36\1\37"+
    "\1\23\3\16\1\12\2\16\2\23\1\12\1\13\1\14"+
    "\1\15\1\16\1\17\1\20\1\21\1\22\2\16\1\23"+
    "\1\24\1\21\1\25\1\26\1\23\1\27\1\30\1\31"+
    "\1\32\1\33\1\34\1\35\2\23\1\55\1\36\1\37"+
    "\1\23\3\16\1\12\2\16\2\23\1\12\1\56\1\14"+
    "\1\15\1\16\1\17\1\20\1\21\1\22\2\16\1\23"+
    "\1\24\1\21\1\25\1\26\1\23\1\27\1\30\1\31"+
    "\1\32\1\33\1\34\1\35\1\23\1\54\1\55\1\36"+
    "\1\37\1\23\3\16\1\12\2\16\2\23\2\57\1\60"+
    "\1\15\42\57\47\0\1\13\47\0\1\15\46\0\2\61"+
    "\1\0\1\62\3\61\1\0\1\61\1\63\1\64\17\0"+
    "\3\61\1\0\2\61\6\0\1\65\1\17\1\0\1\66"+
    "\1\17\2\65\1\0\2\65\20\0\3\65\1\0\2\65"+
    "\6\0\1\67\1\70\2\0\1\70\2\67\1\0\1\67"+
    "\2\0\1\71\1\0\1\27\1\30\1\72\3\0\1\35"+
    "\6\0\3\67\1\0\2\67\6\0\2\65\1\0\1\73"+
    "\3\65\1\0\2\65\20\0\3\65\1\0\2\65\6\0"+
    "\1\65\1\17\1\0\1\66\1\17\1\74\1\65\1\0"+
    "\2\65\20\0\3\65\1\0\2\65\6\0\2\75\1\0"+
    "\1\73\3\75\1\0\1\75\1\65\20\0\3\75\1\0"+
    "\2\75\6\0\1\76\4\0\2\76\23\0\3\76\1\0"+
    "\2\76\3\0\1\77\1\100\1\77\66\0\1\101\27\0"+
    "\1\72\10\0\1\72\1\0\3\72\1\0\4\72\21\0"+
    "\2\102\1\0\1\103\3\102\1\0\2\102\1\64\17\0"+
    "\3\102\1\0\2\102\1\0\1\104\4\0\2\105\1\0"+
    "\1\106\3\105\1\0\2\105\20\0\3\105\1\0\2\105"+
    "\1\0\1\104\4\0\1\107\4\0\2\107\1\0\1\107"+
    "\2\0\1\110\7\0\1\35\6\0\3\107\1\0\2\107"+
    "\3\0\1\111\17\0\1\72\32\0\1\72\12\0\1\72"+
    "\3\0\2\72\23\0\2\102\1\0\1\103\3\102\1\0"+
    "\2\102\1\64\17\0\1\102\1\112\1\102\1\0\2\102"+
    "\1\0\1\104\4\0\2\102\1\0\1\103\3\102\1\0"+
    "\2\102\1\64\17\0\1\102\1\113\1\102\1\0\2\102"+
    "\1\0\1\104\3\0\1\51\43\0\1\114\20\0\1\72"+
    "\45\0\1\115\24\0\1\116\27\0\1\54\15\0\1\56"+
    "\27\0\1\54\14\0\3\57\1\0\45\57\1\15\42\57"+
    "\4\0\1\117\1\63\1\0\1\62\1\63\2\117\1\0"+
    "\2\63\1\64\17\0\3\117\1\0\2\117\6\0\2\63"+
    "\1\0\1\62\3\63\1\0\2\63\1\64\17\0\3\63"+
    "\1\0\2\63\6\0\1\120\4\0\2\120\23\0\3\120"+
    "\1\0\2\120\6\0\1\121\1\122\1\0\1\73\1\122"+
    "\2\121\1\0\2\65\20\0\3\121\1\0\2\121\6\0"+
    "\2\123\2\0\3\123\1\0\1\123\21\0\3\123\1\0"+
    "\2\123\7\0\1\70\1\0\1\124\1\70\41\0\1\121"+
    "\1\65\1\0\1\73\1\65\2\121\1\0\2\65\20\0"+
    "\3\121\1\0\2\121\6\0\1\65\1\125\1\0\1\73"+
    "\1\125\1\65\1\125\1\0\2\65\20\0\3\65\1\0"+
    "\1\65\1\125\6\0\2\76\1\0\4\76\1\0\2\76"+
    "\1\64\17\0\3\76\1\0\2\76\5\0\1\126\43\0"+
    "\1\127\2\0\1\130\4\0\2\130\1\0\1\130\1\127"+
    "\6\0\1\131\11\0\3\130\1\0\2\130\6\0\1\132"+
    "\1\102\1\0\1\103\1\102\2\132\1\0\2\102\1\64"+
    "\17\0\3\132\1\0\2\132\1\0\1\104\4\0\2\133"+
    "\1\0\4\133\1\0\2\133\20\0\3\133\1\0\2\133"+
    "\6\0\1\134\1\105\1\0\1\106\1\105\2\134\1\0"+
    "\2\105\20\0\3\134\1\0\2\134\1\0\1\104\4\0"+
    "\2\135\2\0\3\135\1\0\1\135\21\0\3\135\1\0"+
    "\2\135\3\0\1\136\1\137\1\136\1\140\4\0\2\140"+
    "\1\0\1\140\21\0\3\140\1\0\2\140\6\0\2\102"+
    "\1\0\1\103\3\102\1\0\2\102\1\64\17\0\1\102"+
    "\1\141\1\102\1\0\2\102\1\0\1\104\4\0\2\102"+
    "\1\0\1\103\3\102\1\0\2\102\1\64\17\0\2\102"+
    "\1\142\1\0\2\102\1\0\1\104\4\0\2\143\1\0"+
    "\1\120\3\143\1\0\2\120\1\64\17\0\3\143\1\0"+
    "\2\143\6\0\1\65\1\122\1\0\1\73\1\122\2\65"+
    "\1\0\2\65\20\0\3\65\1\0\2\65\7\0\1\144"+
    "\2\0\1\144\36\0\1\127\2\0\1\130\4\0\2\130"+
    "\1\0\1\130\21\0\3\130\1\0\2\130\6\0\2\145"+
    "\2\0\3\145\1\0\1\145\21\0\3\145\1\0\2\145"+
    "\26\0\1\131\25\0\2\133\1\0\1\146\3\133\1\0"+
    "\2\133\20\0\3\133\1\0\2\133\5\0\1\136\46\0"+
    "\2\147\2\0\3\147\1\0\1\147\21\0\3\147\1\0"+
    "\2\147\6\0\2\102\1\0\1\103\3\102\1\0\2\102"+
    "\1\64\17\0\2\102\1\150\1\0\2\102\1\0\1\104"+
    "\4\0\2\102\1\0\1\103\3\102\1\0\2\102\1\64"+
    "\17\0\3\102\1\0\2\102\1\151\1\104\1\0\1\152"+
    "\2\0\2\145\2\0\3\145\1\0\1\145\21\0\3\145"+
    "\1\0\2\145\6\0\2\153\1\0\4\153\1\0\2\153"+
    "\20\0\3\153\1\0\2\153\6\0\2\147\2\0\3\147"+
    "\1\0\1\147\3\0\1\154\15\0\3\147\1\0\2\147"+
    "\6\0\2\102\1\0\1\103\3\102\1\0\2\102\1\64"+
    "\17\0\3\102\1\155\1\142\1\102\1\151\1\104\16\0"+
    "\1\156\30\0\1\152\65\0\1\157\70\0\1\151\17\0"+
    "\1\160\33\0\13\161\6\0\1\161\4\0\2\161\2\0"+
    "\3\161\1\0\4\161\4\0\2\162\2\161\3\162\3\161"+
    "\1\162\6\0\1\161\4\0\2\161\2\0\3\162\1\0"+
    "\2\162\2\161";

  private static int [] zzUnpackTrans() {
    int [] result = new int[2888];
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
    "\11\0\1\11\2\1\1\11\5\1\1\11\3\1\3\11"+
    "\2\1\4\11\4\1\1\11\4\1\1\11\2\1\1\11"+
    "\5\1\10\0\1\11\2\0\1\1\1\0\1\11\1\1"+
    "\14\0\1\11\1\0\1\1\1\0\3\1\1\0\1\1"+
    "\1\11\2\0\2\1\1\0\2\1\1\11\1\1\3\0"+
    "\3\1\4\0\2\1\3\0\1\11\2\0\1\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[114];
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
  private final Stack<Integer> stateStack = new Stack<Integer>();
  private final Stack<String> styleStack = new Stack<String>();

  // State variables for the HEREDOC state. They tell what the stop word is,
  // and whether leading tabs should be removed from the input lines before
  // comparing with the stop word.
  private String heredocStopWord;
  private boolean heredocStripLeadingTabs;

  @Override
  public void reInit(char[] contents, int length) {
    super.reInit(contents, length);
    stateStack.clear();
    styleStack.clear();
  }

  // TODO move this into an include file when bug #16053 is fixed
  @Override
  protected int getLineNumber() { return yyline; }
  @Override
  protected void setLineNumber(int x) { yyline = x; }

  private void pushstate(int state, String style) throws IOException {
    if (!styleStack.empty()) {
      out.write("</span>");
    }
    if (style == null) {
      out.write("<span>");
    } else {
      out.write("<span class=\"" + style + "\">");
    }
    stateStack.push(yystate());
    styleStack.push(style);
    yybegin(state);
  }

  private void popstate() throws IOException {
    out.write("</span>");
    yybegin(stateStack.pop());
    styleStack.pop();
    if (!styleStack.empty()) {
      String style = styleStack.peek();
      if (style == null) {
        out.write("<span>");
      } else {
        out.write("<span class=\"" + style + "\">");
      }
    }
  }

  /**
   * Check the contents of a line to see if it matches the stop word for a
   * here-document.
   *
   * @param line a line in the input file
   * @return true if the line terminates a here-document, false otherwise
   */
  private boolean isHeredocStopWord(String line) {
    // Skip leading tabs if heredocStripLeadingTabs is true.
    int i = 0;
    while (heredocStripLeadingTabs &&
              i < line.length() && line.charAt(i) == '\t') {
      i++;
    }

    // Compare remaining characters on the line with the stop word.
    return line.substring(i).equals(heredocStopWord);
  }



  /**
   * Creates a new scanner
   *
   * @param   in  the java.io.Reader to read input from.
   */
  public ShXref(java.io.Reader in) {
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
    while (i < 174) {
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

      if (zzMarkedPosL > zzStartRead) {
        switch (zzBufferL[zzMarkedPosL-1]) {
        case '\n':
        case '\u000B':
        case '\u000C':
        case '\u0085':
        case '\u2028':
        case '\u2029':
          zzAtBOL = true;
          break;
        case '\r': 
          if (zzMarkedPosL < zzEndReadL)
            zzAtBOL = zzBufferL[zzMarkedPosL] != '\n';
          else if (zzAtEOF)
            zzAtBOL = false;
          else {
            boolean eof = zzRefill();
            zzMarkedPosL = zzMarkedPos;
            zzEndReadL = zzEndRead;
            zzBufferL = zzBuffer;
            if (eof) 
              zzAtBOL = false;
            else 
              zzAtBOL = zzBufferL[zzMarkedPosL] != '\n';
          }
          break;
        default:
          zzAtBOL = false;
        }
      }
      zzAction = -1;

      zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;
  
      if (zzAtBOL)
        zzState = ZZ_LEXSTATE[zzLexicalState+1];
      else
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
              {
                // If we reach EOF while being in a nested state, pop all the way up
    // the initial state so that we close open HTML tags.
    while (!stateStack.isEmpty()) {
        popstate();
    }
    return YYEOF;
              }
      }
      else {
        switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
          case 1: 
            { writeUnicodeChar(yycharat(0));
            }
          case 30: break;
          case 2: 
            { out.write(yytext());
            }
          case 31: break;
          case 3: 
            { startNewLine();
            }
          case 32: break;
          case 4: 
            { out.write(yycharat(0));
            }
          case 33: break;
          case 5: 
            { out.write("<span class=\"n\">"); out.write(yytext()); out.write("</span>");
            }
          case 34: break;
          case 6: 
            { pushstate(STRING, "s"); out.write(yytext());
            }
          case 35: break;
          case 7: 
            { pushstate(QSTRING, "s"); out.write(yytext());
            }
          case 36: break;
          case 8: 
            { pushstate(SCOMMENT, "c"); out.write(yytext());
            }
          case 37: break;
          case 9: 
            { out.write( "&lt;");
            }
          case 38: break;
          case 10: 
            { pushstate(BACKQUOTE, null); out.write(yytext());
            }
          case 39: break;
          case 11: 
            { pushstate(SUBSHELL, null); out.write(yytext());
            }
          case 40: break;
          case 12: 
            { out.write( "&amp;");
            }
          case 41: break;
          case 13: 
            { out.write( "&gt;");
            }
          case 42: break;
          case 14: 
            { out.write(yytext()); popstate();
            }
          case 43: break;
          case 15: 
            { popstate();
     startNewLine();
            }
          case 44: break;
          case 16: 
            { String line = yytext();
    if (isHeredocStopWord(line)) {
      popstate();
    }
    out.write(Util.htmlize(line));
            }
          case 45: break;
          case 17: 
            { String id = yytext();
    writeSymbol(id, Consts.shkwd, yyline);
            }
          case 46: break;
          case 18: 
            // lookahead expression with fixed lookahead length
            zzMarkedPos = Character.offsetByCodePoints
                (zzBufferL, zzStartRead, zzEndRead - zzStartRead, zzMarkedPos, -1);
            { pushstate(BRACEGROUP, null); out.write(yytext());
            }
          case 47: break;
          case 19: 
            { out.write("\\'");
            }
          case 48: break;
          case 20: 
            { String path = yytext();
    out.write("<a href=\""+urlPrefix+"path=");
    out.write(path);
    appendProject();
    out.write("\">");
    out.write(path);
    out.write("</a>");
            }
          case 49: break;
          case 21: 
            // lookahead expression with fixed lookahead length
            zzMarkedPos = Character.offsetByCodePoints
                (zzBufferL, zzStartRead, zzEndRead - zzStartRead, zzMarkedPos, -2);
            { pushstate(BRACEGROUP, null); out.write(yytext());
            }
          case 50: break;
          case 22: 
            { out.write(Util.htmlize(yytext()));
            }
          case 51: break;
          case 23: 
            { String id = yytext();
    out.write("<a href=\"");
    out.write(urlPrefix);
    out.write("refs=");
    out.write(id);
    appendProject();
    out.write("\">");
    out.write(id);
    out.write("</a>");
            }
          case 52: break;
          case 24: 
            // lookahead expression with fixed base length
            zzMarkedPos = Character.offsetByCodePoints
                (zzBufferL, zzStartRead, zzEndRead - zzStartRead, zzStartRead, 2);
            { pushstate(BRACEGROUP, null); out.write(yytext());
            }
          case 53: break;
          case 25: 
            { out.write(Util.breadcrumbPath(urlPrefix+"path=",yytext(),'/'));
            }
          case 54: break;
          case 26: 
            { String text = yytext();
   out.write(Util.htmlize(text));

   heredocStripLeadingTabs = (text.charAt(2) == '-');
   heredocStopWord = text.substring(heredocStripLeadingTabs ? 3 : 2).trim();
   pushstate(HEREDOC, "s");
            }
          case 55: break;
          case 27: 
            { writeEMailAddress(yytext());
            }
          case 56: break;
          case 28: 
            { out.write(yytext()); pushstate(STRING, "s");
            }
          case 57: break;
          case 29: 
            { String url = yytext();
    out.write("<a href=\"");
    out.write(url);out.write("\">");
    out.write(url);out.write("</a>");
            }
          case 58: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}