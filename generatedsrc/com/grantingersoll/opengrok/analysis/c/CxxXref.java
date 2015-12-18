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
 * Copyright (c) 2008, 2011, Oracle and/or its affiliates. All rights reserved.
 */

/*
 * Cross reference a C++ file
 */

package com.grantingersoll.opengrok.analysis.c;
import com.grantingersoll.opengrok.analysis.JFlexXref;
import java.io.IOException;
import java.io.Writer;
import java.io.Reader;
import com.grantingersoll.opengrok.web.Util;


/**
 * This class is a scanner generated by 
 * <a href="http://www.jflex.de/">JFlex</a> 1.6.1
 * from the specification file <tt>/Users/grantingersoll/projects/OpenGrok/src/com.grantingersoll.opengrok/analysis/c/CxxXref.lex</tt>
 */
public class CxxXref extends JFlexXref {

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

  /**
   * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
   * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
   *                  at the beginning of a line
   * l is of the form l = 2*k, k a non negative integer
   */
  private static final int ZZ_LEXSTATE[] = { 
     0,  0,  1,  1,  2,  2,  3,  3,  4, 4
  };

  /** 
   * Translates characters to character classes
   */
  private static final String ZZ_CMAP_PACKED = 
    "\11\0\1\1\1\3\1\0\1\1\1\2\22\0\1\1\1\6\1\46"+
    "\1\53\2\6\1\52\1\47\2\53\1\50\1\33\1\6\1\10\1\11"+
    "\1\37\1\40\11\5\1\56\1\6\1\44\1\6\1\45\1\6\1\57"+
    "\1\21\1\41\1\13\1\34\1\42\1\17\1\4\1\24\1\36\1\20"+
    "\1\4\1\32\1\31\1\16\1\15\1\23\2\4\1\55\1\26\1\43"+
    "\1\22\1\4\1\30\2\4\1\53\1\51\2\53\1\7\1\53\1\21"+
    "\1\41\1\14\1\34\1\42\1\17\1\4\1\25\1\36\1\20\1\4"+
    "\1\32\1\31\1\16\1\15\1\23\2\4\1\12\1\27\1\43\1\22"+
    "\1\4\1\30\2\4\3\53\1\6\261\0\2\35\115\0\1\54\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\uffff\0\ufe90\0";

  /** 
   * Translates characters to character classes
   */
  private static final char [] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

  /** 
   * Translates DFA states to action switch labels.
   */
  private static final int [] ZZ_ACTION = zzUnpackAction();

  private static final String ZZ_ACTION_PACKED_0 =
    "\5\0\1\1\1\2\2\3\1\4\1\5\3\4\1\6"+
    "\1\7\1\10\1\11\1\12\5\4\1\6\1\13\2\4"+
    "\1\2\2\14\1\15\1\4\1\16\1\0\1\5\1\0"+
    "\1\17\1\20\16\0\1\21\1\22\1\23\1\0\1\24"+
    "\2\5\1\0\1\5\6\0\1\25\1\0\1\25\3\0"+
    "\1\25\1\0\1\25\6\0\1\2\1\5\1\26\27\0"+
    "\1\27\20\0\1\25\2\0\1\25\1\0\1\30\14\0"+
    "\1\31";

  private static int [] zzUnpackAction() {
    int [] result = new int[145];
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
    "\0\0\0\60\0\140\0\220\0\300\0\360\0\u0120\0\u0150"+
    "\0\360\0\u0180\0\u01b0\0\360\0\u01e0\0\u0210\0\u0240\0\360"+
    "\0\360\0\360\0\360\0\u0270\0\u02a0\0\u02d0\0\u0300\0\u0330"+
    "\0\360\0\u0360\0\u0390\0\u03c0\0\u03f0\0\u0420\0\360\0\u0450"+
    "\0\u0480\0\u0180\0\u04b0\0\u04e0\0\u0510\0\360\0\360\0\u0540"+
    "\0\u0570\0\u05a0\0\u05d0\0\u0600\0\u0270\0\u0630\0\u0660\0\u0690"+
    "\0\u02a0\0\u06c0\0\u06f0\0\u0720\0\u0750\0\360\0\360\0\360"+
    "\0\u0780\0\360\0\u07b0\0\u07e0\0\u0810\0\u0840\0\u0870\0\u08a0"+
    "\0\u08d0\0\u0900\0\u0930\0\u0960\0\u0270\0\u0990\0\u0990\0\u09c0"+
    "\0\u09f0\0\u0a20\0\u0a20\0\u0a50\0\u0a50\0\u0a80\0\u0ab0\0\u0ae0"+
    "\0\u0b10\0\u0b40\0\u0b70\0\360\0\u0ba0\0\360\0\u0bd0\0\u0c00"+
    "\0\u0c30\0\u0c60\0\u0c90\0\u0cc0\0\u0cf0\0\u0d20\0\u0d50\0\u0d80"+
    "\0\u0db0\0\u0de0\0\u0e10\0\u0e40\0\u0e70\0\u0ea0\0\u0ed0\0\u0f00"+
    "\0\u0f30\0\u0f60\0\u0f90\0\u0fc0\0\u0ff0\0\u1020\0\u1050\0\u1080"+
    "\0\u10b0\0\u10e0\0\u1110\0\u1140\0\u1170\0\u11a0\0\u11d0\0\u1200"+
    "\0\u1230\0\u1260\0\u1290\0\u12c0\0\u12f0\0\u1320\0\360\0\u1350"+
    "\0\u1380\0\u0f90\0\u13b0\0\u1050\0\u13e0\0\u1410\0\u1440\0\u1470"+
    "\0\u14a0\0\u14d0\0\u1500\0\u1530\0\u1560\0\u1590\0\u15c0\0\u15f0"+
    "\0\u15f0";

  private static int [] zzUnpackRowMap() {
    int [] result = new int[145];
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
    "\1\6\1\7\1\10\1\11\1\12\1\13\1\14\1\12"+
    "\2\14\21\12\1\14\1\12\1\6\1\12\1\15\1\16"+
    "\3\12\1\17\1\20\1\21\1\22\2\14\1\23\1\14"+
    "\1\6\1\12\2\14\1\6\1\7\1\10\1\11\1\24"+
    "\1\25\1\14\3\25\5\24\1\26\4\24\2\27\5\24"+
    "\1\14\1\24\1\6\1\24\1\30\1\25\3\24\1\31"+
    "\1\20\1\32\2\14\1\33\1\23\1\14\1\6\1\24"+
    "\2\14\1\6\1\7\1\10\1\11\1\24\1\25\1\14"+
    "\3\25\5\24\1\26\4\24\2\27\5\24\1\14\1\24"+
    "\1\6\1\24\1\30\1\25\3\24\1\31\1\20\2\14"+
    "\1\34\1\14\1\23\1\14\1\6\1\24\2\14\1\6"+
    "\1\35\1\36\1\37\1\24\1\25\1\14\3\25\5\24"+
    "\1\26\4\24\2\27\5\24\1\14\1\24\1\6\1\24"+
    "\1\30\1\25\3\24\1\31\1\20\4\14\1\23\1\14"+
    "\1\6\1\24\2\14\1\6\1\7\1\10\1\11\1\24"+
    "\1\25\1\14\3\25\5\24\1\26\4\24\2\27\5\24"+
    "\1\14\1\24\1\6\1\24\1\30\1\25\3\24\1\31"+
    "\1\20\1\14\1\40\1\14\1\41\1\23\1\14\1\6"+
    "\1\24\2\14\61\0\1\7\1\10\1\11\57\0\1\11"+
    "\60\0\2\42\1\0\1\42\2\0\21\42\1\0\1\42"+
    "\1\0\1\42\1\0\4\42\11\0\1\42\7\0\1\13"+
    "\3\0\1\43\5\0\1\44\12\0\1\44\1\0\1\44"+
    "\3\0\1\13\1\0\1\45\1\44\53\0\1\46\10\0"+
    "\1\47\14\0\1\50\3\0\1\43\16\0\1\51\7\0"+
    "\1\50\23\0\1\52\2\0\1\53\2\0\21\52\1\0"+
    "\1\52\1\0\1\52\1\54\1\0\3\52\11\0\1\52"+
    "\6\0\2\55\1\0\2\55\1\56\21\55\1\0\1\55"+
    "\1\0\1\55\1\57\4\55\11\0\1\55\1\0\1\60"+
    "\4\0\2\61\1\0\24\61\1\0\1\61\1\0\1\61"+
    "\1\0\4\61\11\0\1\61\1\0\1\60\4\0\2\55"+
    "\1\0\2\55\1\56\14\55\2\62\3\55\1\0\1\55"+
    "\1\0\1\55\1\57\4\55\11\0\1\55\1\0\1\60"+
    "\4\0\2\55\1\0\2\55\1\56\14\55\2\63\3\55"+
    "\1\0\1\55\1\0\1\55\1\57\4\55\11\0\1\55"+
    "\1\0\1\60\4\0\1\64\5\0\21\64\1\0\1\64"+
    "\1\0\1\64\2\0\3\64\11\0\1\64\3\0\1\65"+
    "\124\0\1\66\2\0\1\67\45\0\1\70\21\0\1\35"+
    "\1\36\1\37\57\0\1\37\55\0\1\71\125\0\1\72"+
    "\1\0\1\67\13\0\1\73\32\0\1\73\36\0\1\44"+
    "\12\0\1\44\1\0\1\44\6\0\1\44\21\0\1\74"+
    "\2\0\1\75\22\0\1\75\4\0\1\74\24\0\1\50"+
    "\3\0\1\43\26\0\1\50\24\0\1\76\5\0\2\76"+
    "\2\0\1\76\1\0\1\76\12\0\1\76\3\0\3\76"+
    "\21\0\2\77\1\0\1\77\1\100\1\101\21\77\1\0"+
    "\1\77\1\0\1\77\1\102\4\77\11\0\1\77\6\0"+
    "\2\103\1\0\1\103\2\0\21\103\1\0\1\103\1\0"+
    "\1\103\1\0\4\103\11\0\1\103\6\0\1\104\5\0"+
    "\21\104\1\0\1\104\1\0\1\104\2\0\3\104\11\0"+
    "\1\104\6\0\2\55\1\0\2\55\1\56\1\105\1\106"+
    "\1\107\3\55\1\110\2\55\1\111\1\112\1\113\1\114"+
    "\1\115\1\116\2\55\1\0\1\117\1\0\1\55\1\57"+
    "\4\55\11\0\1\55\1\0\1\60\4\0\1\120\5\0"+
    "\21\120\1\0\1\120\1\0\1\120\2\0\3\120\11\0"+
    "\1\120\6\0\2\121\1\0\24\121\1\0\1\121\1\0"+
    "\1\121\1\0\4\121\11\0\1\121\6\0\2\55\1\0"+
    "\2\55\1\56\11\55\1\122\7\55\1\0\1\55\1\0"+
    "\1\55\1\57\4\55\11\0\1\55\1\0\1\60\4\0"+
    "\2\55\1\0\2\55\1\56\14\55\2\123\3\55\1\0"+
    "\1\55\1\0\1\55\1\57\4\55\11\0\1\55\1\0"+
    "\1\60\4\0\2\64\1\0\24\64\1\0\1\64\1\0"+
    "\1\64\1\57\4\64\11\0\1\64\3\0\1\65\44\0"+
    "\1\124\12\0\1\71\45\0\1\124\15\0\1\73\11\0"+
    "\1\44\12\0\1\44\1\0\1\44\3\0\1\73\1\0"+
    "\1\45\1\44\21\0\1\74\11\0\1\44\12\0\1\44"+
    "\1\0\1\44\3\0\1\74\2\0\1\44\21\0\1\74"+
    "\32\0\1\74\24\0\1\76\5\0\2\76\2\0\1\76"+
    "\1\0\1\76\10\0\1\44\1\0\1\76\3\0\2\76"+
    "\1\125\1\44\20\0\2\77\1\0\1\77\1\100\1\101"+
    "\21\77\1\0\1\77\1\0\1\77\1\102\4\77\1\0"+
    "\1\126\7\0\1\77\6\0\2\100\1\0\2\100\1\101"+
    "\21\100\1\0\1\100\1\0\1\100\1\102\4\100\11\0"+
    "\1\100\6\0\2\100\1\0\2\100\1\101\1\127\1\130"+
    "\1\131\3\100\1\132\2\100\1\133\1\134\1\135\1\136"+
    "\1\137\1\140\2\100\1\0\1\141\1\0\1\100\1\102"+
    "\4\100\11\0\1\100\6\0\1\142\5\0\21\142\1\0"+
    "\1\142\1\0\1\142\2\0\3\142\11\0\1\142\6\0"+
    "\2\103\1\0\1\103\2\0\21\103\1\0\1\103\1\0"+
    "\1\103\1\0\4\103\1\0\1\126\7\0\1\103\6\0"+
    "\2\104\1\0\24\104\1\0\1\104\1\0\1\104\1\102"+
    "\4\104\11\0\1\104\6\0\2\55\1\0\2\55\1\56"+
    "\1\55\2\105\1\143\5\55\1\144\4\55\1\145\2\55"+
    "\1\146\1\55\1\0\1\55\1\57\4\55\11\0\1\55"+
    "\1\0\1\60\4\0\2\55\1\0\2\55\1\56\7\55"+
    "\1\147\11\55\1\0\1\55\1\0\1\55\1\57\4\55"+
    "\11\0\1\55\1\0\1\60\4\0\2\55\1\0\2\55"+
    "\1\56\7\55\1\150\10\55\1\105\1\0\1\55\1\0"+
    "\1\55\1\57\4\55\11\0\1\55\1\0\1\60\4\0"+
    "\2\55\1\0\2\55\1\56\11\55\1\144\2\105\2\151"+
    "\1\145\2\55\1\146\1\55\1\0\1\55\1\57\4\55"+
    "\11\0\1\55\1\0\1\60\4\0\2\55\1\0\2\55"+
    "\1\56\16\55\1\152\2\55\1\0\1\55\1\0\1\55"+
    "\1\57\4\55\11\0\1\55\1\0\1\60\4\0\2\55"+
    "\1\0\2\55\1\56\17\55\1\153\1\55\1\0\1\55"+
    "\1\0\1\55\1\57\4\55\11\0\1\55\1\0\1\60"+
    "\4\0\2\55\1\0\2\55\1\56\21\55\1\0\1\55"+
    "\1\154\1\155\1\57\4\55\11\0\1\55\1\0\1\60"+
    "\4\0\2\156\1\0\3\120\21\156\1\0\1\156\1\0"+
    "\1\156\1\0\4\156\11\0\1\156\6\0\2\121\1\0"+
    "\2\121\1\157\21\121\1\0\1\121\1\0\1\121\1\0"+
    "\4\121\11\0\1\121\6\0\2\55\1\0\2\55\1\56"+
    "\21\55\1\0\1\55\1\0\1\55\1\57\4\55\11\0"+
    "\1\55\1\160\1\60\4\0\2\55\1\0\2\55\1\56"+
    "\11\55\1\161\7\55\1\0\1\55\1\0\1\55\1\57"+
    "\4\55\11\0\1\55\1\0\1\60\5\0\1\76\2\0"+
    "\1\75\2\0\2\76\2\0\1\76\1\0\1\76\10\0"+
    "\1\44\1\75\1\76\3\0\2\76\1\125\1\44\20\0"+
    "\2\100\1\0\2\100\1\101\21\100\1\0\1\100\1\0"+
    "\1\100\1\102\4\100\1\0\1\126\7\0\1\100\6\0"+
    "\2\100\1\0\2\100\1\101\1\100\2\127\1\162\5\100"+
    "\1\163\4\100\1\164\2\100\1\165\1\100\1\0\1\100"+
    "\1\102\4\100\11\0\1\100\6\0\2\100\1\0\2\100"+
    "\1\101\1\100\2\127\1\162\5\100\1\163\4\100\1\164"+
    "\2\100\1\165\1\100\1\0\1\100\1\102\4\100\1\0"+
    "\1\126\7\0\1\100\6\0\2\100\1\0\2\100\1\101"+
    "\7\100\1\166\11\100\1\0\1\100\1\0\1\100\1\102"+
    "\4\100\11\0\1\100\6\0\2\100\1\0\2\100\1\101"+
    "\7\100\1\167\10\100\1\127\1\0\1\100\1\0\1\100"+
    "\1\102\4\100\11\0\1\100\6\0\2\100\1\0\2\100"+
    "\1\101\11\100\1\163\2\127\2\170\1\164\2\100\1\165"+
    "\1\100\1\0\1\100\1\102\4\100\11\0\1\100\6\0"+
    "\2\100\1\0\2\100\1\101\11\100\1\163\2\127\2\170"+
    "\1\164\2\100\1\165\1\100\1\0\1\100\1\102\4\100"+
    "\1\0\1\126\7\0\1\100\6\0\2\100\1\0\2\100"+
    "\1\101\16\100\1\171\2\100\1\0\1\100\1\0\1\100"+
    "\1\102\4\100\11\0\1\100\6\0\2\100\1\0\2\100"+
    "\1\101\16\100\1\171\2\100\1\0\1\100\1\0\1\100"+
    "\1\102\4\100\1\0\1\126\7\0\1\100\6\0\2\100"+
    "\1\0\2\100\1\101\17\100\1\172\1\100\1\0\1\100"+
    "\1\0\1\100\1\102\4\100\11\0\1\100\6\0\2\100"+
    "\1\0\2\100\1\101\21\100\1\0\1\100\1\173\1\174"+
    "\1\102\4\100\11\0\1\100\6\0\2\175\1\0\3\142"+
    "\21\175\1\0\1\175\1\0\1\175\1\0\4\175\11\0"+
    "\1\175\6\0\2\55\1\0\2\55\1\56\4\55\1\176"+
    "\14\55\1\0\1\55\1\0\1\55\1\57\4\55\11\0"+
    "\1\55\1\0\1\60\4\0\2\55\1\0\2\55\1\56"+
    "\11\55\1\105\7\55\1\0\1\55\1\0\1\55\1\57"+
    "\4\55\11\0\1\55\1\0\1\60\4\0\2\55\1\0"+
    "\2\55\1\56\16\55\1\105\2\55\1\0\1\55\1\0"+
    "\1\55\1\57\4\55\11\0\1\55\1\0\1\60\33\0"+
    "\1\177\30\0\2\55\1\0\2\55\1\56\10\55\1\200"+
    "\10\55\1\0\1\55\1\0\1\55\1\57\4\55\11\0"+
    "\1\55\1\0\1\60\4\0\2\55\1\0\2\55\1\56"+
    "\14\55\2\201\3\55\1\0\1\55\1\0\1\55\1\57"+
    "\4\55\11\0\1\55\1\0\1\60\4\0\2\55\1\0"+
    "\2\55\1\56\17\55\1\202\1\55\1\0\1\55\1\0"+
    "\1\55\1\57\4\55\11\0\1\55\1\0\1\60\4\0"+
    "\2\55\1\0\2\55\1\56\14\55\2\105\3\55\1\0"+
    "\1\55\1\0\1\55\1\57\4\55\11\0\1\55\1\0"+
    "\1\60\4\0\2\55\1\0\2\55\1\56\20\55\1\105"+
    "\1\0\1\55\1\0\1\55\1\57\4\55\11\0\1\55"+
    "\1\0\1\60\17\0\1\203\44\0\2\55\1\0\2\55"+
    "\1\56\5\55\1\176\13\55\1\0\1\55\1\0\1\55"+
    "\1\57\4\55\11\0\1\55\1\0\1\60\4\0\2\156"+
    "\1\0\3\120\21\156\1\0\1\156\1\0\1\156\1\57"+
    "\4\156\11\0\1\156\6\0\2\204\1\0\24\204\1\0"+
    "\1\204\1\0\1\204\1\0\4\204\11\0\1\204\41\0"+
    "\1\205\24\0\2\55\1\0\2\55\1\56\1\122\20\55"+
    "\1\0\1\55\1\0\1\55\1\57\4\55\10\0\1\206"+
    "\1\122\1\160\1\60\4\0\2\100\1\0\2\100\1\101"+
    "\4\100\1\207\14\100\1\0\1\100\1\0\1\100\1\102"+
    "\4\100\11\0\1\100\6\0\2\100\1\0\2\100\1\101"+
    "\11\100\1\127\7\100\1\0\1\100\1\0\1\100\1\102"+
    "\4\100\11\0\1\100\6\0\2\100\1\0\2\100\1\101"+
    "\16\100\1\127\2\100\1\0\1\100\1\0\1\100\1\102"+
    "\4\100\11\0\1\100\35\0\1\210\30\0\2\100\1\0"+
    "\2\100\1\101\10\100\1\211\10\100\1\0\1\100\1\0"+
    "\1\100\1\102\4\100\11\0\1\100\6\0\2\100\1\0"+
    "\2\100\1\101\14\100\2\212\3\100\1\0\1\100\1\0"+
    "\1\100\1\102\4\100\11\0\1\100\6\0\2\100\1\0"+
    "\2\100\1\101\17\100\1\213\1\100\1\0\1\100\1\0"+
    "\1\100\1\102\4\100\11\0\1\100\6\0\2\100\1\0"+
    "\2\100\1\101\14\100\2\127\3\100\1\0\1\100\1\0"+
    "\1\100\1\102\4\100\11\0\1\100\6\0\2\100\1\0"+
    "\2\100\1\101\20\100\1\127\1\0\1\100\1\0\1\100"+
    "\1\102\4\100\11\0\1\100\21\0\1\214\44\0\2\100"+
    "\1\0\2\100\1\101\5\100\1\207\13\100\1\0\1\100"+
    "\1\0\1\100\1\102\4\100\11\0\1\100\6\0\2\175"+
    "\1\0\3\142\21\175\1\0\1\175\1\0\1\175\1\102"+
    "\4\175\1\0\1\126\7\0\1\175\6\0\2\55\1\0"+
    "\2\55\1\56\5\55\1\105\13\55\1\0\1\55\1\0"+
    "\1\55\1\57\4\55\11\0\1\55\1\0\1\60\4\0"+
    "\2\55\1\0\2\55\1\56\7\55\1\105\11\55\1\0"+
    "\1\55\1\0\1\55\1\57\4\55\11\0\1\55\1\0"+
    "\1\60\4\0\2\55\1\0\2\55\1\56\1\55\2\215"+
    "\16\55\1\0\1\55\1\0\1\55\1\57\4\55\11\0"+
    "\1\55\1\0\1\60\17\0\1\177\77\0\1\216\76\0"+
    "\1\160\5\0\2\100\1\0\2\100\1\101\5\100\1\127"+
    "\13\100\1\0\1\100\1\0\1\100\1\102\4\100\11\0"+
    "\1\100\47\0\1\126\16\0\2\100\1\0\2\100\1\101"+
    "\7\100\1\127\11\100\1\0\1\100\1\0\1\100\1\102"+
    "\4\100\11\0\1\100\6\0\2\100\1\0\2\100\1\101"+
    "\1\100\2\217\16\100\1\0\1\100\1\0\1\100\1\102"+
    "\4\100\11\0\1\100\6\0\2\100\1\0\2\100\1\101"+
    "\20\100\1\127\1\0\1\100\1\0\1\100\1\102\4\100"+
    "\1\0\1\126\7\0\1\100\21\0\1\210\44\0\2\55"+
    "\1\0\2\55\1\56\12\55\2\105\5\55\1\0\1\55"+
    "\1\0\1\55\1\57\4\55\11\0\1\55\1\0\1\60"+
    "\4\0\31\220\1\0\6\220\4\0\3\220\2\0\3\220"+
    "\4\0\2\100\1\0\2\100\1\101\12\100\2\127\5\100"+
    "\1\0\1\100\1\0\1\100\1\102\4\100\11\0\1\100"+
    "\6\0\2\221\4\220\21\221\1\220\1\221\1\0\6\221"+
    "\4\0\3\220\2\0\1\221\2\220";

  private static int [] zzUnpackTrans() {
    int [] result = new int[5664];
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
    "\5\0\1\11\2\1\1\11\2\1\1\11\3\1\4\11"+
    "\5\1\1\11\5\1\1\11\3\1\1\0\1\1\1\0"+
    "\2\11\16\0\3\11\1\0\1\11\2\1\1\0\1\1"+
    "\6\0\1\1\1\0\1\1\3\0\1\1\1\0\1\1"+
    "\6\0\1\11\1\1\1\11\27\0\1\1\20\0\1\11"+
    "\2\0\1\1\1\0\1\1\14\0\1\1";

  private static int [] zzUnpackAttribute() {
    int [] result = new int[145];
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
  public CxxXref(java.io.Reader in) {
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
    while (i < 212) {
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
          case 26: break;
          case 2: 
            { out.write(yytext());
            }
          case 27: break;
          case 3: 
            { startNewLine();
            }
          case 28: break;
          case 4: 
            { out.write(yycharat(0));
            }
          case 29: break;
          case 5: 
            { out.write("<span class=\"n\">"); out.write(yytext()); out.write("</span>");
            }
          case 30: break;
          case 6: 
            { out.write( "&lt;");
            }
          case 31: break;
          case 7: 
            { out.write( "&gt;");
            }
          case 32: break;
          case 8: 
            { yybegin(STRING);out.write("<span class=\"s\">\"");
            }
          case 33: break;
          case 9: 
            { yybegin(QSTRING);out.write("<span class=\"s\">\'");
            }
          case 34: break;
          case 10: 
            { out.write( "&amp;");
            }
          case 35: break;
          case 11: 
            { yybegin(YYINITIAL); out.write("\"</span>");
            }
          case 36: break;
          case 12: 
            { yybegin(YYINITIAL); out.write("</span>");
                  startNewLine();
            }
          case 37: break;
          case 13: 
            { yybegin(YYINITIAL); out.write("'</span>");
            }
          case 38: break;
          case 14: 
            { String id = yytext();
    writeSymbol(id, CxxConsts.kwd, yyline);
            }
          case 39: break;
          case 15: 
            { yybegin(SCOMMENT);out.write("<span class=\"c\">//");
            }
          case 40: break;
          case 16: 
            { yybegin(COMMENT);out.write("<span class=\"c\">/*");
            }
          case 41: break;
          case 17: 
            { out.write("\\\"");
            }
          case 42: break;
          case 18: 
            { out.write("\\\\");
            }
          case 43: break;
          case 19: 
            { yybegin(YYINITIAL); out.write("*/</span>");
            }
          case 44: break;
          case 20: 
            { out.write("\\\'");
            }
          case 45: break;
          case 21: 
            { String path = yytext();
        out.write("<a href=\""+urlPrefix+"path=");
        out.write(path);
        appendProject();
        out.write("\">");
        out.write(path);
        out.write("</a>");
            }
          case 46: break;
          case 22: 
            { out.write("&lt;");
        String path = yytext().substring(1, yylength() - 1);
        out.write(Util.breadcrumbPath(urlPrefix + "path=", path));
        out.write("&gt;");
            }
          case 47: break;
          case 23: 
            { out.write(Util.breadcrumbPath(urlPrefix+"path=",yytext(),'/'));
            }
          case 48: break;
          case 24: 
            { writeEMailAddress(yytext());
            }
          case 49: break;
          case 25: 
            { String url = yytext();
         out.write("<a href=\"");
         out.write(url);out.write("\">");
         out.write(url);out.write("</a>");
            }
          case 50: break;
          default:
            zzScanError(ZZ_NO_MATCH);
        }
      }
    }
  }


}
