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
 * Copyright (c) 2008, 2010, Oracle and/or its affiliates. All rights reserved.
 */

/*
 * Gets Tcl symbols - ignores comments, strings, keywords
 */

package com.grantingersoll.opengrok.analysis.tcl;
import java.io.IOException;
import java.io.Reader;
import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import org.apache.lucene.util.AttributeFactory;

%%
%public
%class TclSymbolTokenizer
%extends JFlexTokenizer
%unicode
%init{
super(in);
%init}
%type boolean
%eofval{
return false;
%eofval}
%char


%{
    public TclSymbolTokenizer(AttributeFactory factory) {
        super(factory);
    }

    @Override
    protected void yysetreader(java.io.Reader in) {
        zzReader = in;
    }

    @Override
    public int yychar() {
        return yychar;
    }
%}

// From http://tmml.sourceforge.net/doc/tcl/Tcl.html:
//
// $name
//
// Name is the name of a scalar variable; the name is a sequence of one or more
// characters that are a letter, digit, underscore, or namespace separators
// (two or more colons). Letters and digits are only the standard ASCII ones
// (0-9, A-Z and a-z).
//
// $name(index)
//
// Name gives the name of an array variable and index gives the name of an
// element within that array. Name must contain only letters, digits,
// underscores, and namespace separators, and may be an empty string.
// Letters and digits are only the standard ASCII ones (0-9, A-Z and a-z).
// Command substitutions, variable substitutions, and backslash substitutions
// are performed on the characters of index.
//
// ${name}
//
// Name is the name of a scalar variable or array element. It may contain any
// characters whatsoever except for close braces. It indicates an array element
// if name is in the form "arrayName(index)" where arrayName does not contain
// any open parenthesis characters, "(", or close brace characters, "}", and
// index can be any sequence of characters except for close brace characters.
// No further substitutions are performed during the parsing of name.

BareIdentifier = (":" ":"+)? [a-zA-Z_] [a-zA-Z0-9_]* (":" ":"+ [a-zA-Z_] [a-zA-Z0-9_]* )*
DereferencedIdentifier = "$" {BareIdentifier}
DereferencedBracketedArrayElement = "${" [^(]* "(" [^\)\}]* ")}"
DereferencedBracketedIdentifier = "${" [^\}]* "}"

%state STRING COMMENT SCOMMENT

%%

// TODO: recognize identifiers in interpolated strings (double quotes)

<YYINITIAL> {
"0" [xX][0-9a-fA-F]+ {} // Ignore hex literals, to block recognition of "x..." (after "0") as an Identifier
{BareIdentifier} { String id = yytext();
                   if (!Consts.kwd.contains(id)) {
                        setAttribs(id, yychar, yychar + yylength());
                        return true;
                   }
                 }
{DereferencedIdentifier} { String id = yytext().substring(1);
                           setAttribs(id, yychar + 1, yychar + yylength());
                           return true;
                         }
{DereferencedBracketedArrayElement} { String id = yytext().substring(2);
                                      int elemStartPos = id.indexOf('(');
                                      id = id.substring(0, elemStartPos);
                                      setAttribs(id, yychar + 2, yychar + 2 + elemStartPos);
                                      // Ignore the (array-element) part: it's an uninterpolated associative array index
                                      return true;
                                    }
{DereferencedBracketedIdentifier} { if (yylength() > 3) { // Ignore the zero-length identifier
                                      String id = yytext().substring(2, yytext().length() - 1);
                                      setAttribs(id, yychar + 2, yychar + yylength() - 1);
                                      return true;
                                    }
                                  }

 \"     { yybegin(STRING); }
"#"     { yybegin(SCOMMENT); }
}

<STRING> {
 \"     { yybegin(YYINITIAL); }
\\\\ | \\\"     {}
}

<SCOMMENT> {
\n      { yybegin(YYINITIAL);}
}

<YYINITIAL, STRING, COMMENT, SCOMMENT> {
<<EOF>>   { return false;}
[^]    {}
}
