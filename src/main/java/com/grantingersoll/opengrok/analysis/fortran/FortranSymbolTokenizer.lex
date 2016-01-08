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
 * Copyright (c) 2009, 2010, Oracle and/or its affiliates. All rights reserved.
 */

package com.grantingersoll.opengrok.analysis.fortran;
import java.io.IOException;
import java.io.Reader;
import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import org.apache.lucene.util.AttributeFactory;

%%
%public
%class FortranSymbolTokenizer
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
    public FortranSymbolTokenizer(AttributeFactory factory) {
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

Identifier = [a-zA-Z_] [a-zA-Z0-9_]*
Label = [0-9]+
BinaryLiteral = ([bB] ("'" [01]+ "'"|"\"" [01]+ "\"") | ("'" [01]+ "'"|"\"" [01]+ "\"") [bB])
OctalLiteral = ([oO] ("'" [0-7]+ "'"|"\"" [0-7]+ "\"") | ("'" [0-7]+ "'"|"\"" [0-7]+ "\"") [oO])
HexLiteral = ([zZxX] ("'" [0-9a-fA-F]+ "'"|"\"" [0-9a-fA-F]+ "\"") | ("'" [0-9a-fA-F]+ "'"|"\"" [0-9a-fA-F]+ "\"") [zZxX])
BOZliteral = {BinaryLiteral} | {OctalLiteral} | {HexLiteral}

%state STRING SCOMMENT QSTRING

%%

<YYINITIAL> {
 ^{Label} { }
 ^[*cC!] { yybegin(SCOMMENT); }
{BOZliteral} {} // Ignore numeric literals, to block (partial) recognition as Identifiers
{Identifier} {String id = yytext();
                if(!Consts.kwd.contains(id.toLowerCase())) {
                        setAttribs(id, yychar, yychar + yylength());
                        return true; }
              }
 \"     { yybegin(STRING); }
 \'     { yybegin(QSTRING); }
 \!     { yybegin(SCOMMENT); }
}

<STRING> {
 \"     { yybegin(YYINITIAL); }
\\\\ | \\\"     {}
}

<QSTRING> {
 \'     { yybegin(YYINITIAL); }
}

<SCOMMENT> {
\n      { yybegin(YYINITIAL);}
}

<YYINITIAL, STRING, SCOMMENT, QSTRING> {
<<EOF>>   { return false;}
[^]    {}
}
