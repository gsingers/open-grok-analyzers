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
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved.
 */

/*
 * Gets Erlang symbols - ignores comments, strings, keywords
 */

package com.grantingersoll.opengrok.analysis.erlang;
import java.io.IOException;
import java.io.Reader;
import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import org.apache.lucene.util.AttributeFactory;

%%
%public
%class ErlangSymbolTokenizer
%extends JFlexTokenizer
%unicode
%type boolean
%eofval{
return false;
%eofval}
%char

%{
    public ErlangSymbolTokenizer(AttributeFactory factory) {
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

Identifier = [a-zA-Z_] [a-zA-Z0-9_@]*

%state STRING COMMENT QATOM

%%

<YYINITIAL> {
{Identifier} {String id = yytext();
                if(!Consts.kwd.contains(id)){
                        setAttribs(id, yychar, yychar + yylength());
                        return true; }
              }
 \"     { yybegin(STRING); }
 \'     { yybegin(QATOM); }
 "%"   { yybegin(COMMENT); }
 }

<STRING> {
 \"     { yybegin(YYINITIAL); }
 \\\\ | \\\"   {}
}

<QATOM> {
 \'     { yybegin(YYINITIAL); }
 \\\\ | \\\'   {}
}

<COMMENT> {
 \n    { yybegin(YYINITIAL);}
}

<YYINITIAL, STRING, QATOM, COMMENT> {
<<EOF>>   { return false;}
[^]    {}
}
