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
 * Copyright (c) 2006, 2010, Oracle and/or its affiliates. All rights reserved.
 */

/*
 * Gets VB symbols - ignores comments, strings, keywords
 */

package com.grantingersoll.opengrok.analysis.vb;
import java.io.IOException;
import java.io.Reader;
import com.grantingersoll.opengrok.analysis.SymbolTokenizer;
import org.apache.lucene.util.AttributeFactory;

%%
%public
%class VBSymbolTokenizer
%extends SymbolTokenizer
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
    public VBSymbolTokenizer(AttributeFactory factory) {
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

    @Override
    public String getSourceCodeLanguage() {
        return Consts.SOURCE_CODE_LANGUAGE;
    }

    @Override
    public String getMimeType() {
        return Consts.MIME_TYPE;
    }
%}

Identifier = [a-zA-Z_] [a-zA-Z0-9_]*

%state STRING COMMENT

%%

<YYINITIAL> {
"&" [hH][0-9a-fA-F]+ {} // Ignore hex literals, to block recognition of "h..." (after "&") as an Identifier
"&" [oO][0-7]+ {} // Ignore octal literals, to block recognition of "o..." (after "&") as an Identifier
{Identifier} {String id = yytext();
                if(!Consts.getReservedKeywords().contains(id)){
                        setAttribs(id, yychar, yychar + yylength());
                        return true; }
              }
 \"     { yybegin(STRING); }
 \'     { yybegin(COMMENT); }
}

<STRING> {
   \"     { yybegin(YYINITIAL); }
\\\\ | \\\"     {}
}

<COMMENT> {
\n      { yybegin(YYINITIAL);}
}

<YYINITIAL, STRING, COMMENT> {
<<EOF>>   { return false;}
[^]    {}
}
