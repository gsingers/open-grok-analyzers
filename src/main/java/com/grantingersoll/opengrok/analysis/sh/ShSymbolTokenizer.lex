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
 * Copyright (c) 2005, 2010, Oracle and/or its affiliates. All rights reserved.
 */

package com.grantingersoll.opengrok.analysis.sh;
import java.io.IOException;
import java.io.Reader;
import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import org.apache.lucene.util.AttributeFactory;

%%
%public
%class ShSymbolTokenizer
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
    public ShSymbolTokenizer(AttributeFactory factory) {
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

%state STRING COMMENT SCOMMENT QSTRING

%%

<YYINITIAL> {
"0" [xX][0-9a-fA-F]+ {} // Ignore hex literals, to block recognition of "x..." (after "0") as an Identifier
{Identifier} {String id = yytext();
                if(!Consts.shkwd.contains(id)){
                        setAttribs(id, yychar, yychar + yylength());
                        return true; }
              }
 \"     { yybegin(STRING); }
 \'     { yybegin(QSTRING); }
 "#"    { yybegin(SCOMMENT); }
}

<STRING> {
"$" {Identifier} {
    setAttribs(yytext().substring(1), yychar + 1, yychar + yylength());
    return true;
}

"${" {Identifier} "}" {
    int startOffset = 2;            // trim away the "${" prefix
    int endOffset = yylength() - 1; // trim away the "}" suffix
    setAttribs(yytext().substring(startOffset, endOffset),
               yychar + startOffset,
               yychar + endOffset);
    return true;
}

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
