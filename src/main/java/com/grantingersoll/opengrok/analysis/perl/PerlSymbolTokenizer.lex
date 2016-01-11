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
 * Copyright (c) 2010, Oracle and/or its affiliates. All rights reserved.
 */

/*
 * Gets Perl symbols - ignores comments, strings, keywords
 */

package com.grantingersoll.opengrok.analysis.perl;
import java.io.IOException;
import java.io.Reader;
import com.grantingersoll.opengrok.analysis.SymbolTokenizer;
import org.apache.lucene.util.AttributeFactory;

%%
%public
%class PerlSymbolTokenizer
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
    public PerlSymbolTokenizer(AttributeFactory factory) {
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

%state STRING SCOMMENT QSTRING

%%

// TODO: recognize POD format as comments
// TODO: handle full numeric syntax, to prohibit false Identifier recognition (underscores, exponents)
// TODO: handle *all* quoting constructs: tr///, q//, qq//, s///, heredocs
// TODO: recognize symbols in interpolated strings (including heredocs)

<YYINITIAL> {
"0" [xX][0-9a-fA-F]+ {} // Ignore hex literals, to block recognition of "x..." (after "0") as an Identifier
{Identifier} {String id = yytext();
                if(!Consts.kwd.contains(id)){
                        setAttribs(id, yychar, yychar + yylength());
                        return true; }
              }
 \"     { yybegin(STRING); }
 \'     { yybegin(QSTRING); }
 "#"   { yybegin(SCOMMENT); }
 }

<STRING> {
 \"     { yybegin(YYINITIAL); }
 \\\"    {}
 \n     { yybegin(YYINITIAL); }
}

<QSTRING> {
 \'     { yybegin(YYINITIAL); }
 \\\'   {}
 \n     { yybegin(YYINITIAL); }
}

<SCOMMENT> {
 \n    { yybegin(YYINITIAL);}
}

<YYINITIAL, STRING, SCOMMENT, QSTRING> {
<<EOF>>   { return false;}
[^]    {}
}
