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
 * Get Haskell symbols
 */

package com.grantingersoll.opengrok.analysis.haskell;
import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import org.apache.lucene.util.AttributeFactory;

/**
 * @author Harry Pan
 */

%%
%public
%class HaskellSymbolTokenizer
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
    public HaskellSymbolTokenizer(AttributeFactory factory) {
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

Identifier = [a-zA-Z_] [a-zA-Z0-9_']*

%state STRING CHAR COMMENT BCOMMENT

%%

<YYINITIAL> {
    "0" [oO][0-9a-fA-F]+ {} // Ignore octal literals, to block recognition of "o..." (after "0") as an Identifier
    "0" [xX][0-9a-fA-F]+ {} // Ignore hex literals, to block recognition of "x..." (after "0") as an Identifier
    {Identifier} {
        String id = yytext();
        if (!Consts.kwd.contains(id)) {
            setAttribs(id, yychar, yychar + yylength());
            return true;
        }
    }
    \"   { yybegin(STRING);   }
    \'   { yybegin(CHAR);     }
    "--" { yybegin(COMMENT);  }
    "{-" { yybegin(BCOMMENT); }
}

<STRING> {
    \"   { yybegin(YYINITIAL); }
    \\\" {}     // escaped double quote - don't do anything
}

<CHAR> {    // we don't need to consider the case where prime is part of an identifier since it is handled above
    \'   { yybegin(YYINITIAL); }
    \\\' {}     // escaped single quote - don't do anything
}

<COMMENT> {
    \n { yybegin(YYINITIAL); }
}

<BCOMMENT> {
    "-}" { yybegin(YYINITIAL); }
}

// fallback
[^] {}
