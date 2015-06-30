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
 * Copyright (c) 2005, 2015, Oracle and/or its affiliates. All rights reserved.
 */
package com.grantingersoll.opengrok.analysis.sh;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import com.grantingersoll.opengrok.analysis.Definitions;
import com.grantingersoll.opengrok.analysis.FileAnalyzer;
import com.grantingersoll.opengrok.analysis.FileAnalyzer.Genre;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.configuration.Project;
import com.grantingersoll.opengrok.history.Annotation;

public class ShAnalyzerFactory extends FileAnalyzerFactory {
    
    private static final String name = "Shell script";
    
    private static final String[] NAMES = {
        "GNUMAKEFILE", "MAKEFILE"
    };

    private static final String[] PREFIXES = {
        "MAKEFILE"
    };

    private static final String[] SUFFIXES = {
        "SH",
        "KSH",
        "KSHLIB", // RFE #17849
        "CSH",
        "BASH",
        "RUBY",
        "RB",
        "P5",
        "AWK",
        "GMK",
        "CONF",
        "COM",
        "SPEC",
        "FLG",
        "XCL", // message
    };

    private static final String[] MAGICS = {
        "#!",
    };

    public ShAnalyzerFactory() {
        super(NAMES, PREFIXES, SUFFIXES, MAGICS, null, "text/plain", Genre.PLAIN, name);
    }

    @Override
    protected FileAnalyzer newAnalyzer() {
        return new ShAnalyzer(this);
    }

    @Override
    public void writeXref(Reader in, Writer out, Definitions defs,
        Annotation annotation, Project project)
        throws IOException {
        ShAnalyzer.writeXref(in, out, defs, annotation, project);
    }
}
