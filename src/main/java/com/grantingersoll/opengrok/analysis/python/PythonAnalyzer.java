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
 * Copyright (c) 2010, 2013, Oracle and/or its affiliates. All rights reserved.
 */
package com.grantingersoll.opengrok.analysis.python;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import com.grantingersoll.opengrok.analysis.Definitions;
import com.grantingersoll.opengrok.analysis.JFlexTokenizer;
import com.grantingersoll.opengrok.analysis.JFlexXref;
import com.grantingersoll.opengrok.analysis.plain.AbstractSourceCodeAnalyzer;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.configuration.Project;
import com.grantingersoll.opengrok.history.Annotation;

/**
 *
 * @author Lubos Kosco
 */
public class PythonAnalyzer extends AbstractSourceCodeAnalyzer {

    /**
     * Creates a new instance of PythonAnalyzer
     */
    protected PythonAnalyzer(FileAnalyzerFactory factory) {
        super(factory);
    }

    @Override
    protected JFlexTokenizer newSymbolTokenizer(Reader reader) {
        return new PythonSymbolTokenizer(reader);
    }

    @Override
    protected JFlexXref newXref(Reader reader) {
        return new PythonXref(reader);
    }

    static void writeXref(Reader in, Writer out, Definitions defs, Annotation annotation, Project project) throws IOException {
        PythonXref xref = new PythonXref(in);
        AbstractSourceCodeAnalyzer.writeXref(xref, in, out, defs, annotation, project);
    }
}
