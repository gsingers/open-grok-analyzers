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
 * Copyright (c) 2012, 2013, Oracle and/or its affiliates. All rights reserved.
 */
package com.grantingersoll.opengrok.analysis.sql;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import com.grantingersoll.opengrok.analysis.Definitions;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.analysis.JFlexXref;
import com.grantingersoll.opengrok.analysis.plain.PlainAnalyzer;
import com.grantingersoll.opengrok.configuration.Project;
import com.grantingersoll.opengrok.history.Annotation;

public class PLSQLAnalyzer extends PlainAnalyzer {

    public PLSQLAnalyzer(FileAnalyzerFactory factory) {
        super(factory);
    }

    @Override
    protected JFlexXref newXref(Reader reader) {
        return new PLSQLXref(reader);
    }

    /**
     * Write a cross referenced HTML file. Reads the source from an input
     * stream.
     *
     * @param in input source
     * @param out output xref writer
     * @param defs definitions for the file (could be null)
     * @param annotation annotation for the file (could be null)
     */
    static void writeXref(Reader in, Writer out, Definitions defs, Annotation annotation, Project project) throws IOException {
        PLSQLXref xref = new PLSQLXref(in);
        xref.annotation = annotation;
        xref.project = project;
        xref.setDefs(defs);
        xref.write(out);
    }
}