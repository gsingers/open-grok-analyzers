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
 * Copyright (c) 2005, 2013, Oracle and/or its affiliates. All rights reserved.
 */
package com.grantingersoll.opengrok.analysis.archive;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import com.grantingersoll.opengrok.analysis.FileAnalyzer;
import com.grantingersoll.opengrok.analysis.IteratorReader;
import com.grantingersoll.opengrok.analysis.StreamSource;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.TextField;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.web.Util;

/**
 * Analyzes Zip files Created on September 22, 2005
 *
 * @author Chandan
 */
public class ZipAnalyzer extends FileAnalyzer {

    protected ZipAnalyzer(FileAnalyzerFactory factory) {
        super(factory);
    }

    @Override
    public void analyze(Document doc, StreamSource src, Writer xrefOut) throws IOException {
        ArrayList<String> names = new ArrayList<>();

        try (ZipInputStream zis = new ZipInputStream(src.getStream())) {
            ZipEntry entry;
            while ((entry = zis.getNextEntry()) != null) {
                String name = entry.getName();
                names.add(name);
                if (xrefOut != null) {
                    Util.htmlize(name, xrefOut);
                    xrefOut.append("<br/>");
                }
            }
        }

        doc.add(new TextField("full", new IteratorReader(names)));
    }
}
