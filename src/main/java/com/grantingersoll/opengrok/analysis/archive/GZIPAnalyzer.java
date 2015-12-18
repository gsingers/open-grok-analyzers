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
package com.grantingersoll.opengrok.analysis.archive;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.GZIPInputStream;

import com.grantingersoll.opengrok.analysis.StreamSource;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import com.grantingersoll.opengrok.analysis.AnalyzerGuru;
import com.grantingersoll.opengrok.analysis.FileAnalyzer;
import com.grantingersoll.opengrok.analysis.FileAnalyzer.Genre;
import com.grantingersoll.opengrok.analysis.FileAnalyzerFactory;
import com.grantingersoll.opengrok.analysis.StreamSource;
import com.grantingersoll.opengrok.logger.LoggerFactory;

/**
 * Analyzes GZip files Created on September 22, 2005
 *
 * @author Chandan
 */
public class GZIPAnalyzer extends FileAnalyzer {

    private static final Logger LOGGER = LoggerFactory.getLogger(GZIPAnalyzer.class);

    private Genre g;

    @Override
    public Genre getGenre() {
        if (g != null) {
            return g;
        }
        return super.getGenre();
    }

    protected GZIPAnalyzer(FileAnalyzerFactory factory) {
        super(factory);
    }
    private FileAnalyzer fa;

    @Override
    public void analyze(Document doc, StreamSource src, Writer xrefOut) throws IOException {
        StreamSource gzSrc = wrap(src);
        String path = doc.get("path");
        if (path != null
                && (path.endsWith(".gz") || path.endsWith(".GZ") || path.endsWith(".Gz"))) {
            String newname = path.substring(0, path.length() - 3);
            //System.err.println("GZIPPED OF = " + newname);
            try (InputStream gzis = gzSrc.getStream()) {
                fa = AnalyzerGuru.getAnalyzer(gzis, newname);
            }
            if (fa == null) {
                this.g = Genre.DATA;
                LOGGER.log(Level.WARNING, "Did not analyze {0}, detected as data.", newname);
                //TODO we could probably wrap tar analyzer here, need to do research on reader coming from gzis ...
            } else { // cant recurse!
                //simple file gziped case captured here
                if (fa.getGenre() == Genre.PLAIN || fa.getGenre() == Genre.XREFABLE) {
                    this.g = Genre.XREFABLE;
                } else {
                    this.g = Genre.DATA;
                }
                fa.analyze(doc, gzSrc, xrefOut);
                if (doc.get("t") != null) {
                    doc.removeField("t");
                    if (g == Genre.XREFABLE) {
                        doc.add(new Field("t", g.typeName(), AnalyzerGuru.string_ft_stored_nanalyzed_norms));
                    }
                }

            }
        }
    }

    /**
     * Wrap the raw stream source in one that returns the uncompressed stream.
     */
    private static StreamSource wrap(final StreamSource src) {
        return new StreamSource() {
            @Override
            public InputStream getStream() throws IOException {
                return new BufferedInputStream(
                        new GZIPInputStream(src.getStream()));
            }
        };
    }

    @Override
    public TokenStreamComponents createComponents(String fieldName) {
        if (fa != null) {
            return fa.createComponents(fieldName);
        }
        return super.createComponents(fieldName);
    }
}
