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
 * Copyright (c) 2007, 2015, Oracle and/or its affiliates. All rights reserved.
 */
package com.grantingersoll.opengrok.index;

import java.io.File;

/**
 * This class maintains a list of file names (like "cscope.out"), SRC_ROOT
 * relative file paths (like "usr/src/uts" or "usr/src/Makefile"), and glob
 * patterns (like .make.*) which opengrok should ignore.
 *
 * @author Chandan
 */
public final class IgnoredFiles extends Filter {
    private static final String[] defaultPatternsFiles = {
        "cscope.in.out",
        "cscope.out.po",
        "cscope.out.in",
        "cscope.po.out",
        "cscope.po.in",
        "cscope.files",
        "cscope.out",
        // CVS
        ".cvsignore",
        ".repo",
        ".hgtags",
        ".p4config",
        "*~",
        ".make.*",
        ".del-*",
        "_MTN",
        // File Extensions for Visual Studio and Mono Projects
        ".vspscc",
        ".suo",
        ".vssscc",
        ".user",
        ".ncb",
        ".gpState",
        ".snc",
        ".sln",
        ".vsmdi",
        ".dll",
        ".opengrok_skip_history",
    };

    public IgnoredFiles() {
        super();
        addDefaultPatterns();
    }

    /**
     * Should the file be ignored or not?
     * @param file the file to check
     * @return true if this file should be ignored, false otherwise
     */
    public boolean ignore(File file) {
        return match(file) && file.isFile();
    }

    /**
     * Should the file be ignored or not?
     * @param name the name of the file to check
     * @return true if this pathname should be ignored, false otherwise
     */
    public boolean ignore(String name) {
        return match(name);
    }

    private void addDefaultPatterns() {
        for (String s : defaultPatternsFiles) {
            add(s);
        }
    }
}
