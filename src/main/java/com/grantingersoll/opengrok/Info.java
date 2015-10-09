package com.grantingersoll.opengrok;


import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * Utility class to get information of the OpenGrok version.
 *
 * @author Trond Norbye
 */
@SuppressWarnings("PMD.AvoidThrowingRawExceptionTypes")
public final class Info {
    private static final Properties properties = new Properties();

    private static final String VERSION;
    private static final String REVISION;

    static {
        try (InputStream in = Info.class.getResourceAsStream("info.properties")) {
            if (in != null) {
                properties.load(in);
            }
            VERSION = properties.getProperty("version", "unknown");
            REVISION = properties.getProperty("changeset", "unknown");
        } catch (IOException ioe) {
            throw new RuntimeException(ioe);
        }
    }

    /**
     * get major version
     * @return major version
     */
    public static String getVersion() {
        return VERSION;
    }

    /**
     * get full version (product vMajor revMinor)
     * @return full version
     */
    public static String getFullVersion() {
        return "OpenGrok v" + VERSION + " rev " + REVISION;
    }

    /**
     * get minor version
     * @return minor version
     */
    public static String getRevision() {
        return REVISION;
    }

    private Info() {
    }
}
