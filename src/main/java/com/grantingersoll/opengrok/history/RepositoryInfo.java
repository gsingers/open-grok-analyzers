package com.grantingersoll.opengrok.history;


/**
 *
 *
 **/
import java.io.Serializable;

/**
 * Class to contain the common info for a repository. This object
 * will live on the server and the client side, so don't add logic
 * that will only work on one side in this object.
 *
 * @author Trond Norbye
 */
public class RepositoryInfo implements Serializable {
    private static final long serialVersionUID = 2L;

    protected String directoryName; // absolute path
    protected Boolean working;
    protected String type;
    protected boolean remote;
    protected String datePattern;
    protected String parent;
    protected String branch;

    /**
     * Empty constructor to support serialization.
     */
    public RepositoryInfo() {
        super();
    }

    public RepositoryInfo(RepositoryInfo orig) {
        this.directoryName = orig.directoryName;
        this.type = orig.type;
        this.working = Boolean.valueOf(orig.isWorking());
        this.remote = orig.isRemote();
        this.datePattern = orig.datePattern;
        this.parent = orig.parent;
        this.branch = orig.branch;
    }

    /**
     * Get the name of the root directory for this repository.
     * @return the name of the root directory
     */
    public String getDirectoryName() {
        return directoryName;
    }

    /**
     * Specify the name of the root directory for this repository.
     * @param directoryName the new name of the root directory
     */
    public void setDirectoryName(String directoryName) {
        this.directoryName = directoryName;
    }

    /**
     * Returns true if this repository is usable in this context (for SCM
     * systems that use external binaries, the binary must be availabe etc)
     *
     * @return true if the HistoryGuru may use the repository
     */
    public boolean isWorking() {
        return working != null && working.booleanValue();
    }

    /**
     * Set the property working
     *
     * @param working
     */
    public void setWorking(Boolean working) {
        this.working = working;
    }

    /**
     * Is the history and version information for this repository stored on
     * a remote server?
     *
     * @return true if the history is stored on a remote server.
     */
    public boolean isRemote() {
        return remote;
    }

    /**
     * Set the property remote
     * @param remote
     */
    public void setRemote(boolean remote) {
        this.remote = remote;
    }

    /**
     * get property type
     * @return type
     */
    public String getType() {
        return type;
    }

    /**
     * Set property type
     * @param type
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * get property type
     * @return parent
     */
    public String getParent() {
        return parent;
    }

    /**
     * Set property parent
     * @param parent
     */
    public void setParent(String parent) {
        this.parent = parent;
    }

    public void setDatePattern(String datePattern) {
        this.datePattern = datePattern;
    }

    public String getDatePattern() {
        return datePattern;
    }

    public String getBranch() {
        return branch;
    }

    public void setBranch(String branch) {
        this.branch = branch;
    }
}

