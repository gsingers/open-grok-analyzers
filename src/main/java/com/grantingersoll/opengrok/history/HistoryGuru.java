package com.grantingersoll.opengrok.history;


/**
 *
 *
 **/
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;



import com.grantingersoll.opengrok.configuration.Configuration.RemoteSCM;
import com.grantingersoll.opengrok.configuration.RuntimeEnvironment;
import com.grantingersoll.opengrok.index.IgnoredNames;
import com.grantingersoll.opengrok.util.Statistics;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The HistoryGuru is used to implement an transparent layer to the various
 * source control systems.
 *
 * @author Chandan
 */
public final class HistoryGuru {
  private transient static Logger log = LoggerFactory.getLogger(HistoryGuru.class);

    /** The one and only instance of the HistoryGuru */
    private static HistoryGuru instance = new HistoryGuru();

    /** The history cache to use */
    private final HistoryCache historyCache;

    private Map<String, Repository> repositories =
        new HashMap<String, Repository>();
    private final int scanningDepth;

    /**
     * Creates a new instance of HistoryGuru, and try to set the default
     * source control system.
     */
    private HistoryGuru() {
        HistoryCache cache = null;
        RuntimeEnvironment env = RuntimeEnvironment.getInstance();
        scanningDepth=env.getScanningDepth();
        if (env.useHistoryCache()) {
            cache = new FileHistoryCache();
            try {
                cache.initialize();
            } catch (HistoryException he) {
                log.warn(
                        "Failed to initialize the history cache", he);
                // Failed to initialize, run without a history cache
                cache = null;
            }
        }
        historyCache = cache;
    }

    /**
     * Get the one and only instance of the HistoryGuru
     * @return the one and only HistoryGuru instance
     */
    public static HistoryGuru getInstance()  {
        return instance;
    }

    /**
     * Return whether or not a cache should be used for the history log.
     * @return {@code true} if the history cache has been enabled and
     * initialized, {@code false} otherwise
     */
    private boolean useCache() {
        return historyCache != null;
    }

    /**
     * Get a string with information about the history cache.
     *
     * @return a free form text string describing the history cache instance
     * @throws HistoryException if an error occurred while getting the info
     */
    public String getCacheInfo() throws HistoryException {
        return historyCache == null ? "No cache" : historyCache.getInfo();
    }

    /**
     * Annotate the specified revision of a file.
     *
     * @param file the file to annotate
     * @param rev the revision to annotate (<code>null</code> means BASE)
     * @return file annotation, or <code>null</code> if the
     * <code>HistoryParser</code> does not support annotation
     * @throws IOException
     */
    public Annotation annotate(File file, String rev) throws IOException {
        Annotation ret = null;

        Repository repo = getRepository(file);
        if (repo != null) {
            ret = repo.annotate(file, rev);
            History hist = null;
            try {
                hist = repo.getHistory(file);
            } catch (HistoryException ex) {
                log.debug("Cannot get messages for tooltip: ", ex);
            }
            if (hist != null && ret != null) {
             Set<String> revs = ret.getRevisions();
             // !!! cannot do this because of not matching rev ids (keys)
             // first is the most recent one, so we need the position of "rev"
             // until the end of the list
             //if (hent.indexOf(rev)>0) {
             //     hent = hent.subList(hent.indexOf(rev), hent.size());
             //}
             for (HistoryEntry he : hist.getHistoryEntries()) {
                String hist_rev = he.getRevision();
                String short_rev = repo.getRevisionForAnnotate(hist_rev);
                if (revs.contains(short_rev)) {
                    ret.addDesc(short_rev, "changeset: " + he.getRevision() +
                        "\nsummary: " + he.getMessage() + "\nuser: " +
                        he.getAuthor() + "\ndate: " + he.getDate());
                }
             }
            }
        }

        return ret;
    }

    /**
     * Get the appropriate history reader for given file.
     *
     * @param file The file to get the history reader for
     * @throws HistoryException If an error occurs while getting the history
     * @return A HistorReader that may be used to read out history data for a
     *  named file
     */
    public HistoryReader getHistoryReader(File file) throws HistoryException {
        History history = getHistory(file, false);
        return history == null ? null : new HistoryReader(history);
    }

    /**
     * Get the history for the specified file.
     *
     * @param file the file to get the history for
     * @return history for the file
     * @throws HistoryException on error when accessing the history
     */
    public History getHistory(File file) throws HistoryException {
        return getHistory(file, true, false);
    }

    public History getHistory(File file, boolean withFiles) throws HistoryException {
        return getHistory(file, true, false);
    }

    /**
     * Get history for the specified file (called from the web app).
     *
     * @param file the file to get the history for
     * @return history for the file
     * @throws HistoryException on error when accessing the history
     */
    public History getHistoryUI(File file) throws HistoryException {
        return getHistory(file, true, true);
    }

    /**
     * Get the history for the specified file.
     *
     * @param file the file to get the history for
     * @param withFiles whether or not the returned history should contain
     * a list of files touched by each changeset (the file list may be skipped
     * if false, but it doesn't have to)
     * @param ui called from the webapp
     * @return history for the file
     * @throws HistoryException on error when accessing the history
     */
    public History getHistory(File file, boolean withFiles, boolean ui)
            throws HistoryException {
        final File dir = file.isDirectory() ? file : file.getParentFile();
        final Repository repo = getRepository(dir);

        History history = null;
        RemoteSCM rscm = RuntimeEnvironment.getInstance().getRemoteScmSupported();
        boolean doRemote = (ui && (rscm == RemoteSCM.UIONLY))
            || (rscm == RemoteSCM.ON)
            || (ui || ((rscm == RemoteSCM.DIRBASED) && (repo != null) && repo.hasHistoryForDirectories()));

        if (repo != null && repo.isWorking() && repo.fileHasHistory(file)
            && (!repo.isRemote() || doRemote)) {

            if (useCache() && historyCache.supportsRepository(repo)) {
                history = historyCache.get(file, repo, withFiles);
            } else {
                history = repo.getHistory(file);
            }
        }

        return history;
    }

    /**
     * Get a named revision of the specified file.
     * @param parent The directory containing the file
     * @param basename The name of the file
     * @param rev The revision to get
     * @return An InputStream containing the named revision of the file.
     */
    public InputStream getRevision(String parent, String basename, String rev) {
        InputStream ret = null;

        Repository rep = getRepository(new File(parent));
        if (rep != null) {
            ret = rep.getHistoryGet(parent, basename, rev);
        }
        return ret;
    }

    /**
     * Does this directory contain files with source control information?
     * @param file The name of the directory
     * @return true if the files in this directory have associated revision
     * history
     */
    public boolean hasHistory(File file) {
        Repository repo = getRepository(file);

        if (repo == null) {
            return false;
        }

        // This should return true for Annotate view.
        return repo.isWorking() && repo.fileHasHistory(file)
                && ((RuntimeEnvironment.getInstance().getRemoteScmSupported() == RemoteSCM.ON)
                    || (RuntimeEnvironment.getInstance().getRemoteScmSupported() == RemoteSCM.UIONLY)
                    || (RuntimeEnvironment.getInstance().getRemoteScmSupported() == RemoteSCM.DIRBASED)
                    || !repo.isRemote());
    }

    /**
     * Check if we can annotate the specified file.
     *
     * @param file the file to check
     * @return <code>true</code> if the file is under version control and the
     * version control system supports annotation
     */
    public boolean hasAnnotation(File file) {
        if (!file.isDirectory()) {
            Repository repos = getRepository(file);
            if (repos != null && repos.isWorking()) {
                return repos.fileHasAnnotation(file);
            }
        }

        return false;
    }

    /**
     * Get the last modified times for all files and subdirectories in the
     * specified directory.
     *
     * @param directory the directory whose files to check
     * @return a map from file names to modification times for the files that
     *   the history cache has information about
     */
    public Map<String, Date> getLastModifiedTimes(File directory)
            throws HistoryException {
        Repository repository = getRepository(directory);
        if (repository != null && useCache()) {
            return historyCache.getLastModifiedTimes(directory, repository);
        }
        return Collections.emptyMap();
    }

    private void addRepositories(File[] files, Collection<RepositoryInfo> repos,
            IgnoredNames ignoredNames, int depth)
    {
        addRepositories(files, repos, ignoredNames, true, depth);
    }

    /**
     * recursively search for repositories with a depth limit
     * @param files list of files to check if they contain a repo
     * @param repos list of found repos
     * @param ignoredNames what files to ignore
     * @param recursiveSearch whether to use recursive search
     * @param depth current depth - using global scanningDepth - one can limit
     *  this to improve scanning performance
     */
    private void addRepositories(File[] files, Collection<RepositoryInfo> repos,
            IgnoredNames ignoredNames, boolean recursiveSearch, int depth) {
        for (File file : files) {
            Repository repository = null;
            if (file.getName().equals(".opengrok_skip_history")) {
                log.info("Skipping history cache creation for "
                        + file.getParentFile().getAbsolutePath()
                        + " and its subdirectories");
                return;
            }
        }
        for (File file : files) {
            Repository repository = null;
            try {
                repository = RepositoryFactory.getRepository(file);
            } catch (InstantiationException ie) {
                log.warn( "Could not create repoitory for '"
                    + file + "', could not instantiate the repository.", ie);
            } catch (IllegalAccessException iae) {
                log.warn( "Could not create repoitory for '"
                    + file + "', missing access rights.", iae);
            }
            if (repository == null) {
                // Not a repository, search its sub-dirs
                if (file.isDirectory() && !ignoredNames.ignore(file)) {
                    File subFiles[] = file.listFiles();
                    if (subFiles == null) {
                        log.warn(
                            "Failed to get sub directories for '"
                            + file.getAbsolutePath()
                            + "', check access permissions.");
                    } else if (depth<=scanningDepth) {
                        addRepositories(subFiles, repos, ignoredNames, depth+1);
                    }
                }
            } else {
                try {
                    String path = file.getCanonicalPath();
                    repository.setDirectoryName(path);
                    if (RuntimeEnvironment.getInstance().isVerbose()) {
                        log.debug("Adding <{0}> repository: <{1}>",
                            new Object[]{repository.getClass().getName(), path});
                    }

                    repos.add(new RepositoryInfo(repository));

                    // @TODO: Search only for one type of repository - the one found here
                    if (recursiveSearch && repository.supportsSubRepositories()) {
                        File subFiles[] = file.listFiles();
                        if (subFiles == null) {
                            log.warn(
                                "Failed to get sub directories for '"
                                + file.getAbsolutePath()
                                + "', check access permissions.");
                        } else if (depth<=scanningDepth) {
                            // Search only one level down - if not: too much
                            // stat'ing for huge Mercurial repositories
                            addRepositories(subFiles, repos, ignoredNames,
                                false, depth+1);
                        }
                    }

                } catch (IOException exp) {
                    log.warn( "Failed to get canonical path for "
                        + file.getAbsolutePath() + ": " + exp.getMessage());
                    log.warn( "Repository will be ignored...", exp);
                }
            }
        }
    }

    /**
     * Search through the all of the directories and add all of the source
     * repositories found.
     *
     * @param dir the root directory to start the search in.
     */
    public void addRepositories(String dir) {
        List<RepositoryInfo> repos = new ArrayList<RepositoryInfo>();
        addRepositories(new File[] {new File(dir)}, repos,
                RuntimeEnvironment.getInstance().getIgnoredNames(),0);
        RuntimeEnvironment.getInstance().setRepositories(repos);
        invalidateRepositories(repos);
    }

    /**
     * Update the source the contents in the source repositories.
     */
    public void updateRepositories() {
        boolean verbose = RuntimeEnvironment.getInstance().isVerbose();

        for (Map.Entry<String, Repository> entry : repositories.entrySet()) {
            Repository repository = entry.getValue();

            String path = entry.getKey();
            String type = repository.getClass().getSimpleName();

            if (repository.isWorking()) {
                if (verbose) {
                    log.info(String.format("Update %s repository in %s",
                        type, path));
                }

                try {
                    repository.update();
                } catch (UnsupportedOperationException e) {
                    log.warn(String.format("Skipping update of %s repository"
                        + " in %s: Not implemented", type, path));
                } catch (Exception e) {
                    log.warn( "An error occured while updating "
                        + path + " (" + type + ")", e);
                }
            } else {
                log.warn(String.format("Skipping update of %s repository in "
                    + "%s: Missing SCM dependencies?", type, path));
            }
        }
    }

    /**
     * Update the source the contents in the source repositories.
     * @param paths A list of files/directories to update
     */
    public void updateRepositories(Collection<String> paths) {
        boolean verbose = RuntimeEnvironment.getInstance().isVerbose();

        List<Repository> repos = getReposFromString(paths);

        for (Repository repository : repos) {
            String type = repository.getClass().getSimpleName();

            if (repository.isWorking()) {
                if (verbose) {
                    log.info(String.format("Update %s repository in %s", type,
                        repository.getDirectoryName()));
                }

                try {
                    repository.update();
                } catch (UnsupportedOperationException e) {
                    log.warn(String.format("Skipping update of %s repository"
                        + " in %s: Not implemented", type,
                        repository.getDirectoryName()));
                } catch (Exception e) {
                    log.warn( "An error occured while updating "
                        + repository.getDirectoryName() + " (" + type + ")", e);
                }
            } else {
                log.warn(String.format("Skipping update of %s repository in"
                    + " %s: Missing SCM dependencies?", type,
                    repository.getDirectoryName()));
            }
        }
    }

    private void createCache(Repository repository, String sinceRevision) {
        String path = repository.getDirectoryName();
        String type = repository.getClass().getSimpleName();

        if (repository.isWorking()) {
            boolean verbose = RuntimeEnvironment.getInstance().isVerbose();
            Statistics elapsed = new Statistics();

            if (verbose) {
                log.info("Creating historycache for {0} ({1})",
                    new Object[]{path, type});
            }

            try {
                repository.createCache(historyCache, sinceRevision);
            } catch (Exception e) {
                log.warn(
                    "An error occured while creating cache for " + path + " ("
                    + type + ")", e);
            }

            if (verbose) {
                elapsed.report(log, "Done historycache for " + path);
            }
        } else {
            log.warn( "Skipping creation of historycache of "
                + type + " repository in " + path + ": Missing SCM dependencies?");
        }
    }

    private void createCacheReal(Collection<Repository> repositories) {
        Statistics elapsed = new Statistics();
        ExecutorService executor = RuntimeEnvironment.getHistoryExecutor();
        // Since we know each repository object from the repositories
        // collection is unique, we can abuse HashMap to create a list of
        // repository,revision tuples with repository as key (as the revision
        // string does not have to be unique - surely it is not unique
        // for the initial index case).
        HashMap<Repository,String> repos2process = new HashMap<>();

        // Collect the list of <latestRev,repo> pairs first so that we
        // do not have to deal with latch decrementing in the cycle below.
        for (final Repository repo : repositories) {
            final String latestRev;

            try {
                latestRev = historyCache.getLatestCachedRevision(repo);
                repos2process.put(repo, latestRev);
            } catch (HistoryException he) {
                log.warn(
                        String.format(
                        "Failed to retrieve latest cached revision for %s",
                        repo.getDirectoryName()), he);
            }
        }

        log.info("Creating historycache for {0} repositories",
            repos2process.size());
        final CountDownLatch latch = new CountDownLatch(repos2process.size());
        for (final Map.Entry<Repository,String> entry : repos2process.entrySet()) {
            executor.submit(new Runnable() {
                @Override
                public void run() {
                    try {
                        createCache(entry.getKey(), entry.getValue());
                    } catch (Exception ex) {
                        // We want to catch any exception since we are in thread.
                        log.warn(
                            "createCacheReal() got exception" + ex);
                    } finally {
                        latch.countDown();
                    }
                }
            });
        }

        /*
         * Wait until the history of all repositories is done. This is necessary
         * since the next phase of generating index will need the history to
         * be ready as it is recorded in Lucene index.
         */
        try {
            latch.await();
        } catch (InterruptedException ex) {
            log.error(
                "latch exception" + ex);
        }

        executor.shutdown();
        while (!executor.isTerminated()) {
            try {
                // Wait forever
                executor.awaitTermination(999,TimeUnit.DAYS);
            } catch (InterruptedException exp) {
                log.warn(
                    "Received interrupt while waiting for executor to finish", exp);
            }
        }
        RuntimeEnvironment.freeHistoryExecutor();
        try {
            /* Thread pool for handling renamed files needs to be destroyed too. */
            RuntimeEnvironment.destroyRenamedHistoryExecutor();
        } catch (InterruptedException ex) {
            log.error(
                "destroying of renamed thread pool failed", ex);
        }

        // The cache has been populated. Now, optimize how it is stored on
        // disk to enhance performance and save space.
        try {
            historyCache.optimize();
        } catch (HistoryException he) {
            log.warn(
                    "Failed optimizing the history cache database", he);
        }
        elapsed.report(log, "Done historycache for all repositories");
        historyCache.setHistoryIndexDone();
    }

    public void createCache(Collection<String> repositories) {
        if (!useCache()) {
            return;
        }
        createCacheReal(getReposFromString(repositories));
    }

    public void removeCache(Collection<String> repositories) throws HistoryException {
        List<Repository> repos = getReposFromString(repositories);
        HistoryCache cache = historyCache;
        if (cache == null) {
            
          cache = new FileHistoryCache();
        }
        for (Repository r : repos) {
            try {
                cache.clear(r);
                log.info("History cache for " + r.getDirectoryName() + " cleared.");
            } catch (HistoryException e) {
                log.warn("Clearing history cache for repository " +
                    r.getDirectoryName() + " failed: " + e.getLocalizedMessage());
            }
        }
        invalidateRepositories(repos);
    }

    /**
     * Create the history cache for all of the repositories
     */
    public void createCache() {
        if (!useCache()) {
            return;
        }

        createCacheReal(repositories.values());
    }

    private List<Repository> getReposFromString(Collection<String> repositories) {
        ArrayList<Repository> repos = new ArrayList<Repository>();
        File root = RuntimeEnvironment.getInstance().getSourceRootFile();
        for (String file : repositories) {
            File f = new File(root, file);
            Repository r = getRepository(f);
            if (r == null) {
                log.warn( "Could not locate a repository for {0}",
                    f.getAbsolutePath());
            } else if (!repos.contains(r)){
                repos.add(r);
            }
        }
        return repos;
    }

    /**
     * Ensure that we have a directory in the cache. If it's not there, fetch
     * its history and populate the cache. If it's already there, and the
     * cache is able to tell how recent it is, attempt to update it to the
     * most recent revision.
     *
     * @param file the root path to test
     * @throws HistoryException if an error occurs while accessing the
     * history cache
     */
    public void ensureHistoryCacheExists(File file) throws HistoryException {
        if (!useCache()) {
            return;
        }

        Repository repository = getRepository(file);

        if (repository == null) {
            // no repository -> no history :(
            return;
        }

        String sinceRevision = null;

        if (historyCache.hasCacheForDirectory(file, repository)) {
            sinceRevision = historyCache.getLatestCachedRevision(repository);
            if (sinceRevision == null) {
                // Cache already exists, but we don't know how recent it is,
                // so don't do anything.
                return;
            }
        }

        // Create cache from the beginning if it doesn't exist, or update it
        // incrementally otherwise.
        createCache(getRepository(file), sinceRevision);
    }

    protected Repository getRepository(File path) {
        Map<String, Repository> repos = repositories;

        File file = path;
        try {
            file = path.getCanonicalFile();
        } catch (IOException e) {
            log.warn( "Failed to get canonical path for " + path, e);
            return null;
        }
        while (file != null) {
            Repository r = repos.get(file.getAbsolutePath());
            if (r != null) {
                return r;
            }
            file = file.getParentFile();
        }

        return null;
    }

    /**
     * Invalidate list of known repositories which match the list of directories.
     *
     * @param repos the new repositories
     * @param dirs only process repositories which match the directories
     */
    public void invalidateRepositories(Collection<? extends RepositoryInfo> repos, List<String> dirs) {
        if (repos != null && !repos.isEmpty() && dirs != null && !dirs.isEmpty()) {
            List<RepositoryInfo> newrepos = new ArrayList<> ();
            for (RepositoryInfo i : repos) {
                for (String dir : dirs) {
                    Path dirPath = new File(dir).toPath();
                    Path iPath = new File(i.getDirectoryName()).toPath();
                    if (iPath.startsWith(dirPath)) {
                        newrepos.add(i);
                    }
                }
            }
            repos = newrepos;
        }

        invalidateRepositories(repos);
    }

    /**
     * Invalidate list of known repositories.
     *
     * @param repos The new repositories
     */
    public void invalidateRepositories(Collection<? extends RepositoryInfo> repos) {
        if (repos == null || repos.isEmpty()) {
            repositories.clear();
        } else {
            Map<String, Repository> newrepos =
                new HashMap<String, Repository>(repos.size());
            Statistics elapsed = new Statistics();
            boolean verbose = RuntimeEnvironment.getInstance().isVerbose();
            if (verbose) {
                log.debug("invalidating repositories");
            }
            for (RepositoryInfo i : repos) {
                try {
                    Repository r = RepositoryFactory.getRepository(i);
                    if (r == null) {
                        log.warn(
                            "Failed to instanciate internal repository data for "
                            + i.getType() + " in " + i.getDirectoryName());
                    } else {
                        newrepos.put(r.getDirectoryName(), r);
                    }
                } catch (InstantiationException ex) {
                    log.warn( "Could not create " + i.getType()
                        + " for '" + i.getDirectoryName()
                        + "', could not instantiate the repository.", ex);
                } catch (IllegalAccessException iae) {
                    log.warn( "Could not create " + i.getType()
                        + " for '" + i.getDirectoryName()
                        + "', missing access rights.", iae);
                }
            }
            repositories = newrepos;
            if (verbose) {
                elapsed.report(log, "done invalidating repositories");
            }
        }
    }
}

