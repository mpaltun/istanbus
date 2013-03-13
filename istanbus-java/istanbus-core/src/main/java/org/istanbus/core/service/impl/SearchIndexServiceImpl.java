package org.istanbus.core.service.impl;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;
import org.istanbus.core.dao.BusDAO;
import org.istanbus.core.lucene.CustomAnalyzer;
import org.istanbus.core.model.node.Bus;
import org.istanbus.core.model.node.Stop;
import org.istanbus.core.service.SearchIndexService;
import org.istanbus.core.util.FileUtils;
import org.neo4j.graphdb.GraphDatabaseService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

public class SearchIndexServiceImpl implements SearchIndexService {

    private static final Logger logger = LoggerFactory.getLogger(SearchIndexServiceImpl.class);

    private String indexRoot;
    private BusDAO busDAO;
    private GraphDatabaseService db;

    private final Locale DEFAULT_LOCALE = new Locale("tr");

    @Inject
    public SearchIndexServiceImpl(@Named("search.index.root.path") String indexRoot, BusDAO busDAO) {
        this.indexRoot = indexRoot;
        this.busDAO = busDAO;
    }

    public IndexWriter openWriter(String index) {
        String indexDir = indexRoot + index;
        File indexFolder = new File(indexDir);
        if (indexFolder.exists()) {

            FileUtils.deleteDirectory(indexFolder);
            logger.info("index folder already exists at {}, deleted", indexDir);

            indexFolder.mkdirs();
            logger.info("index folder created at {}", indexDir);
        }
        FSDirectory directory = null;
        try {
            directory = FSDirectory.open(indexFolder);
        } catch (IOException e) {
            logger.error("could not open directory", e);
            return null;
        }

        IndexWriterConfig writerConfig = new IndexWriterConfig(Version.LUCENE_36, new CustomAnalyzer(Version.LUCENE_36));
        IndexWriter indexWriter = null;
        try {
            indexWriter = new IndexWriter(directory, writerConfig);
        } catch (IOException e) {
            logger.error("could not initialize index writer", e);
            return null;
        }
        return indexWriter;
    }

    private boolean closeWriter(IndexWriter indexWriter) {
        try {
            indexWriter.close();
        } catch (IOException e) {
            logger.error("could not close index writer", e);
            return false;
        }
        indexWriter = null;
        return true;
    }

    @Override
    public void indexAll() {
        List<Bus> busList = busDAO.loadAllBuses();

        Set<String> indexedStops = new HashSet<String>();

        // NPE is ok
        IndexWriter stopIndexWriter = openWriter("stop");
        IndexWriter busIndexWriter = openWriter("bus");
        for (Bus bus : busList) {

            String busCode = bus.getId();
            String busName = bus.getName();

            String[] busTextFields = { busCode, busName };

            Document busDoc = getDocument(busCode, busName, busTextFields);
            addDocumentToIndexWriter(busIndexWriter, busDoc);

            List<Stop> stops = new ArrayList<Stop>();
            stops.addAll(bus.getStops().getGo());
            stops.addAll(bus.getStops().getTurn());

            for (Stop stop : stops) {
                // ensure not indexed already
                String stopCode = stop.getId();
                if (indexedStops.add(stopCode)) {
                    String stopName = stop.getName();
                    String[] stopTextFields = { stopName };

                    Document stopDoc = getDocument(stopCode, stopName, stopTextFields);
                    stopDoc.add(new Field("district", stop.getDistrict(), Field.Store.YES, Field.Index.NOT_ANALYZED));

                    addDocumentToIndexWriter(stopIndexWriter, stopDoc);
                }
            }
        }

        try {
            logger.info("{} bus, {} stops indexed", busIndexWriter.numDocs(), stopIndexWriter.numDocs());
        } catch (IOException e) {
            logger.error("error while getting doc count");
        }
        closeWriter(stopIndexWriter);
        closeWriter(busIndexWriter);

    }

    private Document getDocument(String id, String name, String... textFields) {
        Field idField = new Field("id", id, Field.Store.YES, Field.Index.NOT_ANALYZED);
        Field nameField = new Field("name", name, Field.Store.YES, Field.Index.NOT_ANALYZED);

        Document document = new Document();
        document.add(idField);
        document.add(nameField);

        String text = prepareSearchIndexText(textFields);
        Field textField = new Field("text", text, Field.Store.NO, Field.Index.ANALYZED);
        document.add(textField);

        return document;
    }

    private String prepareSearchIndexText(String... strings) {
        StringBuilder sb = new StringBuilder();

        for (String s : strings) {
            String string = s.toLowerCase(DEFAULT_LOCALE);
            sb.append(string).append(" ");
        }

        return sb.toString().trim();
    }

    private void addDocumentToIndexWriter(IndexWriter indexWriter, Document document) {
        try {
            indexWriter.addDocument(document);
        } catch (IOException e) {
            logger.error("error while adding document", e);
        }
    }

}
