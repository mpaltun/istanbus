package org.istanbus.core.util;

import java.io.File;

public class FileUtils {

    public static void deleteDirectory(File file) {
        if (file.isDirectory())
        {
            File[] files = file.listFiles();
            for (File f : files) {
                deleteDirectory(f);
            }
        }
        file.delete();
    }
}
